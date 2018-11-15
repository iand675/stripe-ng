{-# LANGUAGE CPP #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Stripe.Utils
  ( module Stripe.Utils
  , module X
  , H.HashMap
  , Text
  , A.FromJSON(..)
  , A.Object
  , A.withText
  , encodeUtf8
  , decodeUtf8
  , (<>)
  ) where

import qualified Control.Exception as E
import Control.Lens (Lens', view)
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State.Lazy
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as C
import Data.Coerce
import Conduit (ConduitT, yieldMany)
import Data.Foldable
import Data.Hashable
import Data.Maybe (isJust, fromMaybe)
import Data.Monoid
import Data.Scientific (toBoundedInteger)
import Data.String (IsString(..))
import Data.Tagged
import Data.Text (Text, unpack)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX
import Data.Typeable as X
import Data.Foldable (toList)
import qualified Data.Vector as V
import GHC.Generics as X
import qualified Data.HashMap.Strict as H
import Network.HTTP.Conduit
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Header
import Network.HTTP.Types.Status
import System.Environment
import Web.FormUrlEncoded as X
import Web.HttpApiData as X
import Prelude hiding (id)

class HasId s a | s -> a where
  id :: Lens' s a

-- | Stripe utilizes cursor-based pagination via the starting_after and ending_before parameters. Both parameters take an existing object ID value (see below) and return objects in reverse chronological order. The ending_before parameter returns objects listed before the named object. The starting_after parameter returns objects listed after the named object. If both parameters are provided, only ending_before is used.
data Pagination a = Pagination
  { paginationLimit :: Maybe Int
  , paginationStartingAfter :: Maybe (Id a)
  , paginationEndingAfter :: Maybe (Id a)
  } deriving (Show, Eq, Generic, Typeable)

-- | No pagination parameters specified, returns the Stripe query with no pagination paramters set.
basePage :: Pagination a
basePage = Pagination Nothing Nothing Nothing

-- TODO may need to get fancier
enumerate :: (MonadStripe m, HasId a (Id a)) => (Pagination a -> m (List a)) -> ConduitT () a m ()
enumerate baseQuery = go basePage
  where
    go req = do
      result <- lift $ baseQuery req
      yieldMany $ listData_ result
      if listHasMore result
        then go $ basePage { paginationStartingAfter = Just $ view id $ V.last $ listData_ result }
        else return ()

paginationParams :: Pagination a -> [(ByteString, Maybe ByteString)]
paginationParams p = filter (isJust . snd)
  [ ("limit", (C.pack . show) <$> paginationLimit p)
  , ("starting_after", (encodeUtf8 . fromId) <$> paginationStartingAfter p)
  , ("ending_after", (encodeUtf8 . fromId) <$> paginationEndingAfter p)
  ]

joinParams :: ToForm a => a -> [(ByteString, Maybe ByteString)] -> [(ByteString, Maybe ByteString)]
joinParams f rest = customParams <> rest
  where
    (Form fVals) = toForm f
    fList = H.toList fVals
    customParams :: [(ByteString, Maybe ByteString)]
    customParams =
      concatMap
        (\(k, vs) ->
           let bk = encodeUtf8 k
            in map ((,) bk . Just . encodeUtf8) vs) $
      filter (not . null . snd) fList

data List a = List
  { listUrl :: Text
  , listHasMore :: Bool
  , listData_ :: V.Vector a
  , listTotalCount :: Maybe Int
  } deriving (Show, Eq, Generic, Typeable, Functor)

instance A.FromJSON a => A.FromJSON (List a) where
  parseJSON = parseObject "List" $ do
    assertObject "list"
    List
      <$> req "url"
      <*> req "has_more"
      <*> req "data"
      <*> opt "total_count"

newtype Id a = Id { fromId :: Text }
  deriving (Eq, Ord, Generic, Typeable, Hashable, A.ToJSON, A.FromJSON, ToHttpApiData, IsString)

instance Show (Id a) where
  show = show . fromId

instance Read (Id a) where
  readsPrec d r = coerce (readsPrec d r :: [(Text, String)])


newtype Timestamp = Timestamp { fromTimestamp :: UTCTime }
  deriving (Show, Eq, Generic, Typeable)

instance A.FromJSON Timestamp where
  parseJSON = A.withScientific "Timestamp" $ \n -> do
    case toBoundedInteger n of
      Nothing -> fail "Timestamp was not a bounded integer"
      Just ts -> pure $ Timestamp $ posixSecondsToUTCTime $ realToFrac (ts :: Word)

type Metadata = H.HashMap Text Text
type Expandable a = Id a
type CountryCode = Text
data Application

data Address = Address
  { addressCity :: Maybe Text
  , addressCountry :: CountryCode -- TODO make country code
  , addressLine1 :: Text
  , addressLine2 :: Maybe Text
  , addressPostalCode :: Text
  , addressState :: Maybe Text
  } deriving (Show, Eq, Generic, Typeable)

instance A.FromJSON Address where
  parseJSON = parseObject "Address" $ do
    Address
      <$> opt "city"
      <*> req "country"
      <*> req "line1"
      <*> opt "line2"
      <*> req "postal_code"
      <*> opt "state"

#ifdef STRICT_JSON_CHECK
type Parser = StateT A.Object A.Parser

parseObject :: String -> Parser a -> A.Value -> A.Parser a
parseObject n m = A.withObject n (\o -> runParser o m)

runParser :: A.Object -> Parser a -> A.Parser a
runParser o m = do
  (r, o') <- runStateT m o
  if H.null o'
    then pure r
    else fail ("Unused keys: " <> show o')

req :: A.FromJSON a => Text -> Parser a
req t = do
  o <- get
  r <- lift (o A..: t)
  modify (H.delete t)
  pure r

opt :: A.FromJSON a => Text -> Parser (Maybe a)
opt t = do
  o <- get
  r <- lift (o A..:? t)
  modify (H.delete t)
  pure r

#else

type Parser = ReaderT A.Object A.Parser

parseObject :: String -> Parser a -> A.Value -> A.Parser a
parseObject n m = A.withObject n (\o -> runParser o m)

runParser :: A.Object -> Parser a -> A.Parser a
runParser o m = runReaderT m o

req :: A.FromJSON a => Text -> Parser a
req t = do
  o <- ask
  lift (o A..: t)

opt :: A.FromJSON a => Text -> Parser (Maybe a)
opt t = do
  o <- ask
  lift (o A..:? t)

#endif

assertObject :: Text -> Parser ()
assertObject t = do
  str <- req "object"
  when (str /= t) $ fail ("object is of type " ++ show str ++ ", expected object of type " ++ show t)

class StripeResult t a where
  stripeResult :: (MonadStripe m) => Tagged t (Response L.ByteString) -> m a

instance (A.FromJSON t) => StripeResult t t where
  stripeResult (Tagged r) =
    case A.eitherDecode $ responseBody r of
      Left err -> fail err
      Right ok -> pure ok

instance (A.FromJSON t) => StripeResult t (Response t) where
  stripeResult (Tagged r) =
    case A.eitherDecode $ responseBody r of
      Left err -> fail err
      Right ok -> pure (ok <$ r)

instance StripeResult t () where
  stripeResult = const $ pure ()

instance StripeResult t (Response L.ByteString) where
  stripeResult = pure . unTagged

instance (StripeResult t r) => StripeResult t (A.Value, r) where
  stripeResult r'@(Tagged r) =
    case A.eitherDecode $ responseBody r of
      Left err -> fail err
      Right ok -> (,) <$> pure ok <*> stripeResult r'

buildRequest :: StripeState -> ByteString -> [(ByteString, Maybe ByteString)] -> Request
buildRequest StripeState{..} b qps =
  setQueryString qps $
  applyBasicAuth (fromStripeKey stripeStateKey) "" $
  stripeStateBaseRequest
    { requestHeaders = ("Stripe-Version", fromStripeVersion stripeStateVersion) : requestHeaders stripeStateBaseRequest
    , path = path stripeStateBaseRequest <> b
    }

extractResult :: (MonadStripe m, StripeResult t a) => proxy t -> Response L.ByteString -> m a
extractResult p r = if responseStatus r >= status400
  then case A.eitherDecode $ responseBody r of
    Left err -> E.throw $ StripeJsonError err
    Right ok -> throwStripeError (ok { stripeErrorContext = ErrorContext (responseStatus r) (responseHeaders r) (responseBody r)})
  else stripeResult $ tagWith p r

standardStripeGet :: (MonadStripe m, MonadIO m, StripeResult t a) => proxy t -> ByteString -> [(ByteString, Maybe ByteString)] -> m a
standardStripeGet p b qps = do
  st <- askStripeState
  r <- liftIO $ httpLbs (buildRequest st b qps) (stripeStateManager st)
  extractResult p r

standardStripePost :: (MonadStripe m, MonadIO m, ToForm a, StripeResult t b) => proxy t -> ByteString -> a -> m b
standardStripePost p b f = do
  st <- askStripeState
  r <-
    liftIO $
    httpLbs
      ((buildRequest st b []) { method = "POST", requestBody = RequestBodyLBS $ urlEncodeAsForm f })
      (stripeStateManager st)
  extractResult p r

standardStripeDelete :: (MonadStripe m, MonadIO m, StripeResult t a) => proxy t -> ByteString -> [(ByteString, Maybe ByteString)] -> m a
standardStripeDelete p b qps = do
  st <- askStripeState
  r <-
    liftIO $
    httpLbs
      ((buildRequest st b qps) { method = "DELETE" })
      (stripeStateManager st)
  extractResult p r

class AsStripeError e where
  fromStripeError :: StripeError -> e
  toStripeError :: e -> Maybe StripeError

instance AsStripeError StripeError where
  fromStripeError e = e
  toStripeError = pure

class Monad m => MonadStripe (m :: * -> *) where
  askStripeState :: m StripeState

  throwStripeError :: StripeError -> m a
  default throwStripeError :: (MonadError e m, AsStripeError e) => StripeError -> m a
  throwStripeError = throwError . fromStripeError

  catchStripeError :: m a -> (StripeError -> m a) -> m a
  default catchStripeError :: (MonadError e m, AsStripeError e) => m a -> (StripeError -> m a) -> m a
  catchStripeError m f = catchError m (\e -> maybe (throwError e) f $ toStripeError e)

  stripeGet :: StripeResult t a => proxy t -> ByteString -> [(ByteString, Maybe ByteString)] -> m a
  default stripeGet :: (MonadIO m, StripeResult t a) => proxy t -> ByteString -> [(ByteString, Maybe ByteString)] -> m a
  stripeGet = standardStripeGet

  stripePost :: (ToForm a, StripeResult t b) => proxy t -> ByteString -> a -> m b
  default stripePost :: (MonadIO m, ToForm a, StripeResult t b) => proxy t -> ByteString -> a -> m b
  stripePost = standardStripePost

  stripeDelete :: StripeResult t a => proxy t -> ByteString -> [(ByteString, Maybe ByteString)] -> m a
  default stripeDelete :: (MonadIO m, StripeResult t a) => proxy t -> ByteString -> [(ByteString, Maybe ByteString)] -> m a
  stripeDelete = standardStripeDelete

notFoundToMaybe :: MonadStripe m => m a -> m (Maybe a)
notFoundToMaybe m = catchStripeError (Just <$> m) $ \se -> if errorContextStatus (stripeErrorContext se) == notFound404
  then pure Nothing
  else throwStripeError se

data StripeState = StripeState
  { stripeStateBaseRequest :: Request -- A.Object
  , stripeStateManager :: Manager
  , stripeStateVersion :: StripeVersion
  , stripeStateKey :: StripeKey
  }

data StripeErrorType
  = ApiConnectionError
  | ApiError
  | AuthenticationError
  | CardError
  | IdempotencyError
  | InvalidRequestError
  | RateLimitError
  | ValidationError
  deriving (Show, Eq, Generic, Typeable)

instance A.FromJSON StripeErrorType where
  parseJSON = A.withText "StripeErrorType" $ \e -> case e of
    "api_connection_error" -> pure ApiConnectionError
    "api_error" -> pure ApiError
    "authentication_error" -> pure AuthenticationError
    "card_error" -> pure CardError
    "idempotency_error" -> pure IdempotencyError
    "invalid_request_error" -> pure InvalidRequestError
    "rate_limit_error" -> pure RateLimitError
    "validation_error" -> pure ValidationError
    _ -> fail (show e <> " is not a recognized error type.")

data DeclineCode
  = ApproveWithId
  | CallIssuer
  | CardNotSupported
  | CardVelocityExceeded
  | CurrencyNotSupported
  | DoNotHonor
  | DoNotTryAgain
  | DuplicateTransaction
  | ExpiredCardDecline
  | Fraudulent
  | GenericDecline
  | IncorrectCvcDecline
  | IncorrectPinDecline
  | IncorrectZipDecline
  | InsufficientFunds
  | InvalidAccount
  | InvalidAmount
  | InvalidCvcDecline
  | InvalidExpiryYearDecline
  | InvalidNumberDecline
  | InvalidPin
  | IssuerNotAvailable
  | LostCard
  | MerchantBlacklist
  | NewAccountInformationAvailable
  | NoActionTaken
  | NotPermitted
  | PickupCard
  | PinTryExceeded
  | ProcessingErrorDecline
  | ReenterTransaction
  | RestrictedCard
  | RevocationOfAllAuthorizations
  | RevocationOfAuthorization
  | SecurityViolation
  | ServiceNotAllowed
  | StolenCard
  | StopPaymentOrder
  | TestmodeDecline
  | TransactionNotALlowed
  | TryAgainLater
  | WithdrawalCountLimitExceeded
  | OtherDecline Text
  deriving (Show, Eq, Generic, Typeable)

data ErrorCode
  = AccountAlreadyExists
  | AccountCountryInvalidAddress
  | AccountInvalid
  | AccountNumberInvalid
  | AlipayUpgradeRequired
  | AmountTooLarge
  | ApiKeyExpired
  | BalanceInsufficient
  | BankAccountExists
  | BankAccountUnusable
  | BankAccountUnverified
  | BitcoinUpgradeRequired
  | CardDeclined
  | ChargeAlreadyCaptured
  | ChargeAlreadyRefunded
  | ChargeDisputed
  | ChargeExceedsSourceLimit
  | ChargeExpiredForCapture
  | CountryUnsupported
  | CouponExpired
  | CustomerMaxSubscriptions
  | EmailInvalid
  | ExpiredCard
  | IdempotencyKeyInUse
  | IncorrectAddress
  | IncorrectCvc
  | IncorrectNumber
  | IncorrectZip
  | InstantPayoutsUnsupported
  | InvalidCardType
  | InvalidChargeAmount
  | InvalidCvc
  | InvalidExpiryMonth
  | InvalidExpiryYear
  | InvalidNumber
  | InvalidSourceUsage
  | InvoiceNoCustomerLineItems
  | InvoiceNoSubscriptionLineItems
  | InvoiceNotEditable
  | InvoiceUpcomingNone
  | LivemodeMismatch
  | Missing
  | NotAllowedOnStandardAccount
  | OrderCreationFailed
  | OrderRequiredSettings
  | OrderStatusInvalid
  | OrderUpstreamTimeout
  | OutOfInventory
  | ParameterInvalidEmpty
  | ParameterInvalidInteger
  | ParameterInvalidStringBlank
  | ParameterInvalidStringEmpty
  | ParameterMissing
  | ParameterUnknown
  | PaymentIntentAuthenticationFailure
  | PaymentIntentUnexpectedState
  | PaymentMethodUnactivated
  | PayoutsNotAllowed
  | PlatformApiKeyExpired
  | PostalCodeInvalid
  | ProcessingError
  | ProductInactive
  | RateLimit
  | ResourceAlreadyExists
  | ResourceMissing
  | RoutingNumberInvalid
  | SecretKeyRequired
  | SepaUnsupportedAccount
  | ShippingCalculationFailed
  | SkuInactive
  | StateUnsupported
  | TaxIdInvalid
  | TaxesCalculationFailed
  | TestmodeChargesOnly
  | TlsVersionUnsupported
  | TokenAlreadyUsed
  | TokenInUse
  | TransfersNotAllowed
  | UpstreamOrderCreationFailed
  | UrlInvalid
  | UnknownErrorCode Text
  deriving (Show, Eq, Generic, Typeable)

instance A.FromJSON ErrorCode where
  parseJSON = A.withText "ErrorCode" (pure . lookupErrorCode)

lookupErrorCode :: Text -> ErrorCode
lookupErrorCode t = fromMaybe (UnknownErrorCode t) $ H.lookup t errorCodeMap

errorCodeMap :: H.HashMap Text ErrorCode
errorCodeMap = H.fromList
  [ ("account_already_exists", AccountAlreadyExists)
  , ("account_country_invalid_address", AccountCountryInvalidAddress)
  , ("account_invalid", AccountInvalid)
  , ("account_number_invalid", AccountNumberInvalid)
  , ("alipay_upgrade_required", AlipayUpgradeRequired)
  , ("amount_too_large", AmountTooLarge)
  , ("api_key_expired", ApiKeyExpired)
  , ("balance_insufficient", BalanceInsufficient)
  , ("bank_account_exists", BankAccountExists)
  , ("bank_account_unusable", BankAccountUnusable)
  , ("bank_account_unverified", BankAccountUnverified)
  , ("bitcoin_upgrade_required", BitcoinUpgradeRequired)
  , ("card_declined", CardDeclined)
  , ("charge_already_captured", ChargeAlreadyCaptured)
  , ("charge_already_refunded", ChargeAlreadyRefunded)
  , ("charge_disputed", ChargeDisputed)
  , ("charge_exceeds_source_limit", ChargeExceedsSourceLimit)
  , ("charge_expired_for_capture", ChargeExpiredForCapture)
  , ("country_unsupported", CountryUnsupported)
  , ("coupon_expired", CouponExpired)
  , ("customer_max_subscriptions", CustomerMaxSubscriptions)
  , ("email_invalid", EmailInvalid)
  , ("expired_card", ExpiredCard)
  , ("idempotency_key_in_use", IdempotencyKeyInUse)
  , ("incorrect_address", IncorrectAddress)
  , ("incorrect_cvc", IncorrectCvc)
  , ("incorrect_number", IncorrectNumber)
  , ("incorrect_zip", IncorrectZip)
  , ("instant_payouts_unsupported", InstantPayoutsUnsupported)
  , ("invalid_card_type", InvalidCardType)
  , ("invalid_charge_amount", InvalidChargeAmount)
  , ("invalid_cvc", InvalidCvc)
  , ("invalid_expiry_month", InvalidExpiryMonth)
  , ("invalid_expiry_year", InvalidExpiryYear)
  , ("invalid_number", InvalidNumber)
  , ("invalid_source_usage", InvalidSourceUsage)
  , ("invoice_no_customer_line_items", InvoiceNoCustomerLineItems)
  , ("invoice_no_subscription_line_items", InvoiceNoSubscriptionLineItems)
  , ("invoice_not_editable", InvoiceNotEditable)
  , ("invoice_upcoming_none", InvoiceUpcomingNone)
  , ("livemode_mismatch", LivemodeMismatch)
  , ("missing", Missing)
  , ("not_allowed_on_standard_account", NotAllowedOnStandardAccount)
  , ("order_creation_failed", OrderCreationFailed)
  , ("order_required_settings", OrderRequiredSettings)
  , ("order_status_invalid", OrderStatusInvalid)
  , ("order_upstream_timeout", OrderUpstreamTimeout)
  , ("out_of_inventory", OutOfInventory)
  , ("parameter_invalid_empty", ParameterInvalidEmpty)
  , ("parameter_invalid_integer", ParameterInvalidInteger)
  , ("parameter_invalid_string_blank", ParameterInvalidStringBlank)
  , ("parameter_invalid_string_empty", ParameterInvalidStringEmpty)
  , ("parameter_missing", ParameterMissing)
  , ("parameter_unknown", ParameterUnknown)
  , ("payment_intent_authentication_failure", PaymentIntentAuthenticationFailure)
  , ("payment_intent_unexpected_state", PaymentIntentUnexpectedState)
  , ("payment_method_unactivated", PaymentMethodUnactivated)
  , ("payouts_not_allowed", PayoutsNotAllowed)
  , ("platform_api_key_expired", PlatformApiKeyExpired)
  , ("postal_code_invalid", PostalCodeInvalid)
  , ("processing_error", ProcessingError)
  , ("product_inactive", ProductInactive)
  , ("rate_limit", RateLimit)
  , ("resource_already_exists", ResourceAlreadyExists)
  , ("resource_missing", ResourceMissing)
  , ("routing_number_invalid", RoutingNumberInvalid)
  , ("secret_key_required", SecretKeyRequired)
  , ("sepa_unsupported_account", SepaUnsupportedAccount)
  , ("shipping_calculation_failed", ShippingCalculationFailed)
  , ("sku_inactive", SkuInactive)
  , ("state_unsupported", StateUnsupported)
  , ("tax_id_invalid", TaxIdInvalid)
  , ("taxes_calculation_failed", TaxesCalculationFailed)
  , ("testmode_charges_only", TestmodeChargesOnly)
  , ("tls_version_unsupported", TlsVersionUnsupported)
  , ("token_already_used", TokenAlreadyUsed)
  , ("token_in_use", TokenInUse)
  , ("transfers_not_allowed", TransfersNotAllowed)
  , ("upstream_order_creation_failed", UpstreamOrderCreationFailed)
  , ("url_invalid", UrlInvalid)
  ]

data ErrorContext = ErrorContext
  { errorContextStatus :: Status
  , errorContextResponseHeaders :: ResponseHeaders
  , errorContextFullBody :: L.ByteString
  } deriving (Show, Eq, Generic, Typeable)

data StripeError = StripeError
  { stripeErrorType :: StripeErrorType
  -- TODO
  -- , stripeErrorCharge :: Maybe (Id Charge) -- ^ For card errors, the ID of the failed charge.
  , stripeErrorCode :: Maybe ErrorCode -- ^ For some errors that could be handled programmatically, a short string indicating the error code reported.
  , stripeErrorDeclineCode :: Maybe Text -- ^ For card errors resulting from a card issuer decline, a short string indicating the card issuer’s reason for the decline if they provide one.
  , stripeErrorDocUrl :: Maybe Text -- ^ A URL to more information about the error code reported.
  , stripeErrorMessage :: Maybe Text -- ^ A human-readable message providing more details about the error. For card errors, these messages can be shown to your users.
  , stripeErrorParam :: Maybe Text -- ^ If the error is parameter-specific, the parameter related to the error. For example, you can use this to display a message near the correct form field.
  , stripeErrorSource :: Maybe A.Object -- ^ The source object for errors returned on a request involving a source.
  , stripeErrorContext :: ErrorContext
  } deriving (Show, Eq, Generic, Typeable)

instance Exception StripeError

instance A.FromJSON StripeError where
  parseJSON = A.withObject "StripeError" $ \o -> do
    e <- o A..: "error"
    StripeError
      <$> e A..: "type"
      -- <*> e .: "charge"
      <*> e A..:? "code"
      <*> e A..:? "decline_code"
      <*> e A..:? "doc_url"
      <*> e A..:? "message"
      <*> e A..:? "param"
      <*> e A..:? "source"
      <*> pure (ErrorContext (Status 0 "Unknown") [] "")

data StripeJsonError = StripeJsonError String
  deriving (Show, Eq, Generic, Typeable)

instance Exception StripeJsonError

newtype StripeVersion = StripeVersion { fromStripeVersion :: ByteString }
  deriving (Show, Eq, Ord)

currentStripeVersion :: StripeVersion
currentStripeVersion = StripeVersion "2018-10-31"

newtype StripeKey = StripeKey { fromStripeKey :: ByteString }
  deriving (Show, Eq, Ord)

mkStripeState :: MonadIO m => StripeKey -> m StripeState
mkStripeState stripeStateKey = liftIO $ do
  stripeStateManager <- getGlobalManager
  stripeStateBaseRequest <- parseRequest "https://api.stripe.com/v1/"
  let stripeStateVersion = currentStripeVersion
  pure $ StripeState {..}

instance (MonadIO m, MonadThrow m, MonadCatch m) => MonadStripe (ReaderT StripeState m) where
  askStripeState = ask
  throwStripeError = throwM
  catchStripeError = catch

instance (MonadStripe m) => MonadStripe (ExceptT StripeError m) where
  askStripeState = lift askStripeState
  stripeGet p b qps =
    ExceptT $ catchStripeError (Right <$> stripeGet p b qps) (pure . Left)
  stripePost p b d =
    ExceptT $ catchStripeError (Right <$> stripePost p b d) (pure . Left)
  stripeDelete p b qps =
    ExceptT $ catchStripeError (Right <$> stripeDelete p b qps) (pure . Left)
  throwStripeError = throwError
  catchStripeError = catchError

runSimpleStripe :: (MonadIO m) => StripeState -> ReaderT StripeState m a -> m a
runSimpleStripe st m = runReaderT m st

-- | Get API key from STRIPE_SECRET_KEY
-- stripeWithEnv :: (MonadIO m) => ReaderT StripeState (ExceptT StripeError m) a -> m (Either StripeError a)
stripeWithEnv m = do
  k <- liftIO $ getEnv "STRIPE_SECRET_KEY"
  st <- mkStripeState $ StripeKey $ C.pack k
  runSimpleStripe st m

arrayParams :: (ToHttpApiData a) => Text -> [a] -> Form
arrayParams t vs = Form $ H.singleton (t <> "[]") $ map toQueryParam vs

indexedArrayFormParams :: forall f a. (Foldable f, ToForm a) => Text -> f a -> Form
indexedArrayFormParams baseName vs = Form $ H.fromList $ concat $ zipWith reform [0..] (toList vs)
  where
    reform :: Int -> a -> [(Text, [Text])]
    reform ix formable =
      let (Form hm) = toForm formable
          ls = H.toList hm
       in map (\(k, v) -> (T.concat [baseName, "[", T.pack $ show ix , "][", k, "]"], v)) ls

dictParams :: (ToForm a) => Text -> a -> Form
dictParams t f = Form . H.fromList . map transform . H.toList $ formed
  where
    (Form formed) = toForm f
    transform (k, v) = (t <> "[" <> k <> "]", v)

hashParams :: (ToHttpApiData v) => Text -> H.HashMap Text v -> Form
hashParams t = Form . H.fromList . map transform . H.toList
  where
    transform (k, v) = (t <> "[" <> k <> "]", [toQueryParam v])

reqParam :: (ToHttpApiData v) => Text -> v -> Form
reqParam k v = Form $ H.singleton k [toQueryParam v]

optParam :: (ToHttpApiData v) => Text -> Maybe v -> Form
optParam k v = Form $ H.singleton k (toList $ fmap toQueryParam v)

data CurrencyCode
  = USD
  | AED
  | AFN
  | ALL
  | AMD
  | ANG
  | AOA
  | ARS
  | AUD
  | AWG
  | AZN
  | BAM
  | BBD
  | BDT
  | BGN
  | BIF
  | BMD
  | BND
  | BOB
  | BRL
  | BSD
  | BWP
  | BZD
  | CAD
  | CDF
  | CHF
  | CLP
  | CNY
  | COP
  | CRC
  | CVE
  | CZK
  | DJF
  | DKK
  | DOP
  | DZD
  | EGP
  | ETB
  | EUR
  | FJD
  | FKP
  | GBP
  | GEL
  | GIP
  | GMD
  | GNF
  | GTQ
  | GYD
  | HKD
  | HNL
  | HRK
  | HTG
  | HUF
  | IDR
  | ILS
  | INR
  | ISK
  | JMD
  | JPY
  | KES
  | KGS
  | KHR
  | KMF
  | KRW
  | KYD
  | KZT
  | LAK
  | LBP
  | LKR
  | LRD
  | LSL
  | MAD
  | MDL
  | MGA
  | MKD
  | MMK
  | MNT
  | MOP
  | MRO
  | MUR
  | MVR
  | MWK
  | MXN
  | MYR
  | MZN
  | NAD
  | NGN
  | NIO
  | NOK
  | NPR
  | NZD
  | PAB
  | PEN
  | PGK
  | PHP
  | PKR
  | PLN
  | PYG
  | QAR
  | RON
  | RSD
  | RUB
  | RWF
  | SAR
  | SBD
  | SCR
  | SEK
  | SGD
  | SHP
  | SLL
  | SOS
  | SRD
  | STD
  | SVC
  | SZL
  | THB
  | TJS
  | TOP
  | TRY
  | TTD
  | TWD
  | TZS
  | UAH
  | UGX
  | UYU
  | UZS
  | VND
  | VUV
  | WST
  | XAF
  | XCD
  | XOF
  | XPF
  | YER
  | ZAR
  | ZMW
  deriving (Show, Eq, Generic, Typeable)

instance A.FromJSON CurrencyCode where
  parseJSON = A.withText "CurrencyCode" $ \cc -> case cc of
    "usd" -> pure USD
    "aed" -> pure AED
    "afn" -> pure AFN
    "all" -> pure ALL
    "amd" -> pure AMD
    "ang" -> pure ANG
    "aoa" -> pure AOA
    "ars" -> pure ARS
    "aud" -> pure AUD
    "awg" -> pure AWG
    "azn" -> pure AZN
    "bam" -> pure BAM
    "bbd" -> pure BBD
    "bdt" -> pure BDT
    "bgn" -> pure BGN
    "bif" -> pure BIF
    "bmd" -> pure BMD
    "bnd" -> pure BND
    "bob" -> pure BOB
    "brl" -> pure BRL
    "bsd" -> pure BSD
    "bwp" -> pure BWP
    "bzd" -> pure BZD
    "cad" -> pure CAD
    "cdf" -> pure CDF
    "chf" -> pure CHF
    "clp" -> pure CLP
    "cny" -> pure CNY
    "cop" -> pure COP
    "crc" -> pure CRC
    "cve" -> pure CVE
    "czk" -> pure CZK
    "djf" -> pure DJF
    "dkk" -> pure DKK
    "dop" -> pure DOP
    "dzd" -> pure DZD
    "egp" -> pure EGP
    "etb" -> pure ETB
    "eur" -> pure EUR
    "fjd" -> pure FJD
    "fkp" -> pure FKP
    "gbp" -> pure GBP
    "gel" -> pure GEL
    "gip" -> pure GIP
    "gmd" -> pure GMD
    "gnf" -> pure GNF
    "gtq" -> pure GTQ
    "gyd" -> pure GYD
    "hkd" -> pure HKD
    "hnl" -> pure HNL
    "hrk" -> pure HRK
    "htg" -> pure HTG
    "huf" -> pure HUF
    "idr" -> pure IDR
    "ils" -> pure ILS
    "inr" -> pure INR
    "isk" -> pure ISK
    "jmd" -> pure JMD
    "jpy" -> pure JPY
    "kes" -> pure KES
    "kgs" -> pure KGS
    "khr" -> pure KHR
    "kmf" -> pure KMF
    "krw" -> pure KRW
    "kyd" -> pure KYD
    "kzt" -> pure KZT
    "lak" -> pure LAK
    "lbp" -> pure LBP
    "lkr" -> pure LKR
    "lrd" -> pure LRD
    "lsl" -> pure LSL
    "mad" -> pure MAD
    "mdl" -> pure MDL
    "mga" -> pure MGA
    "mkd" -> pure MKD
    "mmk" -> pure MMK
    "mnt" -> pure MNT
    "mop" -> pure MOP
    "mro" -> pure MRO
    "mur" -> pure MUR
    "mvr" -> pure MVR
    "mwk" -> pure MWK
    "mxn" -> pure MXN
    "myr" -> pure MYR
    "mzn" -> pure MZN
    "nad" -> pure NAD
    "ngn" -> pure NGN
    "nio" -> pure NIO
    "nok" -> pure NOK
    "npr" -> pure NPR
    "nzd" -> pure NZD
    "pab" -> pure PAB
    "pen" -> pure PEN
    "pgk" -> pure PGK
    "php" -> pure PHP
    "pkr" -> pure PKR
    "pln" -> pure PLN
    "pyg" -> pure PYG
    "qar" -> pure QAR
    "ron" -> pure RON
    "rsd" -> pure RSD
    "rub" -> pure RUB
    "rwf" -> pure RWF
    "sar" -> pure SAR
    "sbd" -> pure SBD
    "scr" -> pure SCR
    "sek" -> pure SEK
    "sgd" -> pure SGD
    "shp" -> pure SHP
    "sll" -> pure SLL
    "sos" -> pure SOS
    "srd" -> pure SRD
    "std" -> pure STD
    "svc" -> pure SVC
    "szl" -> pure SZL
    "thb" -> pure THB
    "tjs" -> pure TJS
    "top" -> pure TOP
    "try" -> pure TRY
    "ttd" -> pure TTD
    "twd" -> pure TWD
    "tzs" -> pure TZS
    "uah" -> pure UAH
    "ugx" -> pure UGX
    "uyu" -> pure UYU
    "uzs" -> pure UZS
    "vnd" -> pure VND
    "vuv" -> pure VUV
    "wst" -> pure WST
    "xaf" -> pure XAF
    "xcd" -> pure XCD
    "xof" -> pure XOF
    "xpf" -> pure XPF
    "yer" -> pure YER
    "zar" -> pure ZAR
    "zmw" -> pure ZMW
    _ -> fail "Unknown currency code. Please submit an issue or pull request to the stripe-ng project."

instance ToHttpApiData CurrencyCode where
  toQueryParam c = case c of
    USD -> "usd"
    AED -> "aed"
    AFN -> "afn"
    ALL -> "all"
    AMD -> "amd"
    ANG -> "ang"
    AOA -> "aoa"
    ARS -> "ars"
    AUD -> "aud"
    AWG -> "awg"
    AZN -> "azn"
    BAM -> "bam"
    BBD -> "bbd"
    BDT -> "bdt"
    BGN -> "bgn"
    BIF -> "bif"
    BMD -> "bmd"
    BND -> "bnd"
    BOB -> "bob"
    BRL -> "brl"
    BSD -> "bsd"
    BWP -> "bwp"
    BZD -> "bzd"
    CAD -> "cad"
    CDF -> "cdf"
    CHF -> "chf"
    CLP -> "clp"
    CNY -> "cny"
    COP -> "cop"
    CRC -> "crc"
    CVE -> "cve"
    CZK -> "czk"
    DJF -> "djf"
    DKK -> "dkk"
    DOP -> "dop"
    DZD -> "dzd"
    EGP -> "egp"
    ETB -> "etb"
    EUR -> "eur"
    FJD -> "fjd"
    FKP -> "fkp"
    GBP -> "gbp"
    GEL -> "gel"
    GIP -> "gip"
    GMD -> "gmd"
    GNF -> "gnf"
    GTQ -> "gtq"
    GYD -> "gyd"
    HKD -> "hkd"
    HNL -> "hnl"
    HRK -> "hrk"
    HTG -> "htg"
    HUF -> "huf"
    IDR -> "idr"
    ILS -> "ils"
    INR -> "inr"
    ISK -> "isk"
    JMD -> "jmd"
    JPY -> "jpy"
    KES -> "kes"
    KGS -> "kgs"
    KHR -> "khr"
    KMF -> "kmf"
    KRW -> "krw"
    KYD -> "kyd"
    KZT -> "kzt"
    LAK -> "lak"
    LBP -> "lbp"
    LKR -> "lkr"
    LRD -> "lrd"
    LSL -> "lsl"
    MAD -> "mad"
    MDL -> "mdl"
    MGA -> "mga"
    MKD -> "mkd"
    MMK -> "mmk"
    MNT -> "mnt"
    MOP -> "mop"
    MRO -> "mro"
    MUR -> "mur"
    MVR -> "mvr"
    MWK -> "mwk"
    MXN -> "mxn"
    MYR -> "myr"
    MZN -> "mzn"
    NAD -> "nad"
    NGN -> "ngn"
    NIO -> "nio"
    NOK -> "nok"
    NPR -> "npr"
    NZD -> "nzd"
    PAB -> "pab"
    PEN -> "pen"
    PGK -> "pgk"
    PHP -> "php"
    PKR -> "pkr"
    PLN -> "pln"
    PYG -> "pyg"
    QAR -> "qar"
    RON -> "ron"
    RSD -> "rsd"
    RUB -> "rub"
    RWF -> "rwf"
    SAR -> "sar"
    SBD -> "sbd"
    SCR -> "scr"
    SEK -> "sek"
    SGD -> "sgd"
    SHP -> "shp"
    SLL -> "sll"
    SOS -> "sos"
    SRD -> "srd"
    STD -> "std"
    SVC -> "svc"
    SZL -> "szl"
    THB -> "thb"
    TJS -> "tjs"
    TOP -> "top"
    TRY -> "try"
    TTD -> "ttd"
    TWD -> "twd"
    TZS -> "tzs"
    UAH -> "uah"
    UGX -> "ugx"
    UYU -> "uyu"
    UZS -> "uzs"
    VND -> "vnd"
    VUV -> "vuv"
    WST -> "wst"
    XAF -> "xaf"
    XCD -> "xcd"
    XOF -> "xof"
    XPF -> "xpf"
    YER -> "yer"
    ZAR -> "zar"
    ZMW -> "zmw"

class BaseQuery a where
  baseQuery :: a
