{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
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

import Control.Lens (Lens', view)
import Control.Monad.Reader
import Control.Monad.State.Lazy
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as C
import Conduit (ConduitT, yieldMany)
import Data.Hashable
import Data.Maybe (isJust)
import Data.Monoid
import Data.Tagged
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Typeable as X
import Data.Foldable (toList)
import qualified Data.Vector as V
import GHC.Generics as X
import qualified Data.HashMap.Strict as H
import Network.HTTP.Conduit
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Header
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
enumerate :: (StripeMonad m, HasId a (Id a)) => (Pagination a -> m (List a)) -> ConduitT () a m ()
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
  } deriving (Show, Eq, Generic, Typeable)

instance A.FromJSON a => A.FromJSON (List a) where
  parseJSON = parseObject "List" $ do
    assertObject "list"
    List
      <$> req "url"
      <*> req "has_more"
      <*> req "data"
      <*> opt "total_count"

newtype Id a = Id { fromId :: Text }
  deriving (Eq, Generic, Typeable, Hashable, A.ToJSON, A.FromJSON, ToHttpApiData)

instance Show (Id a) where
  show = show . fromId

newtype Timestamp = Timestamp Integer
  deriving (Show, Eq, Generic, Typeable)

instance A.FromJSON Timestamp where
  parseJSON = fmap Timestamp . A.parseJSON

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
  stripeResult :: (Monad m) => Tagged t (Response L.ByteString) -> m a

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

instance (StripeResult t r) => StripeResult t (A.Value, r) where
  stripeResult r'@(Tagged r) =
    case A.eitherDecode $ responseBody r of
      Left err -> fail err
      Right ok -> (,) <$> pure ok <*> stripeResult r'

class Monad m => StripeMonad (m :: * -> *) where
  jsonGet :: StripeResult t a => proxy t -> ByteString -> [(ByteString, Maybe ByteString)] -> m a
  formPost :: (ToForm a, StripeResult t b) => proxy t -> ByteString -> a -> m b

  -- jsonPost :: _

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

data StripeError = StripeError
  { stripeErrorType :: StripeErrorType
  -- TODO
  -- , stripeErrorCharge :: Maybe (Id Charge) -- ^ For card errors, the ID of the failed charge.
  , stripeErrorCode :: Maybe Text -- ^ For some errors that could be handled programmatically, a short string indicating the error code reported.
  , stripeErrorDeclineCode :: Maybe Text -- ^ For card errors resulting from a card issuer decline, a short string indicating the card issuer’s reason for the decline if they provide one.
  , stripeErrorDocUrl :: Maybe Text -- ^ A URL to more information about the error code reported.
  , stripeErrorMessage :: Maybe Text -- ^ A human-readable message providing more details about the error. For card errors, these messages can be shown to your users.
  , stripeErrorParam :: Maybe Text -- ^ If the error is parameter-specific, the parameter related to the error. For example, you can use this to display a message near the correct form field.
  , stripeErrorSource :: Maybe A.Object -- ^ The source object for errors returned on a request involving a source.
  }

newtype StripeVersion = StripeVersion ByteString

currentStripeVersion :: StripeVersion
currentStripeVersion = StripeVersion "2018-10-31"

newtype StripeKey = StripeKey ByteString

mkStripeState :: MonadIO m => StripeKey -> m StripeState
mkStripeState stripeStateKey = liftIO $ do
  stripeStateManager <- getGlobalManager
  stripeStateBaseRequest <- parseRequest "https://api.stripe.com/v1/"
  let stripeStateVersion = currentStripeVersion
  pure $ StripeState {..}

instance (MonadIO m) => StripeMonad (ReaderT StripeState m) where
  jsonGet p b qps = do
    StripeState {..} <- ask
    let (StripeVersion v) = stripeStateVersion
        (StripeKey k) = stripeStateKey
    r <-
      liftIO $
      httpLbs
        (setQueryString qps $
        applyBasicAuth k "" $
        stripeStateBaseRequest
          { requestHeaders =
              ("Stripe-Version", v) : requestHeaders stripeStateBaseRequest
          , path = path stripeStateBaseRequest <> b
          })
        stripeStateManager
    stripeResult $ tagWith p r
  formPost p b f = do
    StripeState {..} <- ask
    let (StripeVersion v) = stripeStateVersion
        (StripeKey k) = stripeStateKey
    r <-
      liftIO $
      httpLbs
        (applyBasicAuth k "" $
        stripeStateBaseRequest
          { requestHeaders =
              ("Stripe-Version", v) : (hContentType, "application/x-www-form-urlencoded") : requestHeaders stripeStateBaseRequest
          , method = "POST"
          , path = path stripeStateBaseRequest <> b
          , requestBody = RequestBodyLBS $ urlEncodeAsForm f
          })
        stripeStateManager
    stripeResult $ tagWith p r

runSimpleStripe :: (MonadIO m) => StripeState -> ReaderT StripeState m a -> m a
runSimpleStripe st m = runReaderT m st

-- | Get API key from STRIPE_SECRET_KEY
stripeWithEnv :: (MonadIO m) => ReaderT StripeState m a -> m a
stripeWithEnv m = do
  k <- liftIO $ getEnv "STRIPE_SECRET_KEY"
  st <- mkStripeState $ StripeKey $ C.pack k
  runSimpleStripe st m


arrayParams :: (ToHttpApiData a) => Text -> [a] -> Form
arrayParams t vs = Form $ H.singleton (t <> "[]") $ map toQueryParam vs

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
