module Stripe.PaymentMethods.Sources where
import Stripe.Customers
import Stripe.Utils

data CodeVerificationStatus
  = VerificationPending
  | VerificationSucceeded
  | VerificationFailed
  deriving (Show, Eq, Generic, Typeable)

instance FromJSON CodeVerificationStatus where
  parseJSON = withText "CodeVerificationStatus" $ \t -> case t of
    "pending" -> pure VerificationPending
    "succeeded" -> pure VerificationSucceeded
    "failed" -> pure VerificationFailed
    _ -> fail ("Invalid CodeVerificationStatus: " ++ show t)

data Flow
  = RedirectFlow
  | ReceiverFlow
  | CodeVerificationFlow
  | NoFlow
  deriving (Show, Eq, Generic, Typeable)

instance FromJSON Flow where
  parseJSON = withText "Flow" $ \t -> case t of
    "redirect" -> pure RedirectFlow
    "receiver" -> pure ReceiverFlow
    "code_verification" -> pure CodeVerificationFlow
    "none" -> pure NoFlow
    _ -> fail ("Invalid Flow: " ++ show t)

data CodeVerification = CodeVerification
  { codeVerificationAttemptsRemaining :: Word
  , codeVerificationStatus :: CodeVerificationStatus
  } deriving (Show, Eq, Generic, Typeable)

instance FromJSON CodeVerification where
  parseJSON = parseObject "CodeVerification" $ do
    CodeVerification
      <$> req "attempts_remaining"
      <*> req "status"

data Owner = Owner
  { ownerAddress :: Maybe Address
  , ownerEmail :: Text
  , ownerName :: Maybe Text
  , ownerPhone :: Maybe Text
  , ownerVerifiedAddress :: Maybe Address
  , ownerVerifiedEmail :: Maybe Text
  , ownerVerifiedName :: Maybe Text
  , ownerVerifiedPhone :: Maybe Text
  } deriving (Show, Eq, Generic, Typeable)

instance FromJSON Owner where
  parseJSON = parseObject "Owner" $ do
    Owner
      <$> opt "address"
      <*> req "email"
      <*> opt "name"
      <*> opt "phone"
      <*> opt "verified_address"
      <*> opt "verified_email"
      <*> opt "verified_name"
      <*> opt "verified_phone"

data RedirectFailureReason
  = UserAbort
  | Declined
  | ProcessingError
  deriving (Show, Eq, Generic, Typeable)

instance FromJSON RedirectFailureReason where
  parseJSON = withText "RedirectFailureReason" $ \t -> case t of
    "user_abort" -> pure UserAbort
    "declined" -> pure Declined
    "processing_error" -> pure ProcessingError
    _ -> fail ("Invalid RedirectFailureReason: " ++ show t)

data RedirectStatus
  = RedirectPending
  | RedirectSucceeded
  | RedirectNotRequired
  | RedirectFailed
  deriving (Show, Eq, Generic, Typeable)

instance FromJSON RedirectStatus where
  parseJSON = withText "RedirectStatus" $ \t -> case t of
    "pending" -> pure RedirectPending
    "succeeded" -> pure RedirectSucceeded
    "not_required" -> pure RedirectNotRequired
    "failed" -> pure RedirectFailed
    _ -> fail ("Invalid RedirectStatus: " ++ show t)

data Redirect = Redirect
  { redirectFailureReason :: Maybe RedirectFailureReason
  , redirectReturnUrl :: Text
  , redirectStatus :: RedirectStatus
  , redirectUrl :: Text
  } deriving (Show, Eq, Generic, Typeable)

instance FromJSON Redirect where
  parseJSON = parseObject "Redirect" $ do
    Redirect
      <$> opt "failure_reason"
      <*> req "return_url"
      <*> req "status"
      <*> req "url"

data SourceStatus
  = SourceCanceled
  | SourceChargeable
  | SourceConsumed
  | SourceFailed
  | SourcePending
  deriving (Show, Eq, Generic, Typeable)

instance FromJSON SourceStatus where
  parseJSON = withText "SourceStatus" $ \t -> case t of
    "canceled" -> pure SourceCanceled
    "chargeable" -> pure SourceChargeable
    "consumed" -> pure SourceConsumed
    "failed" -> pure SourceFailed
    "pending" -> pure SourcePending
    _ -> fail ("Invalid SourceStatus: " ++ show t)

data SourceType
  = ACHCreditTransfer Object
  | ACHDebit Object
  | AliPay Object
  | Bancontact Object
  | Card Object
  | CardPresent Object
  | EPS Object
  | Giropay Object
  | Ideal Object
  | Multibanco Object
  | P24 Object
  | PaperCheck Object
  | SEPACreditTransfer Object
  | SEPADebit Object
  | Sofort Object
  | ThreeDSecure Object
  deriving (Show, Eq, Generic, Typeable)

getSourceType :: Parser SourceType
getSourceType = do
  t <- req "type"
  case t of
    "ach_credit_transfer" -> ACHCreditTransfer <$> req t
    "ach_debit" -> ACHDebit <$> req t
    "alipay" -> AliPay <$> req t
    "bancontact" -> Bancontact <$> req t
    "card" -> Card <$> req t
    "card_present" -> CardPresent <$> req t
    "eps" -> EPS <$> req t
    "giropay" -> Giropay <$> req t
    "ideal" -> Ideal <$> req t
    "multibanco" -> Multibanco <$> req t
    "p24" -> P24 <$> req t
    "paper_check" -> PaperCheck <$> req t
    "sepa_credit_transfer" -> SEPACreditTransfer <$> req t
    "sepa_debit" -> SEPADebit <$> req t
    "sofort" -> Sofort <$> req t
    "three_d_secure" -> ThreeDSecure <$> req t
    _ -> fail ("Invalid SourceStatus: " ++ show t)

data Receiver = Receiver
  { receiverAddress :: Text
  , receiverAmountCharged :: Integer
  , receiverAmountReceived :: Integer
  , receiverAmountReturned :: Integer
  -- TODO Stripe doesn't have docs for other fields
  , receiverRefundAttributesMethod :: Maybe Text
  , receiverRefundAttributesStatus :: Maybe Text
  } deriving (Show, Eq, Generic, Typeable)

instance FromJSON Receiver where
  parseJSON = parseObject "Receiver" $ do
    Receiver
      <$> req "address"
      <*> req "amount_charged"
      <*> req "amount_received"
      <*> req "amount_returned"
      <*> opt "refund_attributes_method"
      <*> opt "refund_attributes_status"

data SourceUsage
  = Reusable
  | SingleRun
  deriving (Show, Eq, Generic, Typeable)

instance FromJSON SourceUsage where
  parseJSON = withText "SourceUsage" $ \t -> case t of
    "reusable" -> pure Reusable
    "single_use" -> pure SingleRun

data Source = Source
  { sourceId :: Id Source
  , sourceAmount :: Maybe Word
  -- TODO no idea what this is for
  , sourceClientSecret :: Text
  -- TODO wat
  , sourceCodeVerification :: Maybe CodeVerification
  , sourceCreated :: Timestamp
  , sourceCurrency :: Text -- TODO
  , sourceCustomer :: Maybe (Id Customer)
  , sourceFlow :: Flow
  , sourceLiveMode :: Bool
  , sourceMetadata :: Metadata
  , sourceOwner :: Owner
  , sourceReceiver :: Receiver
  , sourceRedirect :: Maybe Redirect
  , sourceStatementDescriptor :: Maybe Text
  , sourceStatus :: SourceStatus
  , sourceType_ :: SourceType
  -- TODO sourceType determines existence of one extra field
  , sourceUsage :: SourceUsage
  } deriving (Show, Eq, Generic, Typeable)

instance FromJSON Source where
  parseJSON = parseObject "Source" $ do
    assertObject "source"
    Source
      <$> req "id"
      <*> req "amount"
      <*> req "client_secret"
      <*> opt "code_verification"
      <*> req "created"
      <*> req "currency"
      <*> opt "customer"
      <*> req "flow"
      <*> req "livemode"
      <*> req "metadata"
      <*> req "owner"
      <*> req "receiver"
      <*> opt "redirect"
      <*> opt "statement_descriptor"
      <*> req "status"
      <*> getSourceType
      <*> req "usage"

{-
createSource
retrieveSource
updateSource
attachSource
detachSource
-}
