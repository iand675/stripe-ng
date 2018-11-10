module Stripe.Charges where
import {-# SOURCE #-} Stripe.Billing.Invoices
import Stripe.Terminal.PaymentIntents
import Stripe.Customers
import Stripe.Connect.Account
import {-# SOURCE #-} Stripe.Disputes
import {-# SOURCE #-} Stripe.Orders
import {-# SOURCE #-} Stripe.Refunds
import Stripe.Utils

data UserReportedFraud
  = UserReportedSafe
  | UserReportedFraudulent
 deriving (Show, Eq, Generic, Typeable)

instance FromJSON UserReportedFraud where
  parseJSON = withText "UserReportedFraud" $ \t -> case t of
    "safe" -> pure UserReportedSafe
    "fraudulent" -> pure UserReportedFraudulent
    _ -> fail ("Invalid UserReportedFraud: " ++ show t)

data StripeReportedFraud = StripeReportedFraudulent
  deriving (Show, Eq, Generic, Typeable)

instance FromJSON StripeReportedFraud where
  parseJSON = withText "StripeReportedFraud" $ \t -> case t of
    "fraudulent" -> pure StripeReportedFraudulent
    _ -> fail ("Invalid StripeReportedFraud: " ++ show t)

data FraudDetails = FraudDetails
  { fraudDetailsUserReport :: Maybe UserReportedFraud
  , fraudDetailsStripeReport :: Maybe StripeReportedFraud
  } deriving (Show, Eq, Generic, Typeable)

instance FromJSON FraudDetails where
  parseJSON = parseObject "FraudDetails" $ do
    FraudDetails <$> opt "user_report" <*> opt "stripe_report"

data OutcomeReason
  = Rule
  | ElevatedRiskLevel
  | HighestRiskLevel
  deriving (Show, Eq, Generic, Typeable)

instance FromJSON OutcomeReason where
  parseJSON = withText "OutcomeReason" $ \t -> case t of
    "rule" -> pure Rule
    "elevated_risk_level" -> pure ElevatedRiskLevel
    "highest_risk_level" -> pure HighestRiskLevel
    _ -> fail ("Invalid OutcomeReason: " ++ show t)

data NetworkStatus
  = ApprovedByNetwork
  | DeclinedByNetwork
  | NotSentToNetwork
  | ReversedAfterApproval
  deriving (Show, Eq, Generic, Typeable)

instance FromJSON NetworkStatus where
  parseJSON = withText "NetworkStatus" $ \t -> case t of
    "approved_by_network" -> pure ApprovedByNetwork
    "declined_by_network" -> pure DeclinedByNetwork
    "not_sent_to_network" -> pure NotSentToNetwork
    "reversed_after_approval" -> pure ReversedAfterApproval
    _ -> fail ("Invalid NetworkStatus: " ++ show t)

data RiskLevel
  = Normal
  | Elevated
  | Highest
  deriving (Show, Eq, Generic, Typeable)

instance FromJSON RiskLevel where
  parseJSON = withText "RiskLevel" $ \t -> case t of
    "normal" -> pure Normal
    "elevated" -> pure Elevated
    "highest" -> pure Highest
    _ -> fail ("Invalid RiskLevel: " ++ show t)

data OutcomeType
  = Authorized
  | ManualReview
  | IssuerDeclined
  | Blocked
  | Invalid
  deriving (Show, Eq, Generic, Typeable)

instance FromJSON OutcomeType where
  parseJSON = withText "OutcomeType" $ \t -> case t of
    "authorized" -> pure Authorized
    "manual_review" -> pure ManualReview
    "issuer_declined" -> pure IssuerDeclined
    "blocked" -> pure Blocked
    "invalid" -> pure Invalid
    _ -> fail ("Invalid OutcomeType: " ++ show t)

-- TODO
data RadarRule

data Outcome = Outcome
  { outcomeNetworkStatus :: NetworkStatus
  , outcomeReason :: Maybe OutcomeReason
  , outcomeRiskLevel :: Maybe RiskLevel
  , outcomeRiskScore :: Maybe Integer
  , outcomeRiskRule :: Maybe (Expandable RadarRule)
  , outcomeSellerMessage :: Maybe Text
  , outcomeType_ :: OutcomeType
  } deriving (Show, Eq, Generic, Typeable)

instance FromJSON Outcome where
  parseJSON = parseObject "Outcome" $do
    Outcome
      <$> req "network_status"
      <*> opt "reason"
      <*> opt "risk_level"
      <*> opt "risk_score"
      <*> opt "risk_rule"
      <*> opt "seller_message"
      <*> req "type"

data ChargeStatus
  = ChargeSucceeded
  | ChargePending
  | ChargeFailed
  deriving (Show, Eq, Generic, Typeable)

instance FromJSON ChargeStatus where
  parseJSON = withText "ChargeStatus" $ \t -> case t of
    "succeeded" -> pure ChargeSucceeded
    "pending" -> pure ChargePending
    "failed" -> pure ChargeFailed
    _ -> fail ("Invalid ChargeStatus: " ++ show t)

data Charge = Charge
  { chargeId :: Id Charge
  , chargeAmount :: Integer
  , chargeAmountRefunded :: Integer
  , chargeApplication :: Maybe (Expandable Application)
  , chargeApplicationFee :: Maybe Text
  -- TODO Expandable
  , chargeBalanceTransaction :: Maybe Text
  , chargeCaptured :: Bool
  , chargeCreated :: Timestamp
  , chargeCurrency :: CurrencyCode
  , chargeCustomer :: Maybe (Expandable Customer)
  , chargeDescription :: Maybe Text
  , chargeDestination :: Maybe (Expandable Account)
  , chargeDispute :: Maybe (Expandable Dispute)
  , chargeFailureCode :: Maybe Text
  , chargeFailureMessage :: Maybe Text
  , chargeFraudDetails :: Maybe FraudDetails
  , chargeInvoice :: Maybe (Expandable Invoice)
  , chargeLiveMode :: Bool
  , chargeMetadata :: Metadata
  , chargeOnBehalfOf :: Maybe (Expandable Account)
  , chargeOrder :: Maybe (Expandable Order)
  , chargeOutcome :: Outcome
  , chargePaid :: Bool
  , chargePaymentIntent :: Maybe (Id PaymentIntent)
  , chargeReceiptEmail :: Maybe Text
  , chargeReceiptNumber :: Maybe Text
  , chargeRefunded :: Bool
  , chargeRefunds :: List Refund
  -- TODO what is it
  , chargeReview :: Maybe (Expandable Text)
  , chargeShipping :: Maybe ShippingDetails
  -- TODO can be card or...
  , chargeSource :: Object
  -- TODO transfer
  , chargeSourceTransfer :: Maybe (Expandable ())
  , chargeStatementDescriptor :: Maybe Text
  , chargeStatus :: ChargeStatus
  , chargeTransfer :: Maybe (Expandable ())
  , chargeTransferGroup :: Maybe Text
  } deriving (Show, Eq, Generic, Typeable)

instance FromJSON Charge where
  parseJSON = parseObject "Charge" $ do
    assertObject "charge"
    Charge
      <$> req "id"
      <*> req "amount"
      <*> req "amount_refunded"
      <*> opt "application"
      <*> opt "application_fee"
      <*> opt "balance_transaction"
      <*> req "captured"
      <*> req "created"
      <*> req "currency"
      <*> opt "customer"
      <*> opt "description"
      <*> opt "destination"
      <*> opt "dispute"
      <*> opt "failure_code"
      <*> opt "failure_message"
      <*> opt "fraud_details"
      <*> opt "invoice"
      <*> req "livemode"
      <*> req "metadata"
      <*> opt "on_behalf_of"
      <*> opt "order"
      <*> req "outcome"
      <*> req "paid"
      <*> opt "payment_intent"
      <*> opt "receipt_email"
      <*> opt "receipt_number"
      <*> req "refunded"
      <*> req "refunds"
      <*> opt "review"
      <*> opt "shipping"
      <*> req "source"
      <*> opt "source_transfer"
      <*> opt "statement_descriptor"
      <*> req "status"
      <*> opt "transfer"
      <*> opt "transfer_group"

-- createCharge

retrieveCharge :: (StripeMonad m, StripeResult Charge charge) => Id Charge -> m charge
retrieveCharge (Id chargeId) = jsonGet (Proxy @Charge) ("charges/" <> encodeUtf8 chargeId) []

-- updateCharge

-- captureCharge

listAllCharges :: (StripeMonad m, StripeResult (List Charge) chargeList) => m chargeList
listAllCharges = jsonGet (Proxy @(List Charge)) "charges" []

data CreateCharge
data UpdateCharge
data CaptureCharge
