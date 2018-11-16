module Stripe.Disputes where
import {-# SOURCE #-} Stripe.Balance
import Stripe.Charges
import Stripe.Core
import Stripe.PaymentMethods.BankAccounts
import Stripe.Utils

data DisputeReason
  = Duplicate
  | Fraudulent
  | SubscriptionCanceled
  | ProductUnacceptable
  | ProductNotReceived
  | Unrecognized
  | CreditNotProcessed
  | General
  | IncorrectAccountDetails
  | InsufficientFunds
  | BankCannotProcess
  | DebitNotAuthorized
  | CustomerInitiated
  deriving (Show, Eq, Generic, Typeable)

instance FromJSON DisputeReason where
  parseJSON = withText "DisputeReason" $ \t -> case t of
    "duplicate" -> pure Duplicate
    "fraudulent" -> pure Stripe.Disputes.Fraudulent
    "subscription_canceled" -> pure SubscriptionCanceled
    "product_unacceptable" -> pure ProductUnacceptable
    "product_not_received" -> pure ProductNotReceived
    "unrecognized" -> pure Unrecognized
    "credit_not_processed" -> pure CreditNotProcessed
    "general" -> pure General
    "incorrect_account_details" -> pure IncorrectAccountDetails
    "insufficient_funds" -> pure Stripe.Disputes.InsufficientFunds
    "bank_cannot_process" -> pure BankCannotProcess
    "debit_not_authorized" -> pure DebitNotAuthorized
    "customer_initiated" -> pure CustomerInitiated
    _ -> fail ("Invalid DisputeReason: " ++ show t)

data DisputeStatus
  = WarningNeedsResponse
  | WarningUnderReview
  | WarningClosed
  | NeedsResponse
  | UnderReview
  | ChargeRefunded
  | Won
  | Lost
  deriving (Show, Eq, Generic, Typeable)

instance FromJSON DisputeStatus where
  parseJSON = withText "DisputeStatus" $ \t -> case t of
    "warning_needs_response" -> pure WarningNeedsResponse
    "warning_under_review" -> pure WarningUnderReview
    "warning_closed" -> pure WarningClosed
    "needs_response" -> pure NeedsResponse
    "under_review" -> pure UnderReview
    "charge_refunded" -> pure ChargeRefunded
    "won" -> pure Won
    "lost" -> pure Lost
    _ -> fail ("Invalid DisputeStatus: " ++ show t)

data Evidence = Evidence
  { evidenceAccessActivityLog :: Maybe Text
  , evidenceBillingAddress :: Maybe Text
  , evidenceCancellationPolicy :: Maybe Text
  , evidenceCancellationPolicyDisclosure :: Maybe Text
  , evidenceCancellationRebuttal :: Maybe Text
  , evidenceCustomerCommunication :: Maybe Text
  , evidenceCustomerEmailAddress :: Maybe Text
  , evidenceCustomerName :: Maybe Text
  , evidenceCustomerPurchaseIp :: Maybe Text
  , evidenceCustomerSignature :: Maybe Text
  , evidenceDuplicateChargeDocumentation :: Maybe Text
  , evidenceDuplicateChargeExplanation :: Maybe Text
  , evidenceDuplicateChargeId :: Maybe Text
  , evidenceProductDescription :: Maybe Text
  , evidenceReceipt :: Maybe Text
  , evidenceRefundPolicy :: Maybe Text
  , evidenceRefundPolicyDisclosure :: Maybe Text
  , evidenceRefundRefusalExplanation :: Maybe Text
  , evidenceServiceDate :: Maybe Text
  , evidenceServiceDocumentation :: Maybe Text
  , evidenceShippingAddress :: Maybe Text
  , evidenceShippingCarrier :: Maybe Text
  , evidenceShippingDate :: Maybe Text
  , evidenceShippingDocumentation :: Maybe Text
  , evidenceShippingTrackingNumber :: Maybe Text
  , evidenceUncategorizedFile :: Maybe Text
  , evidenceUncategorizedText :: Maybe Text
  } deriving (Show, Eq, Generic, Typeable)

instance FromJSON Evidence where
  parseJSON = parseObject "Evidence" $ do
    Evidence
      <$> opt "access_activity_log"
      <*> opt "billing_address"
      <*> opt "cancellation_policy"
      <*> opt "cancellation_policy_disclosure"
      <*> opt "cancellation_rebuttal"
      <*> opt "customer_communication"
      <*> opt "customer_email_address"
      <*> opt "customer_name"
      <*> opt "customer_purchase_ip"
      <*> opt "customer_signature"
      <*> opt "duplicate_charge_documentation"
      <*> opt "duplicate_charge_explanation"
      <*> opt "duplicate_charge_id"
      <*> opt "product_description"
      <*> opt "receipt"
      <*> opt "refund_policy"
      <*> opt "refund_policy_disclosure"
      <*> opt "refund_refusal_explanation"
      <*> opt "service_date"
      <*> opt "service_documentation"
      <*> opt "shipping_address"
      <*> opt "shipping_carrier"
      <*> opt "shipping_date"
      <*> opt "shipping_documentation"
      <*> opt "shipping_tracking_number"
      <*> opt "uncategorized_file"
      <*> opt "uncategorized_text"

data EvidenceDetails = EvidenceDetails
  { evidenceDetailsDueBy :: Timestamp
  , evidenceDetailsHasEvidence :: Bool
  , evidenceDetailsPastDue :: Bool
  , evidenceDetailsSubmissionCount :: Int
  } deriving (Show, Eq, Generic, Typeable)

instance FromJSON EvidenceDetails where
  parseJSON = parseObject "EvidenceDetails" $ do
    EvidenceDetails
      <$> req "due_by"
      <*> req "has_evidence"
      <*> req "past_due"
      <*> req "submission_count"

data Dispute = Dispute
  { disputeId :: Id Dispute
  , disputeAmount :: Integer
  , disputeBalanceTransaction :: Maybe (Id BalanceTransaction)
  , disputeBalanceTransactions :: [BalanceTransaction]
  , disputeCharge :: Expandable Charge
  , disputeCreated :: Timestamp
  , disputeCurrency :: CurrencyCode
  , disputeEvidence :: Evidence
  , disputeEvidenceDetails :: EvidenceDetails
  , disputeIsChargeRefundable :: Bool
  , disputeLiveMode :: Bool
  , disputeMetadata :: Metadata
  , disputeReason :: DisputeReason
  , disputeStatus :: DisputeStatus
  } deriving (Show, Eq, Generic, Typeable)

instance FromJSON Dispute where
  parseJSON = parseObject "Dispute" $ do
    assertObject "dispute"
    Dispute
      <$> req "id"
      <*> req "amount"
      <*> opt "balance_transaction"
      <*> req "balance_transactions"
      <*> req "charge"
      <*> req "created"
      <*> req "currency"
      <*> req "evidence"
      <*> req "evidence_details"
      <*> req "is_charge_refundable"
      <*> req "livemode"
      <*> req "metadata"
      <*> req "reason"
      <*> req "status"

retrieveDispute :: (MonadStripe m, StripeResult Dispute dispute) => Id Dispute -> m dispute
retrieveDispute (Id disputeId) = stripeGet (Proxy @Dispute) ("disputes/" <> encodeUtf8 disputeId) []

-- updateDispute
-- closeDispute

listAllDisputes :: (MonadStripe m, StripeResult (List Dispute) disputeList) => Pagination Dispute -> m disputeList
listAllDisputes = stripeGet (Proxy @(List Dispute)) "disputes" . paginationParams

data UpdateDispute
