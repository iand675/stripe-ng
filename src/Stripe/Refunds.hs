module Stripe.Refunds where
import Stripe.Balance (BalanceTransaction)
import Stripe.Charges
import Stripe.Utils

data FailureReason
  = LostOrStolenCard
  | ExpiredOrCanceledCard
  | Unknown
  deriving (Show, Eq, Generic, Typeable)

instance FromJSON FailureReason where
  parseJSON = withText "FailureReason" $ \t -> case t of
    "lost_or_stolen_card" -> pure LostOrStolenCard
    "expired_or_canceled_card" -> pure ExpiredOrCanceledCard
    "unknown" -> pure Unknown
    _ -> fail ("Invalid FailureReason: " ++ show t)

data RefundReason
  = Duplicate
  | Fraudulent
  | RequestedByCustomer
  deriving (Show, Eq, Generic, Typeable)

instance FromJSON RefundReason where
  parseJSON = withText "RefundReason" $ \t -> case t of
    "duplicate" -> pure Duplicate
    "fraudulent" -> pure Stripe.Refunds.Fraudulent
    "requested_by_customer" -> pure RequestedByCustomer
    _ -> fail ("Invalid RefundReason: " ++ show t)

data RefundStatus
  = Succeeded
  | Failed
  | Pending
  | Canceled
  deriving (Show, Eq, Generic, Typeable)

instance FromJSON RefundStatus where
  parseJSON = withText "RefundStatus" $ \t -> case t of
    "succeeded" -> pure Succeeded
    "failed" -> pure Failed
    "pending" -> pure Pending
    "canceled" -> pure Canceled
    _ -> fail ("Invalid RefundStatus: " ++ show t)

data Refund = Refund
  { refundId :: Id Refund
  , refundAmount :: Integer
  -- TODO what shape does this have? Expandable
  , refundBalanceTransaction :: Maybe (Expandable BalanceTransaction)
  , refundCharge :: Expandable Charge
  , refundCreated :: Timestamp
  , refundCurrency :: Text
  , refundMetadata :: Metadata
  , refundFailureBalanceTransaction :: Maybe (Expandable BalanceTransaction)
  , refundFailureReason :: Maybe FailureReason
  , refundReason :: Maybe RefundReason
  , refundReceiptNumber :: Maybe Text
  -- TODO expandable
  , refundSourceTransferReversal :: Maybe (Expandable Text)
  , refundStatus :: RefundStatus
  } deriving (Show, Eq, Generic, Typeable)

instance FromJSON Refund where
  parseJSON = parseObject "Refund" $ do
    assertObject "refund"
    Refund
      <$> req "id"
      <*> req "amount"
      <*> opt "balance_transaction"
      <*> req "charge"
      <*> req "created"
      <*> req "currency"
      <*> req "metadata"
      <*> opt "failure_balance_transaction"
      <*> req "failure_reason"
      <*> req "reason"
      <*> req "receipt_number"
      <*> req "source_transfer_reversal"
      <*> req "status"

data CreateRefund
data UpdateRefund

{-
createRefund
retrieveRefund
updateRefund
listAllRefunds
-}
