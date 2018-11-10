module Stripe.Connect.ApplicationFeeRefunds where
import {-# SOURCE #-} Stripe.Connect.ApplicationFees
import Stripe.Balance
import Stripe.Utils

data FeeRefund = FeeRefund
  { feeRefundId :: Id FeeRefund
  , feeRefundAmount :: Integer
  , feeRefundBalanceTransaction :: Maybe (Expandable BalanceTransaction)
  , feeRefundCreated :: Timestamp
  , feeRefundCurrency :: CurrencyCode
  , feeRefundFee :: Expandable Fee
  , feeRefundMetadata :: Metadata
  } deriving (Show, Eq, Generic, Typeable)

instance FromJSON FeeRefund where
  parseJSON = parseObject "FeeRefund" $ do
    assertObject "fee_refund"
    FeeRefund
      <$> req "id"
      <*> req "amount"
      <*> opt "balance_transaction"
      <*> req "created"
      <*> req "currency"
      <*> req "fee"
      <*> req "metadata"

data CreateApplicationFeeRefund
data UpdateApplicationFeeRefund

-- createApplicationFeeRefund :: StripeMonad m => m FeeRefund
-- retrieveApplicationFeeRefund :: StripeMonad m => m FeeRefund
-- updateApplicationFeeRefund :: StripeMonad m => m FeeRefund
-- listAllApplicationFeeRefunds :: StripeMonad m => m FeeRefund
