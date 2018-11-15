module Stripe.Connect.ApplicationFees where
import Stripe.Connect.Account
import Stripe.Connect.ApplicationFeeRefunds
import Stripe.Charges
import Stripe.Utils

data Fee = Fee
  { feeId :: Id Fee
  , feeAccount :: Expandable Account
  , feeAmount :: Integer
  , feeAmountRefunded :: Integer
  , feeApplication :: Expandable Application
  , feeBalanceTransaction :: Expandable ()
  , feeCharge :: Expandable Charge
  , feeCreated :: Timestamp
  , feeCurrency :: CurrencyCode
  , feeLiveMode :: Bool
  , feeOriginatingTransaction :: Expandable ()
  , feeRefunded :: Bool
  , feeRefunds :: List FeeRefund
  } deriving (Show, Eq, Generic, Typeable)

instance FromJSON Fee where
  parseJSON = parseObject "Fee" $ do
    assertObject "application_fee"
    Fee
      <$> req "id"
      <*> req "account"
      <*> req "amount"
      <*> req "amount_refunded"
      <*> req "application"
      <*> req "balance_transaction"
      <*> req "charge"
      <*> req "created"
      <*> req "currency"
      <*> req "livemode"
      <*> req "originating_transaction"
      <*> req "refunded"
      <*> req "refunds"


retrieveApplicationFee :: (MonadStripe m, StripeResult Fee fee) => Id Fee -> m fee
retrieveApplicationFee (Id feeId) = stripeGet (Proxy @Fee) ("application_fees/" <> encodeUtf8 feeId) []

listAllApplicationFees :: (MonadStripe m, StripeResult (List Fee) feeList) => m feeList
listAllApplicationFees = stripeGet (Proxy @(List Fee)) "application_fees" []
