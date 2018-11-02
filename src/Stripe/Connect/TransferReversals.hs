module Stripe.Connect.TransferReversals where
import Stripe.Balance
import Stripe.Connect.Transfers
import Stripe.Utils

data TransferReversal = TransferReversal
  { transferReversalId :: Id TransferReversal
  , transferReversalAmount :: Integer
  , transferReversalBalanceTransaction :: Maybe (Expandable BalanceTransaction)
  , transferReversalCreated :: Timestamp
  , transferReversalCurrency :: CurrencyCode
  , transferReversalDestinationPaymentRefund :: Maybe Text
  , transferReversalMetadata :: Metadata
  , transferReversalSourceRefund :: Maybe Text
  , transferReversalTransfer :: Expandable Transfer
  }

instance FromJSON TransferReversal where
  parseJSON = parseObject "TransferReversal" $ do
    assertObject "transfer_reversal"
    TransferReversal
      <$> req "id"
      <*> req "amount"
      <*> opt "balance_transaction"
      <*> req "created"
      <*> req "currency"
      <*> req "destination_payment_refund"
      <*> req "metadata"
      <*> opt "source_refund"
      <*> req "transfer"

-- createReversal

retrieveReversal :: (StripeMonad m) => Id Transfer -> Id TransferReversal -> m TransferReversal
retrieveReversal (Id transferId) (Id transferReversalId) =
  jsonGet
    ("transfers/" <> encodeUtf8 transferId <> "/reversals/" <>
     encodeUtf8 transferReversalId)
    []

-- updateReversal

listAllReversals :: (StripeMonad m) => Id Transfer -> m (List TransferReversal)
listAllReversals (Id transferId) =
  jsonGet ("transfers/" <> encodeUtf8 transferId <> "/reversals") []
