module Stripe.Connect.Transfers where
import Stripe.Balance (BalanceTransaction)
import Stripe.Utils

data TransferSourceType
  = CardTransferSource
  | BankAccountTransferSource
  | AlipayAccountTransferSource
  deriving (Show, Eq, Generic, Typeable)

instance FromJSON TransferSourceType where
  parseJSON = withText "TransferSourceType" $ \t -> case t of
    "card" -> pure CardTransferSource
    "bank_account" -> pure BankAccountTransferSource
    "alipay_account" -> pure AlipayAccountTransferSource
    _ -> fail ("Invalid TransferSourceType: " ++ show t)

data Transfer = Transfer
  { transferId :: Id Transfer
  , transferAmount :: Integer
  , transferAmountReversed :: Integer
  -- TODO
  , transferBalanceTransaction :: Expandable ()
  , transferCreated :: Timestamp
  , transferCurrency :: CurrencyCode
  , transferDescription :: Maybe Text
  -- TODO
  , transferDestination :: Expandable ()
  -- TODO
  , transferDestinationPayment :: Expandable ()
  , transferLiveMode :: Bool
  , transferMetadata :: Metadata
  -- TODO
  , transferReversals :: List ()
  , transferReversed :: Bool
  , transferSourceTransaction :: Maybe (Expandable ())
  , transferSourceType :: TransferSourceType
  , transferTransferGroup :: Maybe Text
  } deriving (Show, Eq, Generic, Typeable)

-- createTransfer
-- retrieveTransfer
-- updateTransfer
-- listAllTransfers


data CreateTransfer
data UpdateTransfer
