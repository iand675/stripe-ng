module Stripe.Payouts where
import Stripe.PaymentMethods.BankAccounts
import Stripe.Balance
import Stripe.Utils

data PayoutMethod
  = Instant
  | Standard
  deriving (Show, Eq, Generic, Typeable)

data PayoutStatus
  = Paid
  | Pending
  | InTransit
  | Canceled
  | Failed
  deriving (Show, Eq, Generic, Typeable)

data PayoutSource
  = BankAccountSource
  | CardSource
  | AliPaySource
  deriving (Show, Eq, Generic, Typeable)

data PayoutType
  = BankAccountPayout
  | CardPayout
  deriving (Show, Eq, Generic, Typeable)

data Payout = Payout
  { payoutId :: Id Payout
  , payoutAmount :: Word
  , payoutArrivalDate :: Timestamp
  , payoutAutomatic :: Bool
  , payoutBalanceTransaction :: Id BalanceTransaction
  , payoutCreated :: Timestamp
  , payoutCurrency :: CurrencyCode
  , payoutDescription :: Text
  , payoutDestination :: Expandable BankAccount
  , payoutFailureBalanceTransaction :: Maybe (Expandable BalanceTransaction)
  , payoutFailureCode :: Maybe Text
  , payoutFailureMessage :: Maybe Text
  , payoutLiveMode :: Bool
  , payoutMetadata :: Metadata
  , payoutMethod :: PayoutMethod
  , payoutSourceType :: PayoutSource
  , payoutStatementDescriptor :: Maybe Text
  , payoutStatus :: PayoutStatus
  , payoutType_ :: PayoutType
  } deriving (Show, Eq, Generic, Typeable)

{-
createPayout
retrievePayout
updatePayout
listAllPayouts
cancelPayout
-}
