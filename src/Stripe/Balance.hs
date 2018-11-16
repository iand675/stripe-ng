module Stripe.Balance where
import Stripe.Charges
import Stripe.Core
import Stripe.Utils

-- TODO Better source_type support
-- https://stripe.com/docs/api/sources/object#source_object-type

-- TODO Better currency support
data BalanceFunds = BalanceFunds
  { balanceFundsCurrency :: CurrencyCode
  , balanceFundsAmount :: Integer
  , balanceFundsSourceTypes :: HashMap Text Integer
  } deriving (Show, Eq, Generic, Typeable)

instance FromJSON BalanceFunds where
  parseJSON = parseObject "BalanceFunds" $ do
    BalanceFunds
      <$> req "currency"
      <*> req "amount"
      <*> req "source_types"

data Balance = Balance
  { balanceAvailable :: [BalanceFunds]
  , balanceConnectReserved :: Maybe [BalanceFunds]
  , balanceLiveMode :: Bool
  , balancePending :: [BalanceFunds]
  } deriving (Show, Eq, Generic, Typeable)

instance FromJSON Balance where
  parseJSON =
    parseObject "Balance" $ do
      assertObject "balance"
      Balance
        <$> req "available"
        <*> opt "connect_reserved"
        <*> req "livemode"
        <*> req "pending"

-- retrieveBalance :: StripeMonad m => m Balance
-- retrieveBalance = jsonGet "/v1/balance"

data TransactionStatus
  = TransactionAvailable
  | TransactionPending
  deriving (Show, Eq, Generic, Typeable)

instance FromJSON TransactionStatus where
  parseJSON = withText "TransactionStatus" $ \t -> case t of
    "available" -> pure TransactionAvailable
    "pending" -> pure TransactionPending
    _ -> fail ("Invalid TransactionStatus: " ++ show t)

data TransactionType
  = Adjustment
  | Advance
  | AdvanceFunding
  | ApplicationFeeTransaction
  | ApplicationFeeRefund
  | ChargeTransaction
  | ConnectCollectionTransfer
  | IssuingAuthorizationHold
  | IssuingAuthorizationRelease
  | IssuingTransaction
  | Payment
  | PaymentFailureRefund
  | PaymentRefund
  | Payout
  | PayoutCancel
  | PayoutFailure
  | Refund
  | RefundFailure
  | ReserveTransaction
  | ReservedFunds
  | StripeFeeTransaction
  | StripeFxFee
  | TaxFee
  | Topup
  | TopupReversal
  | Transfer
  | TransferCancel
  | TransferFailure
  | TransferRefund
  deriving (Show, Eq, Generic, Typeable)

instance FromJSON TransactionType where
  parseJSON = withText "TransactionType" $ \t -> case t of
    "adjustment" -> pure Adjustment
    "advance" -> pure Advance
    "advance_funding" -> pure AdvanceFunding
    "application_fee" -> pure ApplicationFeeTransaction
    "application_fee_refund" -> pure ApplicationFeeRefund
    "charge" -> pure ChargeTransaction
    "connect_collection_transfer" -> pure ConnectCollectionTransfer
    "issuing_authorization_hold" -> pure IssuingAuthorizationHold
    "issuing_authorization_release" -> pure IssuingAuthorizationRelease
    "issuing_transaction" -> pure IssuingTransaction
    "payment" -> pure Payment
    "payment_failure_refund" -> pure PaymentFailureRefund
    "payment_refund" -> pure PaymentRefund
    "payout" -> pure Payout
    "payout_cancel" -> pure PayoutCancel
    "payout_failure" -> pure PayoutFailure
    "refund" -> pure Refund
    "refund_failure" -> pure RefundFailure
    "reserve_transaction" -> pure ReserveTransaction
    "reserved_funds" -> pure ReservedFunds
    "stripe_fee" -> pure StripeFeeTransaction
    "stripe_fx_fee" -> pure StripeFxFee
    "tax_fee" -> pure TaxFee
    "topup" -> pure Topup
    "topup_reversal" -> pure TopupReversal
    "transfer" -> pure Transfer
    "transfer_cancel" -> pure TransferCancel
    "transfer_failure" -> pure TransferFailure
    "transfer_refund" -> pure TransferRefund
    _ -> fail ("Invalid TransactionStatus: " ++ show t)

data FeeType
  = ApplicationFee
  | StripeFee
  | Tax
  deriving (Show, Eq, Generic, Typeable)

instance FromJSON FeeType where
  parseJSON = withText "FeeType" $ \t -> case t of
    "application_fee" -> pure ApplicationFee
    "stripe_fee" -> pure StripeFee
    "tax" -> pure Tax
    _ -> fail ("Invalid FeeType: " ++ show t)

data FeeDetails = FeeDetails
  { feeDetailsId :: Maybe (Id FeeDetails)
  , feeDetailsAmount :: Integer
  , feeDetailsApplication :: Maybe (Id Application)
  , feeDetailsCurrency :: CurrencyCode
  , feeDetailsDescription :: Maybe Text
  , feeDetailsType_ :: FeeType
  } deriving (Show, Eq, Generic, Typeable)

instance FromJSON FeeDetails where
  parseJSON = parseObject "FeeDetails" $ do
    FeeDetails
      <$> opt "id"
      <*> req "amount"
      <*> opt "application"
      <*> req "currency"
      <*> opt "description"
      <*> req "type"

data BalanceTransaction = BalanceTransaction
  { balanceTransactionId :: Id BalanceTransaction
  , balanceTransactionAmount :: Integer
  , balanceTransactionAvailableOn :: Timestamp
  , balanceTransactionCreated :: Timestamp
  , balanceTransactionCurrency :: CurrencyCode
  , balanceTransactionDescription :: Maybe Text
  , balanceTransactionExchangeRate :: Maybe Double
  , balanceTransactionFee :: Integer
  , balanceTransactionFeeDetails :: [FeeDetails]
  , balanceTransactionNet :: Integer
  , balanceTransactionSource :: Expandable Charge
  , balanceTransactionStatus :: TransactionStatus
  , balanceTransactionType_ :: TransactionType
  } deriving (Show, Eq, Generic, Typeable)

instance FromJSON BalanceTransaction where
  parseJSON = parseObject "BalanceTransaction" $ do
    assertObject "balance_transaction"
    BalanceTransaction
      <$> req "id"
      <*> req "amount"
      <*> req "available_on"
      <*> req "created"
      <*> req "currency"
      <*> opt "description"
      <*> opt "exchange_rate"
      <*> req "fee"
      <*> req "fee_details"
      <*> req "net"
      <*> req "source"
      <*> req "status"
      <*> req "type"

retrieveBalance :: (MonadStripe m, StripeResult Balance balance) => m balance
retrieveBalance = stripeGet (Proxy @Balance) "balance" []

retrieveBalanceTransaction :: (MonadStripe m, StripeResult BalanceTransaction balanceTransaction) => Id BalanceTransaction -> m balanceTransaction
retrieveBalanceTransaction (Id txId) = stripeGet (Proxy @BalanceTransaction) ("balance/history/" <> encodeUtf8 txId) []

listAllBalanceHistory :: (MonadStripe m, StripeResult (List BalanceTransaction) balanceTransactionList) => Pagination BalanceTransaction -> m balanceTransactionList
listAllBalanceHistory = stripeGet (Proxy @(List BalanceTransaction)) "balance/history" . paginationParams
