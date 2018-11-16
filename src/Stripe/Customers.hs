module Stripe.Customers where
import Stripe.Billing.Coupons
import Stripe.Billing.Discounts
import Stripe.Billing.Subscriptions
import Stripe.Core
import Stripe.Utils

data Customer = Customer
  { customerId :: Id Customer
  , customerAccountBalance :: Integer
  , customerCreated :: Timestamp -- TODO timestamp
  , customerCurrency :: Maybe CurrencyCode -- TODO currency code
  , customerDefaultSource :: Maybe Text -- TODO Expandable
  , customerDelinquent :: Bool
  , customerDescription :: Maybe Text
  , customerDiscount :: Maybe Discount
  , customerEmail :: Text
  , customerInvoicePrefix :: Text
  , customerLiveMode :: Bool
  , customerMetadata :: Metadata
  , customerShipping :: Maybe Object -- TODO what
  , customerSources :: List Object
  , customerSubscriptions :: List Subscription
  , customerTaxInfo :: Maybe Object
  , customerTaxInfoVerification :: Maybe Object
  } deriving (Show, Eq, Generic, Typeable)

instance FromJSON Customer where
  parseJSON = parseObject "Customer" $ do
    assertObject "customer"
    Customer
      <$> req "id"
      <*> req "account_balance"
      <*> req "created"
      <*> opt "currency"
      <*> opt "default_source"
      <*> req "delinquent"
      <*> opt "description"
      <*> opt "discount"
      <*> req "email"
      <*> req "invoice_prefix"
      <*> req "livemode"
      <*> req "metadata"
      <*> opt "shipping"
      <*> req "sources"
      <*> req "subscriptions"
      <*> opt "tax_info"
      <*> opt "tax_info_verification"

data NewCustomer = NewCustomer
  { newCustomerAccountBalance :: Maybe Integer
  , newCustomerCoupon :: Maybe (Id Coupon)
  , newCustomerDefaultSource :: Maybe (Id ())
  , newCustomerDescription :: Maybe Text
  , newCustomerEmail :: Maybe Text
  , newCustomerInvoicePrefix :: Maybe Text
  -- , newCustomerMetadata
  -- , newCustomerShipping
  , newCustomerSource :: Maybe Text -- TODO can be lots of things
  -- , newCustomerTaxInfo ::
  } deriving (Show, Eq, Generic, Typeable)

instance ToForm NewCustomer where
  toForm NewCustomer{..} = mconcat
    [ optParam "account_balance" newCustomerAccountBalance
    , optParam "coupon" newCustomerCoupon
    , optParam "default_source" newCustomerDefaultSource
    , optParam "description" newCustomerDescription
    , optParam "email" newCustomerEmail
    , optParam "invoice_prefix" newCustomerInvoicePrefix
    , optParam "source" newCustomerSource
    ]

instance BaseQuery NewCustomer where
  baseQuery = NewCustomer
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing

createCustomer :: (MonadStripe m, StripeResult Customer customer) => NewCustomer -> m customer
createCustomer = stripePost (Proxy @Customer) "customers"

retrieveCustomer :: (MonadStripe m, StripeResult Customer customer) => Id Customer -> m customer
retrieveCustomer (Id customerId) = stripeGet (Proxy @Customer) ("customers/" <> encodeUtf8 customerId) []

data UpdateCustomer = UpdateCustomer
  { updateCustomerAccountBalance :: Maybe Integer
  , updateCustomerCoupon :: Maybe (Id Coupon)
  , updateCustomerDefaultSource :: Maybe (Id ())
  , updateCustomerDescription :: Maybe Text
  , updateCustomerEmail :: Maybe Text
  , updateCustomerInvoicePrefix :: Maybe Text
  {- TODO
  , updateCustomerMetadata
  , updateCustomerShipping
  -}
  , updateCustomerSource :: Maybe Text
  {-
  , updateCustomerTaxInfo
  -}
  } deriving (Show, Eq, Generic, Typeable)

instance ToForm UpdateCustomer where
  toForm UpdateCustomer{..} = mconcat
    [ optParam "account_balance" updateCustomerAccountBalance
    , optParam "coupon" updateCustomerCoupon
    , optParam "default_source" updateCustomerDefaultSource
    , optParam "description" updateCustomerDescription
    , optParam "email" updateCustomerEmail
    , optParam "invoice_prefix" updateCustomerInvoicePrefix
    , optParam "source" updateCustomerSource
    ]

instance BaseQuery UpdateCustomer where
  baseQuery = UpdateCustomer
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing

updateCustomer :: (MonadStripe m, StripeResult Customer customer) => Id Customer -> UpdateCustomer -> m customer
updateCustomer (Id customerId) = stripePost (Proxy @Customer) ("customers/" <> encodeUtf8 customerId)

-- deleteCustomer

listAllCustomers :: (MonadStripe m, StripeResult (List Customer) customerList) => Pagination Customer -> m customerList
listAllCustomers = stripeGet (Proxy @(List Customer)) "customers" . paginationParams

data CreateCustomer
