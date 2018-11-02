module Stripe.Customers where
import Stripe.Billing.Discounts
import Stripe.Billing.Subscriptions
import Stripe.Utils

data Customer = Customer
  { customerId :: Id Customer
  , customerAccountBalance :: Integer
  , customerCreated :: Timestamp -- TODO timestamp
  , customerCurrency :: CurrencyCode -- TODO currency code
  , customerDefaultSource :: Text -- TODO Expandable
  , customerDelinquent :: Bool
  , customerDescription :: Maybe Text
  , customerDiscount :: Maybe Discount
  , customerEmail :: Maybe Text
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
      <*> req "currency"
      <*> req "default_source"
      <*> req "delinquent"
      <*> opt "description"
      <*> opt "discount"
      <*> opt "email"
      <*> req "invoice_prefix"
      <*> req "livemode"
      <*> req "metadata"
      <*> opt "shipping"
      <*> req "sources"
      <*> req "subscriptions"
      <*> opt "tax_info"
      <*> opt "tax_info_verification"

{-
data NewCustomer = NewCustomer
  { newCustomerAccountBalance
  , newCustomerCOupon
  , newCustomerDefaultSource
  , newCustomerDescription
  , newCustomerEmail
  , newCustomerInvoicePrefix
  , newCustomerMetadata
  , newCustomerShipping
  , newCustomerSource
  , newCustomerTaxInfo
  }

data UpdateCustomer = UpdateCustomer
  { updateCustomerAccountBalance
  , updateCustomerCoupon
  , updateCustomerDefaultSource
  , updateCustomerDescription
  , updateCustomerEmail
  , updateCustomerInvoicePrefix
  , updateCustomerMetadata
  , updateCustomerShipping
  , updateCustomerSource
  , updateCustomerTaxInfo
  }
-}

-- createCustomer

retrieveCustomer :: (StripeMonad m) => Id Customer -> m Customer
retrieveCustomer (Id customerId) = jsonGet ("customers/" <> encodeUtf8 customerId) []

-- updateCustomer
-- deleteCustomer

listAllCustomers :: (StripeMonad m) => Pagination Customer -> m (List Customer)
listAllCustomers = jsonGet "customers" . paginationParams
