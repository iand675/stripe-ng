module Stripe.Billing.InvoiceItems where
import Stripe.Core
import Stripe.Customers
import Stripe.Billing.Invoices
import Stripe.Billing.Plans
import Stripe.Billing.Subscriptions
import Stripe.Billing.SubscriptionItems
import Stripe.Utils

data InvoiceItem = InvoiceItem
  { invoiceItemId :: Id InvoiceItem
  , invoiceItemAmount :: Integer
  , invoiceItemCurrency :: CurrencyCode
  , invoiceItemCustomer :: Expandable Customer
  , invoiceItemDate :: Timestamp
  , invoiceItemDescription :: Text
  , invoiceItemDiscountable :: Bool
  , invoiceItemInvoice :: Expandable Invoice
  , invoiceItemLiveMode :: Bool
  , invoiceItemMetadata :: Metadata
  , invoiceItemPeriod :: InvoiceLinePeriod
  , invoiceItemPlan :: Plan
  , invoiceItemProration :: Bool
  , invoiceItemQuantity :: Int
  , invoiceItemSubscription :: Expandable Subscription
  , invoiceItemSubscriptionItem :: Id SubscriptionItem
  , invoiceItemUnitAmount :: Integer
  } deriving (Show, Eq, Generic, Typeable)

instance FromJSON InvoiceItem where
  parseJSON = parseObject "InvoiceItem" $ do
    assertObject "invoiceitem"
    InvoiceItem
      <$> req "id"
      <*> req "amount"
      <*> req "currency"
      <*> req "customer"
      <*> req "date"
      <*> req "description"
      <*> req "discountable"
      <*> req "invoice"
      <*> req "livemode"
      <*> req "metadata"
      <*> req "period"
      <*> req "plan"
      <*> req "proration"
      <*> req "quantity"
      <*> req "subscription"
      <*> req "subscription_item"
      <*> req "unit_amount"

{-
createInvoiceItem
retrieveInvoiceItem
updateInvoiceItem
deleteInvoiceItem
listAllInvoiceItems
-}
