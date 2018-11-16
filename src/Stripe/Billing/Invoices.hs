module Stripe.Billing.Invoices where
import Stripe.Charges
import Stripe.Core
import Stripe.Customers
import Stripe.Billing.Discounts
import Stripe.Billing.Subscriptions
import Stripe.Billing.SubscriptionItems
import Stripe.Billing.Plans
import Stripe.Utils

data InvoiceLinePeriod = InvoiceLinePeriod
  { invoiceLinePeriodStart :: Timestamp
  , invoiceLinePeriodEnd :: Timestamp
  } deriving (Show, Eq, Generic, Typeable)

instance FromJSON InvoiceLinePeriod where
  parseJSON = parseObject "InvoiceLinePeriod" $ do
    InvoiceLinePeriod
      <$> req "start"
      <*> req "end"

data InvoiceLine = InvoiceLine
  { invoiceLineId :: Id InvoiceLine
  , invoiceLineAmount :: Integer
  , invoiceLineCurrency :: CurrencyCode
  , invoiceLineDescription :: Text
  , invoiceLineDiscountable :: Bool
  , invoiceLineInvoiceItem :: Maybe Text
  , invoiceLineLiveMode :: Bool
  , invoiceLineMetadata :: Metadata
  , invoiceLinePeriod :: InvoiceLinePeriod
  , invoiceLinePlan :: Plan
  , invoiceLineProration :: Bool
  , invoiceLineQuantity :: Int
  , invoiceLineSubscription :: Id Subscription
  , invoiceLineSubscriptionItem :: Id SubscriptionItem
  , invoiceLineType_ :: Text
  } deriving (Show, Eq, Generic, Typeable)

instance FromJSON InvoiceLine where
  parseJSON = parseObject "InvoiceLine" $ do
    assertObject "line_item"
    InvoiceLine
      <$> req "id"
      <*> req "amount"
      <*> req "currency"
      <*> req "description"
      <*> req "discountable"
      <*> opt "invoice_item"
      <*> req "livemode"
      <*> req "metadata"
      <*> req "period"
      <*> req "plan"
      <*> req "proration"
      <*> req "quantity"
      <*> req "subscription"
      <*> req "subscription_item"
      <*> req "type"

data BillingReason
  = SubscriptionCycle
  | SubscriptionUpdate
  | SubscriptionChangeOrPeriodAdvancement
  | Manual
  | Upcoming
  deriving (Show, Eq, Generic, Typeable)

instance FromJSON BillingReason where
  parseJSON = withText "BillingReason" $ \t -> case t of
    "subscription_cycle" -> pure SubscriptionCycle
    "subscription_update" -> pure SubscriptionUpdate
    "subscription" -> pure SubscriptionChangeOrPeriodAdvancement
    "manual" -> pure Manual
    "upcoming" -> pure Upcoming
    _ -> fail ("Invalid BillingReason: " ++ show t)

data Invoice = Invoice
  { invoiceId :: Id Invoice
  , invoiceAmountDue :: Word
  , invoiceAmountPaid :: Word
  , invoiceAmountRemaining :: Word
  , invoiceApplicationFee :: Maybe Word
  , invoiceAttemptCount :: Word
  , invoiceAttempted :: Bool
  , invoiceAutoAdvance :: Bool
  , invoiceBilling :: ChargeMode
  , invoiceBillingReason :: BillingReason
  , invoiceCharge :: Maybe (Expandable Charge)
  , invoiceClosed :: Maybe Bool
  , invoiceCurrency :: CurrencyCode
  , invoiceCustomer :: Id Customer
  , invoiceDate :: Timestamp
  , invoiceDescription :: Text
  , invoiceDiscount :: Maybe Discount
  , invoiceDueDate :: Maybe Timestamp
  , invoiceEndingBalance :: Maybe Integer
  , invoiceForgiven :: Maybe Bool
  , invoiceHostedInvoicePaymentPending :: Maybe Bool
  , invoiceHostedInvoiceUrl :: Maybe Text
  , invoiceInvoicePdf :: Maybe Text
  , invoiceLines :: List InvoiceLine
  , invoiceLivemode :: Bool
  , invoiceMetadata :: Metadata
  , invoiceNextPaymentAttempt :: Maybe Timestamp
  , invoiceNumber :: Text
  , invoicePaid :: Bool
  , invoicePeriodEnd :: Timestamp
  , invoicePeriodStart :: Timestamp
  , invoiceReceiptNumber :: Maybe Text
  , invoiceStartingBalance :: Integer
  , invoiceStatementDescriptor :: Maybe Text
  , invoiceSubscription :: Id Subscription
  , invoiceSubtotal :: Word
  , invoiceTax :: Integer
  , invoiceTaxPercent :: Maybe Double
  , invoiceTotal :: Word
  , invoiceWebhooksDeliveredAt :: Timestamp
  } deriving (Show, Eq, Generic, Typeable)

instance FromJSON Invoice where
  parseJSON = parseObject "Invoice" $ do
    assertObject "invoice"
    Invoice
      <$> req "id"
      <*> req "amount_due"
      <*> req "amount_paid"
      <*> req "amount_remaining"
      <*> opt "application_fee"
      <*> req "attempt_count"
      <*> req "attempted"
      <*> req "auto_advance"
      <*> req "billing"
      <*> req "billing_reason"
      <*> req "charge"
      <*> opt "closed"
      <*> req "currency"
      <*> req "customer"
      <*> req "date"
      <*> req "description"
      <*> opt "discount"
      <*> opt "due_date"
      <*> opt "ending_balance"
      <*> opt "forgiven"
      <*> opt "hosted_invoice_payment_pending"
      <*> opt "hosted_invoice_url"
      <*> opt "invoice_pdf"
      <*> req "lines"
      <*> req "livemode"
      <*> req "metadata"
      <*> opt "next_payment_attempt"
      <*> req "number"
      <*> req "paid"
      <*> req "period_end"
      <*> req "period_start"
      <*> opt "receipt_number"
      <*> req "starting_balance"
      <*> opt "statement_descriptor"
      <*> req "subscription"
      <*> req "subtotal"
      <*> req "tax"
      <*> opt "tax_percent"
      <*> req "total"
      <*> req "webhooks_delivered_at"

{-
createInvoice
retrieveInvoice
updateInvoice
payInvoice
retrieveInvoiceLineItems
retrieveUpcomingInvoice
retrieveUpcomingInvoiceLineItems
listAllInvoices
-}
