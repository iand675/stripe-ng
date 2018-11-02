module Stripe.Billing.Subscriptions where
import {-# SOURCE #-} Stripe.Billing.Discounts
import Stripe.Billing.Plans
import {-# SOURCE #-} Stripe.Billing.SubscriptionItems
import {-# SOURCE #-} Stripe.Customers
import Stripe.Utils

data ChargeMode
  = ChargeAutomatically
  | SendInvoice
  deriving (Show, Eq, Generic, Typeable)

instance FromJSON ChargeMode where
  parseJSON = withText "ChargeMode" $ \t -> case t of
    "charge_automatically" -> pure ChargeAutomatically
    "send_invoice" -> pure SendInvoice
    _ -> fail ("Invalid ChargeMode: " ++ show t)

data SubscriptionStatus
  = Trialing
  | Active
  | PastDue
  | Canceled
  | Unpaid
  deriving (Show, Eq, Generic, Typeable)

instance FromJSON SubscriptionStatus where
  parseJSON = withText "SubscriptionStatus" $ \t -> case t of
    "trialing" -> pure Trialing
    "active" -> pure Active
    "past_due" -> pure PastDue
    "canceled" -> pure Canceled
    "unpaid" -> pure Unpaid
    _ -> fail ("Invalid SubscriptionStatus: " ++ show t)

data Subscription = Subscription
  { subscriptionId :: Id Subscription
  , subscriptionApplicationFeePercent :: Maybe Double
  , subscriptionBilling :: ChargeMode
  , subscriptionBillingCycleAnchor :: Timestamp
  , subscriptionCancelAtPeriodEnd :: Bool
  , subscriptionCanceledAt :: Maybe Timestamp
  , subscriptionCreated :: Timestamp
  , subscriptionCurrentPeriodEnd :: Timestamp
  , subscriptionCurrentPeriodStart :: Timestamp
  , subscriptionCustomer :: Expandable Customer
  , subscriptionDaysUntilDue :: Maybe Integer
  , subscriptionDiscount :: Maybe Discount
  , subscriptionEndedAt :: Maybe Timestamp
  , subscriptionItems :: List SubscriptionItem
  , subscriptionLiveMode :: Bool
  , subscriptionMetadata :: Metadata
  , subscriptionPlan :: Plan
  , subscriptionQuantity :: Maybe Int
  , subscriptionStart :: Timestamp
  , subscriptionStatus :: SubscriptionStatus
  , subscriptionTaxPercent :: Maybe Double
  , subscriptionTrialEnd :: Maybe Timestamp
  , subscriptionTrialStart :: Maybe Timestamp
  } deriving (Show, Eq, Generic, Typeable)

instance FromJSON Subscription where
  parseJSON = parseObject "Subscription" $ do
    assertObject "subscription"
    Subscription
      <$> req "id"
      <*> opt "application_fee_percent"
      <*> req "billing"
      <*> req "billing_cycle_anchor"
      <*> req "cancel_at_period_end"
      <*> opt "canceled_at"
      <*> req "created"
      <*> req "current_period_end"
      <*> req "current_period_start"
      <*> req "customer"
      <*> opt "days_until_due"
      <*> opt "discount"
      <*> opt "ended_at"
      <*> req "items"
      <*> req "livemode"
      <*> req "metadata"
      <*> req "plan"
      <*> opt "quantity"
      <*> req "start"
      <*> req "status"
      <*> opt "tax_percent"
      <*> opt "trial_end"
      <*> opt "trial_start"

-- createSubscription

retrieveSubscription :: (StripeMonad m) => Id Subscription -> m Subscription
retrieveSubscription (Id subscriptionId) = jsonGet ("subscriptions/" <> encodeUtf8 subscriptionId) []

-- updateSubscription
-- cancelSubscription

listSubscriptions :: (StripeMonad m) => Pagination Subscription -> m (List Subscription)
listSubscriptions = jsonGet "subscriptions" . paginationParams
