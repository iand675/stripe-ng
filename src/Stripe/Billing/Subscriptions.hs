module Stripe.Billing.Subscriptions where
import Stripe.Billing.Coupons
import {-# SOURCE #-} Stripe.Billing.Discounts
import Stripe.Billing.Plans
import {-# SOURCE #-} Stripe.Billing.SubscriptionItems
import Stripe.Core
import {-# SOURCE #-} Stripe.Customers
import Data.List.NonEmpty (NonEmpty)
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
  , subscriptionDefaultSource :: Maybe (Expandable ())
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
      <*> opt "default_source"

data NewSubscriptionItem = NewSubscriptionItem
  { newSubscriptionItemPlan :: Id Plan
  , newSubscriptionItemQuantity :: Maybe Int
  }

instance ToForm NewSubscriptionItem where
  toForm NewSubscriptionItem{..} = mconcat
    [ reqParam "plan" newSubscriptionItemPlan
    , optParam "quantity" newSubscriptionItemQuantity
    ]

data NewSubscription = NewSubscription
  { newSubscriptionCustomer :: Id Customer
  , newSubscriptionItems :: NonEmpty NewSubscriptionItem
  , newSubscriptionCoupon :: Maybe (Id Coupon)
  }

instance ToForm NewSubscription where
  toForm NewSubscription{..} = mconcat
    [ reqParam "customer" newSubscriptionCustomer
    , indexedArrayFormParams "items" newSubscriptionItems
    , optParam "coupon" newSubscriptionCoupon
    ]

newSubscription :: Id Customer -> NonEmpty NewSubscriptionItem -> NewSubscription
newSubscription cid xs = NewSubscription
  { newSubscriptionCustomer = cid
  , newSubscriptionItems = xs
  , newSubscriptionCoupon = Nothing
  }

createSubscription :: (MonadStripe m, StripeResult Subscription subscription) => NewSubscription -> m subscription
createSubscription = stripePost (Proxy @Subscription) "subscriptions"

retrieveSubscription :: (MonadStripe m, StripeResult Subscription subscription) => Id Subscription -> m subscription
retrieveSubscription (Id subscriptionId) = stripeGet (Proxy @Subscription) ("subscriptions/" <> encodeUtf8 subscriptionId) []

data UpdateSubscriptionItem = UpdateSubscriptionItem
  { updateSubscriptionItemId :: Maybe (Id SubscriptionItem)
  , updateSubscriptionItemClearUsage :: Maybe Bool
  , updateSubscriptionItemDeleted :: Maybe Bool
  , updateSubscriptionItemMetadata :: Metadata
  , updateSubscriptionItemPlan :: Maybe (Id Plan)
  , updateSubscriptionItemQuantity :: Maybe Int
  }

instance BaseQuery UpdateSubscriptionItem where
  baseQuery = UpdateSubscriptionItem
    Nothing
    Nothing
    Nothing
    mempty
    Nothing
    Nothing

instance ToForm UpdateSubscriptionItem where
  toForm UpdateSubscriptionItem{..} = mconcat
    [ optParam "id" updateSubscriptionItemId
    , optParam "clear_usage" updateSubscriptionItemClearUsage
    , optParam "deleted" updateSubscriptionItemDeleted
    , hashParams "metadata" updateSubscriptionItemMetadata
    , optParam "plan" updateSubscriptionItemPlan
    , optParam "quantity" updateSubscriptionItemQuantity
    ]

data UpdateSubscription = UpdateSubscription
  { -- updateSubscriptionCustomer :: Id Customer
    updateSubscriptionItems :: [UpdateSubscriptionItem]
  , updateSubscriptionCoupon :: Maybe (Id Coupon)
  }

subscriptionChanges :: UpdateSubscription
subscriptionChanges = UpdateSubscription
  { -- updateSubscriptionCustomer = cid
    updateSubscriptionItems = []
  , updateSubscriptionCoupon = Nothing
  }

instance ToForm UpdateSubscription where
  toForm UpdateSubscription{..} = mconcat
    [ -- reqParam "customer" updateSubscriptionCustomer
      indexedArrayFormParams "items" updateSubscriptionItems
    , optParam "coupon" updateSubscriptionCoupon
    ]

unsetCoupon :: Id Coupon
unsetCoupon = Id ""

updateSubscription :: (MonadStripe m, StripeResult Subscription subscription) => Id Subscription -> UpdateSubscription -> m subscription
updateSubscription (Id subscriptionId) = stripePost (Proxy @Subscription) ("subscriptions/" <> encodeUtf8 subscriptionId)

cancelSubscription :: (MonadStripe m, StripeResult Subscription subscription) => Id Subscription -> m subscription
cancelSubscription (Id subscriptionId) = stripeDelete (Proxy @Subscription) ("subscriptions/" <> encodeUtf8 subscriptionId) []

listSubscriptions :: (MonadStripe m, StripeResult (List Subscription) subscriptionList) => Pagination Subscription -> m subscriptionList
listSubscriptions = stripeGet (Proxy @(List Subscription)) "subscriptions" . paginationParams


-- Utils

existingSubscriptionItemUpdate :: SubscriptionItem -> UpdateSubscriptionItem
existingSubscriptionItemUpdate SubscriptionItem{..} = baseQuery
  { updateSubscriptionItemId = Just subscriptionItemId
  , updateSubscriptionItemPlan = Just $ planId subscriptionItemPlan
  }
