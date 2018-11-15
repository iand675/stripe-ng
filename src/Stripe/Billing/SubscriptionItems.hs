module Stripe.Billing.SubscriptionItems where
import Stripe.Billing.Plans
import {-# SOURCE #-} Stripe.Billing.Subscriptions
import Stripe.Utils

data SubscriptionItem = SubscriptionItem
  { subscriptionItemId :: Id SubscriptionItem
  , subscriptionItemCreated :: Timestamp
  , subscriptionItemMetadata :: Metadata
  , subscriptionItemPlan :: Plan
  , subscriptionItemQuantity :: Maybe Word
  , subscriptionItemSubscription :: Id Subscription
  } deriving (Show, Eq, Generic, Typeable)

instance FromJSON SubscriptionItem where
  parseJSON = parseObject "SubscriptionItem" $ do
    assertObject "subscription_item"
    SubscriptionItem
      <$> req "id"
      <*> req "created"
      <*> req "metadata"
      <*> req "plan"
      <*> opt "quantity"
      <*> req "subscription"

-- createSubscriptionItem

retrieveSubscriptionItem :: (MonadStripe m, StripeResult SubscriptionItem subscriptionItem) => Id SubscriptionItem -> m subscriptionItem
retrieveSubscriptionItem (Id subscriptionItem) =
  stripeGet (Proxy @SubscriptionItem) ("subscription_items/" <> encodeUtf8 subscriptionItem) []

-- updateSubscriptionItem
-- deleteSubscriptionItem

listAllSubscriptionItems ::
     (MonadStripe m, StripeResult (List SubscriptionItem) subscriptionItemList)
  => Id Subscription
  -> Pagination SubscriptionItem
  -> m subscriptionItemList
listAllSubscriptionItems (Id subscriptionId) ps =
  stripeGet
    (Proxy @(List SubscriptionItem))
    "subscription_items"
    (("subscription", Just $ encodeUtf8 subscriptionId) : paginationParams ps)
