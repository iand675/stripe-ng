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

retrieveSubscriptionItem :: (StripeMonad m) => Id SubscriptionItem -> m SubscriptionItem
retrieveSubscriptionItem (Id subscriptionItem) =
  jsonGet ("subscription_items/" <> encodeUtf8 subscriptionItem) []

-- updateSubscriptionItem
-- deleteSubscriptionItem

listAllSubscriptionItems ::
     (StripeMonad m)
  => Id Subscription
  -> Pagination SubscriptionItem
  -> m (List SubscriptionItem)
listAllSubscriptionItems (Id subscriptionId) ps =
  jsonGet
    "subscription_items"
    (("subscription", Just $ encodeUtf8 subscriptionId) : paginationParams ps)
