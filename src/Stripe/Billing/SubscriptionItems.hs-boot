module Stripe.Billing.SubscriptionItems where
import Data.Aeson

data SubscriptionItem
instance FromJSON SubscriptionItem
instance Show SubscriptionItem
instance Eq SubscriptionItem