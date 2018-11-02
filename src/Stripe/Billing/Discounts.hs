module Stripe.Billing.Discounts where
import {-# SOURCE #-} Stripe.Customers
import Stripe.Billing.Coupons
import Stripe.Billing.Subscriptions
import Stripe.Utils

data Discount = Discount
  { discountCoupon :: Coupon
  , discountCustomer :: Id Customer
  , discountEnd :: Maybe Timestamp
  , discountStart :: Timestamp
  , discountSubscription :: Maybe (Id Subscription)
  } deriving (Show, Eq, Generic, Typeable)

instance FromJSON Discount where
  parseJSON = parseObject "Discount" $ do
    assertObject "discount"
    Discount
      <$> req "coupon"
      <*> req "customer"
      <*> opt "end"
      <*> req "start"
      <*> opt "subscription"

-- deleteCustomerDiscount
-- deleteSubscriptionDiscount
