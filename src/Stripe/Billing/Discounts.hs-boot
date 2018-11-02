module Stripe.Billing.Discounts where
import Data.Aeson

data Discount
instance FromJSON Discount
instance Show Discount
instance Eq Discount