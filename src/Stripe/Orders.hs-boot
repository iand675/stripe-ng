module Stripe.Orders where
import Data.Aeson

data Order

data ShippingDetails
instance FromJSON ShippingDetails
instance Show ShippingDetails
instance Eq ShippingDetails
