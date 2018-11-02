module Stripe.Refunds where
import Data.Aeson

data Refund
instance FromJSON Refund
instance Eq Refund
instance Show Refund