module Stripe.FilesLinks where
import Data.Aeson

data FileLink
instance FromJSON FileLink
instance Show FileLink
instance Eq FileLink