module Stripe.Balance where
import Data.Aeson

data BalanceTransaction
instance FromJSON BalanceTransaction
instance Show BalanceTransaction
instance Eq BalanceTransaction