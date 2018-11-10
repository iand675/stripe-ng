module Stripe.Terminal.ConnectionTokens where

import Stripe.Utils

data ConnectionToken = ConnectionToken
  { connectionTokenSecret :: Text
  } deriving (Show, Eq, Generic, Typeable)

instance FromJSON ConnectionToken where
  parseJSON = parseObject "ConnectionToken" $ do
    assertObject "terminal.connection_token"
    ConnectionToken <$> req "secret"

data CreateConnectionToken

-- createConnectionToken
