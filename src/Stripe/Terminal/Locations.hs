module Stripe.Terminal.Locations where
import Stripe.Core
import Stripe.Utils

data Location = Location
  { locationId :: Id Location
  , locationAddress :: Address
  , locationDisplayName :: Text
  } deriving (Show, Eq, Generic, Typeable)

instance FromJSON Location where
  parseJSON = parseObject "Location" $ do
    assertObject "terminal.location"
    Location
      <$> req "id"
      <*> req "address"
      <*> req "display_name"

-- createLocation
-- retrieveLocation
-- updateLocation
-- listAllLocations
