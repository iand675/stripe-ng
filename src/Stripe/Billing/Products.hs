module Stripe.Billing.Products where

import Stripe.Utils

data PackageDimensions = PackageDimensions
  { packageDimensionsHeight :: Double
  , packageDimensionsLength :: Double
  , packageDimensionsWeight :: Double
  , packageDimensionsWidth :: Double
  } deriving (Show, Eq, Generic, Typeable)

instance FromJSON PackageDimensions where
  parseJSON = parseObject "PackageDimensions" $ do
    PackageDimensions
      <$> req "height"
      <*> req "length"
      <*> req "weight"
      <*> req "width"

data ProductType
  = Good
  | Service
  deriving (Show, Eq, Generic, Typeable)

instance FromJSON ProductType where
  parseJSON = withText "ProductType" $ \t -> case t of
    "good" -> pure Good
    "service" -> pure Service
    _ -> fail ("Invalid ProductType: " ++ show t)

data Product = Product
  { productId :: Id Product
  , productActive :: Bool
  , productAttributes :: [Text]
  , productCaption :: Maybe Text
  , productCreated :: Timestamp
  , productDeactivateOn :: [Id Application]
  , productDescription :: Maybe Text
  , productImages :: [Text]
  , productLiveMode :: Bool
  , productMetadata :: Metadata
  , productName :: Text
  , productPackageDimensions :: Maybe PackageDimensions
  , productShippable :: Maybe Bool
  , productStatementDescriptor :: Maybe Text
  , productType_ :: ProductType
  , productUnitLabel :: Maybe Text
  , productUpdated :: Maybe Timestamp
  , productUrl :: Maybe Text
  } deriving (Show, Eq, Generic, Typeable)

instance FromJSON Product where
  parseJSON = parseObject "Product" $ do
    assertObject "product"
    Product
      <$> req "id"
      <*> req "active"
      <*> req "attributes"
      <*> opt "caption"
      <*> req "created"
      <*> req "deactivate_on"
      <*> req "description"
      <*> req "images"
      <*> req "livemode"
      <*> req "metadata"
      <*> req "name"
      <*> opt "package_dimensions"
      <*> opt "shippable"
      <*> opt "statement_descriptor"
      <*> req "type"
      <*> opt "unit_label"
      <*> opt "updated"
      <*> opt "url"

-- createProduct

retrieveProduct :: (StripeMonad m) => Id Product -> m Product
retrieveProduct (Id productId) = jsonGet ("products/" <> encodeUtf8 productId) []

-- updateProduct

listAllProducts :: (StripeMonad m) => Pagination Product -> m (List Product)
listAllProducts = jsonGet "products" . paginationParams

-- deleteProduct
