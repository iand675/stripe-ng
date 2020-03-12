module ProductTest where

import Test.Tasty.Hspec

import Stripe.Billing.Products
import Stripe.Utils
import Stripe.Balance
import qualified Stripe.Core as Stripe
import qualified Stripe.Customers as Stripe
import qualified Stripe.Errors as Stripe
import qualified Stripe.Utils as Stripe (enumerate, baseQuery)

import Conduit
import Control.Lens hiding (simple)
import Control.Monad.Trans (lift)
import Data.Aeson
import qualified Conduit as C
import qualified Data.Conduit.List as C
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Stripe.Lens as S

testProduct :: NewProduct
testProduct = NewProduct
  Nothing
  "test product"
  Service
  Nothing
  []
  Nothing
  mempty
  Nothing
  Nothing

spec_create_product :: Spec
spec_create_product = do
  return ()
{-
  specify "service product" $ do
    p <- stripeWithEnv $ createProduct testProduct
    productName p `shouldBe` ("test product" :: Text)
  specify "product metadata" $ do
    (createdProduct, retrievedProduct) <- stripeWithEnv $ do
      p <- createProduct $ testProduct { newProductName = "product metadata test" , newProductMetadata = H.fromList [("bip", "zoop")]}

      p' <- retrieveProduct $ productId p
      pure (p, p')
    productMetadata retrievedProduct `shouldBe` H.fromList [("bip", "zoop")]
-}

spec_list_all_products :: Spec
spec_list_all_products = do
  specify "baseQuery" $ do
    x <- stripeWithEnv $ C.runConduit
      (Stripe.enumerate (listAllProducts Stripe.baseQuery) .| C.consume)
    print x

  specify "active services" $ do
    x <- stripeWithEnv $ C.runConduit
      (Stripe.enumerate
          (listAllProducts Stripe.baseQuery
            { listAllProductsQueryActive = Just True
            , listAllProductsQueryType_ = Just Service
            }
          )
      .| C.consume
      )
    print x
