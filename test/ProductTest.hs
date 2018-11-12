module ProductTest where
import Control.Lens hiding (simple)
import qualified Data.HashMap.Strict as H
import qualified Stripe.Lens as S
import Stripe.Billing.Products
import Stripe.Utils
import Test.Tasty.Hspec
import qualified Data.Vector as V

testProduct :: NewProduct
testProduct = NewProduct Nothing "test product" Service Nothing [] Nothing mempty Nothing Nothing

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
