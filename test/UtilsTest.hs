module UtilsTest where

import qualified Data.HashMap.Strict as HM
import Stripe.Billing.Subscriptions
import Stripe.Utils
import Test.Tasty.Hspec

spec_arrayParams :: Spec
spec_arrayParams = do
  specify "should output nested fields in some order" $ do
    let expected = Form $ HM.fromList [("foo[]", ["1", "2", "3"])]
    arrayParams "foo" [1 :: Int, 2, 3] `shouldBe` expected

spec_indexedArrayFormParams :: Spec
spec_indexedArrayFormParams = do
  specify "should output nested fields appropriately" $ do
    let (Form actual) = indexedArrayFormParams "items" [NewSubscriptionItem "solo-monthly" Nothing]
        expected = HM.fromList
          [ ("items[0][plan]", ["solo-monthly"])
          , ("items[0][quantity]", [])
          ]
    actual `shouldBe` expected
