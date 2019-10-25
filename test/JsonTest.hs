module JsonTest where

import Data.Aeson
import OpenAPI.Types
import Test.Tasty.Hspec

{-
xspec_openapi_decode_then_encode_is_idempotent :: Spec
xspec_openapi_decode_then_encode_is_idempotent = it "can roundtrip idempotently" $ do
  -- A real blob
  (Right jsonBlob) <- eitherDecodeFileStrict' "openapi/openapi/spec3.json"
  -- The blob as Root
  (Right theRoot) <- eitherDecodeFileStrict' "openapi/openapi/spec3.json"
  toJSON (theRoot :: Root) `shouldBe` jsonBlob
-}
