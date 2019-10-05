module OpenAPI.Gen.Reader where
import Data.Aeson
import OpenAPI.Types

readSpec :: IO Root
readSpec = do
  ef <- eitherDecodeFileStrict' "openapi/openapi/spec3.json"
  case ef of
    Left err -> fail err
    Right ok -> pure ok
