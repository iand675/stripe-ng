module OpenAPI.TemplateHelpers where

import Data.Aeson
import Data.Text (Text)
import qualified Data.HashMap.Strict as H
import Text.EDE
import Text.EDE.Filters
import Text.Pandoc.Class
import Text.Pandoc.Extensions
import Text.Pandoc.Options (def, ReaderOptions(..), WriterOptions(..))
import Text.Pandoc.Readers.CommonMark
import Text.Pandoc.Readers.HTML
import Text.Pandoc.Writers.Haddock

import OpenAPI.Gen.Coders
import OpenAPI.Gen (predFromPattern)
import OpenAPI.Types (MT)

instance ToJSON a => Quote (Maybe a)
instance Unquote MT

makeStandardTemplateHelpers :: Coders -> H.HashMap Id Term
makeStandardTemplateHelpers coders =
  H.fromList
    [ "commonMarkToHaddock" @: commonMarkToHaddock
    , "contentTypeSuffix" @:
      (\t -> coderSuffix <$> resolveCoder coders t)

    , "patternPredicate" @: predFromPattern
    ]

commonMarkToHaddock :: Text -> Text
commonMarkToHaddock mark = case runPure (readCommonMark commonMarkDefs mark >>= writeHaddock haddockDefs) of
  Left err -> error $ show err
  Right ok -> ok
  where
    commonMarkDefs =
      def { readerExtensions = enableExtension Ext_raw_html $ getDefaultExtensions "commonmark" }
    haddockDefs =
      def { writerExtensions = getDefaultExtensions "haddock" }
