module OpenAPI.Gen.Stripe where

import Control.Applicative
import Control.Effect
import Control.Effect.Reader
import Control.Effect.Fail (MonadFail)
import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import qualified Data.Attoparsec.Text as P
import Data.Text (Text)
import qualified Data.Text.Manipulate as T
import qualified Data.HashMap.Strict as H
import qualified Data.List.NonEmpty as NE
import qualified Data.Vector as V

import OpenAPI.Types hiding (optional)
import OpenAPI.Gen
import OpenAPI.Gen.Identifier

-- Extension helpers

expandable :: Schema -> Text -> Bool
expandable parentSchema fieldName =
  case maybeExpandableFields of
    Nothing -> False
    Just expandableFields -> V.elem fieldName expandableFields
  where
    maybeExpandableFields = do
      expandableFieldsJson <-
        H.lookup "x-expandableFields" (schemaExtensions parentSchema)
      parseMaybe parseJSON expandableFieldsJson

parseExpansionResources :: MonadFail m =>
     Text
  -> Schema
  -> m (V.Vector (Reference Schema))
parseExpansionResources n s = case H.lookup "x-expansionResources" (schemaExtensions s) of
  Nothing -> if V.null $ schemaAnyOf s
    then fail "Don't know how to expand a field that doesn\'t provide anyOf"
    else pure $ V.concatMap deref $ schemaAnyOf s
  Just rs -> case parseMaybe (withObject "x-expansionResources" (.: "oneOf")) rs of
    Nothing -> fail "Unable to parse x-expansionResources into references"
    Just ok -> pure ok
  where
    deref (Ref r) = V.singleton r
    deref _ = V.empty

annotateSchemaWithEnumFromDescription :: Schema -> Schema
annotateSchemaWithEnumFromDescription s = s
  { schemaEnum = schemaEnum s <|> (fmap toJSON <$> enumFromDescription s)
  }

-- approx regex: '[Oo]ne of:? (`([A-Za-z0-9_-]+`(, (or)?))+'
enumFromDescription ::
     Schema -- ^ Info including description
  -> Maybe (V.Vector Text) -- ^ Enum Values
enumFromDescription s = do
  (CommonMark desc) <- schemaDescription s
  case P.parseOnly enumDescriptionParser desc of
    Left _ -> Nothing
    Right ok -> pure $ V.fromList ok

enumDescriptionParser :: P.Parser [Text]
enumDescriptionParser = do
  void $ P.manyTill P.anyChar oneOfPattern
  P.sepBy1 (P.char '`' *> P.takeWhile1 (/= '`') <* P.char '`') commaOr
  where
    oneOfPattern =
      void ((P.char 'O' <|> P.char 'o') *> P.string "ne of" *> optional (P.char ':') *> P.char ' ') <|>
      void ((P.char 'C' <|> P.char 'c') *> P.string "an be either ")
    commaOr = ((P.string ", ") *> optional (P.string "or ")) <|> (Just <$> P.string " or ")

friendlyFieldOrder :: ConstructorField -> ConstructorField -> Ordering
friendlyFieldOrder cf1 cf2 = compare (fieldOrderProjection cf1) (fieldOrderProjection cf2)

-- Result is used to specify ordering: Int is absolute sorting index to bubble
-- any of these values to the top. Bool is nullability, text is field name.
fieldOrderProjection :: ConstructorField -> Either Int (Bool, Text)
fieldOrderProjection ConstructorField{..} = case fieldOriginalName of
  "id" -> Left (0 :: Int)
  "object" -> Left 1
  "livemode" -> Left 2
  "deleted" -> Left 3
  _ -> Right (fieldNullable, fieldOriginalName)

stripeEnhancedResolvers ::
     (Carrier sig m, SchemaDescent sig, Monad m)
  => TypeResolvers m
stripeEnhancedResolvers =
  TypeResolvers
    { typeResolvers =
        [ \rs s -> do
            p <- (NE.head . fromSchemaScope) <$> ask
            pure $ case p of
              SchemaParentSchemaProperty "id" referencingSchema ->
                case H.lookup "x-resourceId" (schemaExtensions referencingSchema) >>= parseMaybe parseJSON of
                  Nothing -> Nothing
                  Just str -> Just $
                    dischargeNullable (schemaNullable s) $
                    wrap1 (stdAnn $ localScope $ UnscopedIdent "Id") $
                    (RenderedType $ pure $ stdAnn $ localScope $ UnscopedIdent $ T.toPascal str)
              _ -> Nothing
        ]
    }
