{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where
import Control.Applicative hiding (optional)
import qualified Control.Applicative as App
import Control.Effect.Carrier
import Control.Effect
import Control.Lens hiding (view, use, assign, (.=))
import Control.Effect.Lens (view, use, assign)
import Control.Effect.Fail
import Control.Effect.Reader
import Control.Effect.State
import Control.Monad hiding (fail)
import Control.Lens.TH
import Data.Aeson hiding (Encoding)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Aeson.Types (parseMaybe)
import qualified Data.Attoparsec.Text as P
import Data.List
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe, isJust)
import qualified Data.HashMap.Strict as H
import qualified Data.Set as S
import Data.Either
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Manipulate as T
import qualified Data.Text.Lazy.IO as L
import qualified Data.ByteString.Lazy as BL
import Data.Foldable
import Data.Traversable
import qualified Data.Vector as V
import GHC.Generics (Generic)
import Prelude hiding (fail)
import Text.EDE
import Text.EDE.Filters
import Text.Pandoc.Class
import Text.Pandoc.Extensions
import Text.Pandoc.Options (def, ReaderOptions(..), WriterOptions(..))
import Text.Pandoc.Readers.CommonMark
import Text.Pandoc.Readers.HTML
import Text.Pandoc.Writers.Haddock
import OpenAPI.Types
-- TODO XML support, additional content types


-- DSL layout
-- provide type family to add support for extensions anywhere they are able to be annotated
-- register type
--   * descend into monad stack that supports reading/writing fields, with access to other type details
-- lookup type(s)
-- fail out on unsupported conversions
-- TODO provide a means of resolving references instead of just munging names
type RootSig sig =
  ( Member (State Root) sig
  , Member (State GlobalGeneratorState) sig
  , Member Fail sig
  )

newtype SchemaScope = SchemaScope { fromSchemaScope :: NonEmpty (Text, Schema) }
  deriving (Show, Eq)

schemaScopeNames :: SchemaScope -> NonEmpty Text
schemaScopeNames = fmap fst . fromSchemaScope

rootScope :: Text -> Schema -> SchemaScope
rootScope t s = SchemaScope $ ((t, s) NE.:| [])

data TypedParameter = TypedParameter
  { typedParameterType :: Text
  , typedParameterParam :: Parameter
  } deriving (Show, Eq)

instance ToJSON TypedParameter where
  toJSON TypedParameter{..} = object
    [ "type" .= typedParameterType
    , "param" .= typedParameterParam
    ]

data ResolvedPathSegment = ResolvedPathSegment
  { resolvedPathSegment :: PathSegment
  , resolvedPathSegmentParameter :: Maybe Parameter
  } deriving (Show, Eq)

instance ToJSON ResolvedPathSegment where
  toJSON ResolvedPathSegment{..} = case resolvedPathSegment of
    ConstSegment bs -> object
      [ "type" .= ("const" :: Text)
      , "raw" .= rawSegment resolvedPathSegment
      ]
    NamedSegment bs -> object
      [ "type" .= ("named" :: Text)
      , "param" .= resolvedPathSegmentParameter
      ]

data Endpoint = Endpoint
  { endpointMethod :: Text
  , endpointDescription :: CommonMark
  , endpointName :: Text
  , endpointPath :: Path
  , endpointDeprecated :: Bool
  , endpointParameters :: V.Vector TypedParameter
  , endpointRequestBodyRequired :: Bool
  , endpointRequestBodies :: H.HashMap Text MediaType
  -- Resolves path template segments with given parameters
  , endpointResolvedPath :: [ResolvedPathSegment]
  , endpointResolvedQuery :: V.Vector Parameter
  , endpointResolvedHeaders :: V.Vector Parameter
  , endpointResolvedCookie :: V.Vector Parameter
  -- TODO other useful stuff
  } deriving (Show, Eq, Generic)

instance ToJSON Endpoint

data GlobalGeneratorState = GlobalGeneratorState
  { globalGeneratorStateDeclaredTypes :: H.HashMap Text DataType
  , globalGeneratorStateDeclaredEnums :: S.Set FieldEnum
  , globalGeneratorStateDeclaredEndpoints :: [Endpoint]
  }

data DataType = DataType
  { typeName :: Text
  , constructors :: [Constructor]
  , typeOriginalName :: Text
  , typeDescription :: Maybe Text
  } deriving (Show, Eq, Generic)

instance ToJSON DataType

data Constructor = Constructor
  { constructorName :: Text
  , constructorFields :: [ConstructorField]
  } deriving (Show, Eq, Generic)

instance ToJSON Constructor

data ConstructorField = ConstructorField
  { fieldName :: Text
  , fieldOriginalName :: Text
  , fieldType :: Text
  , fieldNullable :: Bool
  , fieldDescription :: Maybe Text
  } deriving (Show, Eq, Generic)

instance ToJSON ConstructorField

data EnumOption = EnumOption
  { enumOptionName :: Text
  , enumOptionOriginal :: Text
  } deriving (Show, Eq, Ord, Generic)

instance ToJSON EnumOption where
  toJSON EnumOption{..}= object
    [ "name" .= enumOptionName
    , "original" .= enumOptionOriginal
    ]

data FieldEnum = FieldEnum
  { fieldEnumName :: Text
  , fieldEnumOriginal :: Text
  , fieldEnumOptions :: V.Vector EnumOption
  , fieldEnumScope :: NonEmpty Text
  } deriving (Show, Eq, Ord, Generic)

instance ToJSON FieldEnum where
  toJSON FieldEnum{..} = object
    [ "name" .= fieldEnumName
    , "original" .= fieldEnumOriginal
    , "options" .= fieldEnumOptions
    ]

makeFields ''GlobalGeneratorState

emptyGlobalGeneratorState :: GlobalGeneratorState
emptyGlobalGeneratorState = GlobalGeneratorState H.empty mempty mempty


{-
runRootM ::
     (Carrier sig m, RootSig sig) => Root -> m a -> (H.HashMap Text DataType, a)
runRootM r m = run . evalState r . runState (H.empty :: H.HashMap Text DataType) $ m
-}

-- type TypeSig sig = _
-- type ConstructorSig sig = _
-- type FieldSig sig = _
-- type RouteM a = _

componentTopLevelSchemas :: Traversal' Components Schema
componentTopLevelSchemas fa Components{..} = (\s resp param req hdr -> Components s resp param componentsExamples req hdr componentsSecuritySchemes componentsLinks componentsCallbacks)
  <$> (each . _Obj) fa componentsSchemas
  <*> (each . _Obj . content . each . _Obj . schema . _Just . _Obj) fa componentsResponses
  <*> (each . _Obj . schema . _Just . _Obj) fa componentsParameters
  <*> (each . _Obj . content . each . schema . _Just . _Obj) fa componentsRequestBodies
  <*> (each . _Obj . schema . _Just . _Obj) fa componentsHeaders

pathsTopLevelSchemas :: Traversal' (H.HashMap Path (H.HashMap Method ApiEndpoint)) Schema
pathsTopLevelSchemas = each . each . apiEndpointTopLevelSchemas

apiEndpointTopLevelSchemas :: Traversal' ApiEndpoint Schema
apiEndpointTopLevelSchemas fa ApiEndpoint{..} = ApiEndpoint apiEndpointDescription apiEndpointOperationId <$> traverseParams fa apiEndpointParameters <*> traverseRequestBody fa apiEndpointRequestBody <*> traverseResponses fa apiEndpointResponses <*> pure apiEndpointServers <*> pure apiEndpointDeprecated
  where
    traverseParams = each . schema . _Just . _Obj
    traverseRequestBody = content . each . schema . _Just . _Obj
    traverseResponses = responses . each . content . each . _Obj . schema . _Just . _Obj

-- This is maybe still not comprehensive, will need to check.
allTopLevelSchemas :: Traversal' Root Schema
allTopLevelSchemas fa Root{..} = (\c p -> Root rootOpenapi c rootInfo p rootSecurity rootServers rootTags rootExternalDocs)
  <$> componentTopLevelSchemas fa rootComponents
  <*> pathsTopLevelSchemas fa rootPaths

traverseComponentSchemas :: (MonadFail m, Carrier sig m, RootSig sig) => (Text -> Schema -> m a) -> m [a]
traverseComponentSchemas f = do
  r <- get @Root
  forM (r ^. (components . schemas . to H.toList)) $ \(k, v) -> case v of
    Ref (Reference r) -> fail ("Top-level references in the components object not currently supported. Please file a ticket if you need this: " <> show r)
    Obj o -> f k o

traversePaths :: (Carrier sig m, RootSig sig) => (Path -> H.HashMap Method ApiEndpoint -> m a) -> m [a]
traversePaths f = do
  Root{..} <- get
  let pathList = H.toList rootPaths
  forM pathList $ \(k, v) -> f k v


withPath ::
     (Monad m, Carrier sig m, RootSig sig)
  => Path
  -> H.HashMap Method ApiEndpoint
  -> ReaderC Path (ReaderC (H.HashMap Method ApiEndpoint) m) a
  -> m a
withPath p methods m = runReader methods $ runReader p m -- runReader p $ runReader methods m

parWrap :: Text -> Text
parWrap t = "(" <> t <> ")"

data TypeOutputModule
  = TypeOutputModule [Text]
  deriving (Show)

data InstancesVia = InstancesVia Text

haskellRefName :: Reference -> Text
haskellRefName (Reference r) = qualifiedPascal $ last $ T.splitOn "/" r



-- need to carry most specific named typelike thing to use if we need to generate
-- a larger sum type
renderTypeInScope ::
     forall m sig.
     ( MonadFail m
     , Carrier sig m
     , Member Fail sig
     , Member (Reader SchemaScope) sig
     , Member (State GlobalGeneratorState) sig
     )
  => Text
  -> Referenceable Schema
  -> m (Text, Bool)
renderTypeInScope _ (Ref ref@(Reference r)) = pure $ (,) (haskellRefName ref) False
renderTypeInScope fieldName (Obj s) = do
  (SchemaScope parentSchema) <- ask
  case s ^. type_ of
    Nothing -> do
      successfulMatch <-
        firstSuccess
          [ case NE.head parentSchema of
              parent -> if expandable (parent ^. _2) fieldName
                then do
                  rs <- parseExpansionResources fieldName s
                  pure $ Just ("Expandable '[" <> T.intercalate ", " (V.toList $ fmap (\r -> "Const (" <> haskellRefName r <> ")") rs) <> "]", schemaNullable s)
                else pure Nothing
          , if null $ schemaAnyOf s
              then pure Nothing
              else do
                converted <-
                  fmap V.toList $ local (\(SchemaScope ss) -> SchemaScope ((fieldName, s) NE.<| ss)) $ V.mapM (renderHaskellType fieldName) (schemaAnyOf s)
                let allFieldsNullable = all snd converted
                    anyFieldsNullable = any snd converted
                pure $
                  Just $
                  case converted of
                    [(c, nullableCheckInner)] ->
                      (c, nullableCheckInner || schemaNullable s)
                    [(c1, nci1), (c2, nci2)] ->
                      ("Either " <> parWrap (maybeWrap (c1, allFieldsNullable || nci1)) <> " " <>
                       parWrap (maybeWrap (c2, allFieldsNullable || nci2)), anyFieldsNullable)
                    _ ->
                      ( "AnyOf '[" <>
                        T.intercalate ", " (maybeWrap <$> converted) <>
                        "]"
                      , schemaNullable s)
        -- if schemaNullable s or all elements nullable then move Maybe to top level, otherwise maybes at an individual level
          ]
      maybe
        (fail ("No conversion routine found for the schema: " <> show s))
        pure
        successfulMatch
    Just t -> do
      mr <- primSpecialiser
      case mr of
        Nothing -> (,) <$> baseType t <*> pure (schemaNullable s)
        Just r -> pure r
  where
    ret :: Text -> Bool -> (Text, Bool)
    ret = (,)
    primSpecialiser = do
      (SchemaScope parentSchemas) <- ask
      case parentSchemas of
        ((pName, ps) NE.:| _)
          -- TODO extract into plugin thingy
         -> do
          if fieldName == "id"
            then case H.lookup "x-resourceId" (schemaExtensions ps) >>=
                      parseMaybe parseJSON of
                   Nothing -> pure Nothing
                   Just str -> do
                     pure $
                       Just
                         ( "(Id " <> qualifiedPascal str <> ")"
                         , schemaNullable s)
            else pure Nothing
    -- baseType :: Text -> Either String Text
    baseType t =
      maybe
        (fail
           ("Failed to lookup primitive type " <> show t <> " " <>
            show (schemaFormat s)))
        id $
      lookup
        (t, schemaFormat s)
        [ (("integer", Nothing), pure "Int")
        , (("integer", Just "int32"), pure "Int32")
        , (("integer", Just "int64"), pure "Int64")
        , (("integer", Just "unix-time"), pure "POSIXTime")
        , (("number", Nothing), pure "Scientific")
        , (("number", Just "float"), pure "Float")
        , (("number", Just "Double"), pure "Double")
        , (("string", Nothing), case schemaEnum s of
            Nothing -> pure "Text"
            Just vs -> do
              s <- ask
              let mvs' = sequence $ fmap (parseMaybe parseJSON) vs
              case mvs' of
                Nothing -> fail "Type of field is a string, so enum types should also be text"
                Just vs' -> do
                  let immediateParent = case s of
                        (SchemaScope ((fn, _) NE.:| _)) -> ((fn <> ".") <>)
                  -- TODO parent scope for name gen
                  let newEnum = FieldEnum
                        { fieldEnumName = qualifiedPascal $ immediateParent fieldName
                        , fieldEnumOriginal = fieldName
                        , fieldEnumOptions = fmap (\s -> EnumOption (qualifiedPascal (immediateParent fieldName) <> "_" <> qualifiedPascal s) s) vs'
                        , fieldEnumScope = schemaScopeNames s
                        }
                  modify $ \gs -> (gs :: GlobalGeneratorState) & declaredEnums %~ (S.insert newEnum)
                  pure $ fieldEnumName newEnum
          )
        , (("string", Just "byte"), pure "ByteString")
        , (("string", Just "binary"), pure "ByteString")
        , (("string", Just "decimal"), pure "Scientific")
        , (("boolean", Nothing), pure "Bool")
        , ( ("array", Nothing)
          , case schemaItems s of
              Nothing -> pure "Vector Value"
          -- TODO hook into custom type conversions
              Just i -> do
                (innerConv, nullability) <- renderHaskellType fieldName i
                if nullability
                  then pure ("Vector (Maybe (" <> innerConv <> "))")
                  else pure ("Vector (" <> innerConv <> ")"))
        , (("object", Nothing), pure "Object")
        ]

firstSuccess :: Monad m => [m (Maybe a)] -> m (Maybe a)
firstSuccess [] = pure Nothing
firstSuccess (mm : next) = do
  m <- mm
  case m of
    Nothing -> firstSuccess next
    Just _ -> pure m

-- TODO reduce the need for these somewhat by moving things qualified with dots into submodules and importing them qualified
qualifiedPascal :: T.Text -> T.Text
qualifiedPascal = T.intercalate "_" . map T.toPascal . T.splitOn "."

qualifiedCamel :: T.Text -> T.Text
qualifiedCamel = T.intercalate "_" . map T.toCamel . T.splitOn "."

renderComponentSchemaDataTypes :: (MonadFail m, Carrier sig m, RootSig sig) => m ()
renderComponentSchemaDataTypes = do
  -- woot
  modify $ over allTopLevelSchemas annotateSchemaWithEnumFromDescription

  traverseComponentSchemas $ \n fs -> runReader (SchemaScope $ pure (n, fs)) $ do
    let cfn = qualifiedPascal n
    ctorFields <- sequence $ map (constructorFieldFromSchemaProperty n) $ H.toList $ schemaProperties fs
    let r = DataType
              { typeName = qualifiedPascal n
              , typeOriginalName = n
              , typeDescription = fromCommonMark <$> schemaDescription fs
              , constructors =
                [ Constructor
                  { constructorName = cfn
                  , constructorFields = sortOn fieldOrderProjection ctorFields
                  }
                ]
              }
    modify $ \rs -> (rs :: GlobalGeneratorState) & declaredTypes %~ H.insert n r
    pure r
  return ()
  -- TODO component requests
  -- TODO component responses
  -- TODO endpoint requests
  -- TODO endpoint responses

  where
    constructorFieldFromSchemaProperty rawTypeName (n, s) = do
      (tn, nullability) <- renderHaskellType n s
      pure $ ConstructorField
        { fieldName = qualifiedCamel rawTypeName <> "__" <> qualifiedCamel n
        , fieldOriginalName = n
        , fieldType = tn
        , fieldNullable = nullability
        , fieldDescription = case s of
            Obj s' -> fromCommonMark <$> schemaDescription s'
            Ref _ -> Nothing
        }

renderApiEndpoints :: (MonadFail m, Carrier sig m, RootSig sig) => m ()
renderApiEndpoints = do
  result <- traversePaths $ \route methods ->
    forM (H.toList methods) $ \(method, ApiEndpoint{..}) -> do
      typeResolvedParams <- forM apiEndpointParameters $ \param -> case parameterSchema  param of
        Nothing -> fail "Parameters using the 'content' field are not yet supported. Please file an issue if you need this."
        Just rs -> do
          (t, _) <- runReader rootScope $ renderHaskellType (parameterName param) rs
          pure $ TypedParameter t param

      --- TODO combine into one pass
      let pathParams =
            V.filter ((== ParameterInPath) . parameterIn_) apiEndpointParameters
          queryParams =
            V.filter ((== ParameterInQuery) . parameterIn_) apiEndpointParameters
          headerParams =
            V.filter ((== ParameterInHeader) . parameterIn_) apiEndpointParameters
          cookieParams =
            V.filter ((== ParameterInCookie) . parameterIn_) apiEndpointParameters

      pathPieces <- forM (pathSegments route) $ \piece -> case piece of
        ConstSegment _ -> pure $ ResolvedPathSegment piece Nothing
        NamedSegment n -> case find (\p -> parameterName p == n) pathParams of
          Nothing -> fail ("Unable to locate parameter " <> show n <> " for path " <> show piece)
          Just found -> pure $ ResolvedPathSegment piece $ Just found

      pure $ Endpoint
        { endpointMethod = method
        , endpointDescription = apiEndpointDescription
        , endpointName = apiEndpointOperationId
        , endpointPath = route
        , endpointDeprecated = apiEndpointDeprecated
        , endpointParameters = typeResolvedParams
        , endpointRequestBodyRequired = requestBodyRequired apiEndpointRequestBody
        , endpointRequestBodies = requestBodyContent apiEndpointRequestBody
        , endpointResolvedPath = pathPieces
        , endpointResolvedQuery = queryParams
        , endpointResolvedHeaders = headerParams
        , endpointResolvedCookie = cookieParams
        }
  assign @GlobalGeneratorState declaredEndpoints $ concat result


readSpec :: IO Root
readSpec = do
  ef <- eitherDecodeFileStrict' "openapi/openapi/spec3.json"
  case ef of
    Left err -> fail err
    Right ok -> pure ok

main :: IO ()
main = readSpec >>= go


commonMarkToHaddock :: Text -> Text
commonMarkToHaddock mark = case runPure (readCommonMark commonMarkDefs mark >>= writeHaddock haddockDefs) of
  Left err -> error $ show err
  Right ok -> ok
  where
    commonMarkDefs =
      def { readerExtensions = enableExtension Ext_raw_html $ getDefaultExtensions "commonmark" }
    haddockDefs =
      def { writerExtensions = getDefaultExtensions "haddock" }

maybeWrap :: (Text, Bool) -> Text
maybeWrap (t, isMaybe) = if isMaybe
  then "Maybe (" <> t <> ")"
  else t

refSchemaNullability :: Referenceable Schema -> Bool
refSchemaNullability (Ref _) = False
refSchemaNullability (Obj s) = schemaNullable s

-- Result is used to specify ordering: Int is absolute sorting index to bubble
-- any of these values to the top. Bool is nullability, text is field name.
fieldOrderProjection :: ConstructorField -> Either Int (Bool, Text)
fieldOrderProjection ConstructorField{..} = case fieldOriginalName of
  "id" -> Left (0 :: Int)
  "object" -> Left 1
  "livemode" -> Left 2
  "deleted" -> Left 3
  _ -> Right (fieldNullable, fieldOriginalName)


data ContentTypeHandler = ContentTypeHandler
  { contentTypeShorthandSuffix :: Text
  } deriving (Show, Eq)

instance ToJSON ContentTypeHandler where
  toJSON c = object [ "suffix" .= contentTypeShorthandSuffix c ]

builtInContentTypes :: H.HashMap Text ContentTypeHandler
builtInContentTypes = H.fromList
  [ ("application/json", ContentTypeHandler "Json")
  , ("application/octet-stream", ContentTypeHandler "Binary")
  , ("text/xml", ContentTypeHandler "Xml")
  , ("text/html", ContentTypeHandler "Html")
  , ("application/x-www-form-urlencoded", ContentTypeHandler "Form")
  , ("multipart/form-data", ContentTypeHandler "MultiPart")
  ]

data OpenApiContext = OpenApiContext
  { supportedContentTypes :: H.HashMap Text ContentTypeHandler
  }

defaultContext :: OpenApiContext
defaultContext = OpenApiContext builtInContentTypes
{-
  { typeConversions :: Referenceable Schema -> Either String (Text, Bool)
  , fieldSorter :: (Text, Referenceable Schema) -> (Text, Referenceable Schema) -> Ordering
  }


-- | The rule must be capable of examining the schema and comprehensively generating the type.
addConversionRule :: (Referenceable Schema -> Either String (Text, Bool)) -> Context -> Context
addConversionRule rule Context{..} = Context
  { typeConversions = \s -> case rule s of
      -- TODO this discards the error
      Left _ -> typeConversions s
      Right out -> Right out
  , fieldSorter = fieldSorter
  }

-}

instance ToJSON a => Quote (Maybe a)

-- TODO add support for custom adjustedTypes + patching over types with more specific values
go :: Root -> IO ()
go r = do
  let res = run $ runFail $ evalState r $ execState emptyGlobalGeneratorState $ do
              renderComponentSchemaDataTypes
              renderApiEndpoints

  let gs :: GlobalGeneratorState
      gs = either error id res

  -- print $ H.keys $ globalGeneratorStateDeclaredEnums gs
  typeTemplate <- eitherParseFile "template/types.ede"
  -- forM_ (gatheredTypeModules mod) $ \(modulePath, types, endpoints) -> do
  let env = fromPairs
        [ "types" .= (gs ^. declaredTypes)
        , "enums" .= (gs ^. declaredEnums)
        , "endpoints" .= (gs ^. declaredEndpoints)
        , "moduleName" .= T.intercalate "." ["Stripe", "Types"] -- (libraryPrefix : modulePath)
        ]

  either error (L.writeFile "src/Stripe/Types.hs") $ typeTemplate >>= (\t -> eitherRenderWith customFilters t env)

  routeTemplate <- eitherParseFile "template/routes.ede"
  either error (L.writeFile "src/Stripe/Endpoints.hs") $ routeTemplate >>= (\t -> eitherRenderWith customFilters t env)
  where
    customFilters = H.fromList
      [ "commonMarkToHaddock" @: commonMarkToHaddock
      , "lookupContentTypeSuffix" @: (\t -> fmap contentTypeShorthandSuffix $ H.lookup t builtInContentTypes)
      ]



-- Extension helpers

expandable :: Schema -> Text -> Bool
expandable parentSchema fieldName = case H.lookup "x-expandableFields" (schemaExtensions parentSchema)  >>= parseMaybe parseJSON of
  Nothing -> False
  Just expandableFields -> V.elem fieldName expandableFields

parseExpansionResources :: MonadFail m => Text -> Schema -> m (V.Vector Reference)
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

-- TODO skip prior stuff
-- approx regex: '[Oo]ne of:? (`([A-Za-z0-9_-]+`(, (or)?))+'
enumFromDescription ::
     Schema -- ^ Info including description
  -> Maybe (V.Vector Text) -- ^ Enum Values
enumFromDescription s = case schemaDescription s of
  Nothing -> Nothing
  Just (CommonMark desc) -> case P.parseOnly enumDescriptionParser desc of
    Left _ -> Nothing
    Right ok -> Just $ V.fromList ok

enumDescriptionParser :: P.Parser [Text]
enumDescriptionParser = do
  void $ P.manyTill P.anyChar oneOfPattern
  P.sepBy1 (P.char '`' *> P.takeWhile1 (/= '`') <* P.char '`') commaOr
  where
    oneOfPattern =
      void ((P.char 'O' <|> P.char 'o') *> P.string "ne of" *> App.optional (P.char ':') *> P.char ' ') <|>
      void ((P.char 'C' <|> P.char 'c') *> P.string "an be either ")
    commaOr = ((P.string ", ") *> App.optional (P.string "or ")) <|> (Just <$> P.string " or ")
