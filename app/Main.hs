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
import Control.Lens hiding (view, use, assign, (.=), (<.>))
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
import qualified Data.Map as M
import Data.Hashable (Hashable)
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
import System.Directory
import System.FilePath
import Text.EDE
import Text.EDE.Filters

import OpenAPI.Gen
import OpenAPI.Gen.Coders
import OpenAPI.Gen.Identifier
import OpenAPI.Gen.Stripe
import OpenAPI.Gen.Reader
import OpenAPI.Gen.Writer
import OpenAPI.TemplateHelpers
import OpenAPI.Types


main :: IO ()
main = readSpec >>= go

-- TODO provide a means of resolving references instead of just munging names
type RootSig sig =
  ( Member (State Root) sig
  , Member (State GlobalGeneratorState) sig
  , Member Fail sig
  )

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
withPath p methods m = runReader methods $ runReader p m

data TypeOutputModule
  = TypeOutputModule [Text]
  deriving (Show)

renderComponentSchemaDataTypes ::
     (MonadFail m, Carrier sig m, RootSig sig) => TypeResolvers (ReaderC SchemaScope m) -> m ()
renderComponentSchemaDataTypes resolvers = do
  modify $ over allSchemas annotateSchemaWithEnumFromDescription

  r <- get
  traverseComponentSchemas $ \n fs -> runReader (rootScope $ SchemaParentComponents n $ rootComponents r) $ do
    let raw = scopedFromDots n
        cfn = rewriteScoped T.toPascal T.toPascal raw
    ctorFields <- sequence $ map (constructorFieldFromSchemaProperty raw) $ H.toList $ schemaProperties fs
    let r = DataType
              { typeName = fromUnscopedIdent $ scopedToLocal cfn
              , typeOriginalName = n
              , typeDescription = fromCommonMark <$> schemaDescription fs
              , constructors =
                [ Constructor
                  { constructorName = fromUnscopedIdent $ scopedToLocal cfn
                  , constructorFields = sortOn fieldOrderProjection ctorFields
                  }
                ]
              , codings = includeCoders standardCoders [MT "application/json"]
              }
    modify $ \rs -> (rs :: GlobalGeneratorState) & declaredTypes %~ insertIdent cfn r
    pure r
  return ()
  -- TODO component requests
  -- TODO component responses
  -- TODO endpoint requests
  -- TODO endpoint responses

  where
    constructorFieldFromSchemaProperty rawTypeName (n, s) = do
      t <- renderType resolvers {- n -} s
      pure $ ConstructorField
        { fieldName = T.toCamel (fromUnscopedIdent $ scopedToLocal rawTypeName) <> T.toPascal n
        , fieldOriginalName = n
        , fieldType = T.pack $ show t
        , fieldNullable = isNullable t
        , fieldDescription = case s of
            Obj s' -> fromCommonMark <$> schemaDescription s'
            Ref _ -> Nothing
        }

renderApiEndpoints ::
     ( MonadFail m
     , Carrier sig m
     , RootSig sig
     )
  => TypeResolvers (ReaderC SchemaScope m) -> m ()
renderApiEndpoints resolvers = do
  result <- traversePaths $ \route methods ->
    forM (H.toList methods) $ \(method, ApiEndpoint{..}) -> do
      typeResolvedParams <- forM apiEndpointParameters $ \param -> case parameterSchema  param of
        Nothing -> fail "Parameters using the 'content' field are not yet supported. Please file an issue if you need this."
        Just rs -> do
          t <- runReader (rootScope $ SchemaParentParameter (parameterName param) param) $
               renderType resolvers {- (parameterName param) -} rs
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

      let nestedLists =
            H.toList $
            fmap (\r -> M.toList $ responseContent r) $
            (apiEndpointResponses ^. responses)
          searchSpace = do
            (pat, resps) <- nestedLists
            (mediaTy, ref) <- resps
            -- TODO need to resolve this
            mt <- toList ref
            -- TODO need to handle encoding
            s <- toList $ mediaTypeSchema mt
            pure (s, mt, \rt -> OpenAPI.Gen.Response
                                { responseContentType = mediaTy
                                , responsePattern = pat
                                , responsePatternPredicate = predFromPattern pat
                                , responseSuffix = prettyResponseSuffix (Just mediaTy) pat
                                , responseType = rt
                                })
      flatResponses <- forM searchSpace $ \(s, mt, rf) ->
        runReader (rootScope $ SchemaParentMediaType mt) (rf <$> renderType resolvers s)

      -- We need [(status, [(contentType, {status, content, suffix})])]
      let groupedByPatResponses = groupBy (\r1 r2 -> responsePattern r1 == responsePattern r2) flatResponses
      -- Unsafe, but meh.
          nestedResponses = H.fromList $ map (\l -> (responsePattern $ head l, NestedResponse apiEndpointOperationId $ M.fromList $ map (\r -> (responseContentType r, r)) l)) groupedByPatResponses

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
        , endpointFlatResponses = flatResponses
        , endpointNestedResponses = nestedResponses
        }
  assign @GlobalGeneratorState declaredEndpoints $ concat result



-- TODO add support for custom adjustedTypes + patching over types with more specific values
go :: Root -> IO ()
go r = do
  let resolvers = (stripeEnhancedResolvers <> builtInTypeResolvers)
  let res =
        run $
        runFail $
        evalState r $
        execState emptyGlobalGeneratorState $ do
          renderComponentSchemaDataTypes resolvers
          renderApiEndpoints resolvers
  let gs :: GlobalGeneratorState
      gs = either error id res
  -- print $ H.keys $ globalGeneratorStateDeclaredEnums gs
  typeTemplate <- eitherParseFile "template/types.ede"
  forM_ (gs ^. declaredTypes . to groupedIdentList) $ \(modulePath, types) -> do
    let basePath = [UnscopedIdent "Stripe", UnscopedIdent "Types"]
    let modulePath' = ModuleIdent $ (++ reverse basePath) $ fromModuleIdent modulePath
    let filePath = "src" </> (T.unpack $ T.intercalate "/" $ moduleIdentPieces modulePath') <.> "hs"
    createDirectoryIfMissing True (takeDirectory filePath)
    let env =
          fromPairs
            [ "types" .= map snd (sortOn fst types)
            , "enums" .= map snd (sortOn fst $ H.toList $ identsInScope (gs ^. declaredEnums) modulePath)
            -- , "endpoints" .= (gs ^. declaredEndpoints)
            , "moduleName" .= modulePath'
            ]
    BL.writeFile (show modulePath' <> ".json") $ encodePretty env
    either error (L.writeFile filePath) $
      typeTemplate >>= (\t -> eitherRenderWith customFilters t env)

  BL.writeFile "Stripe.Endpoints.json" (gs ^. declaredEndpoints . to encodePretty)
  routeTemplate <- eitherParseFile "template/routes.ede"
  let env = fromPairs [ "endpoints" .= (gs ^. declaredEndpoints) ]
  either error (L.writeFile "src/Stripe/Endpoints.hs") $
    routeTemplate >>= (\t -> eitherRenderWith customFilters t env)
{-
  either error (L.writeFile "src/Stripe/Types.hs") $
    typeTemplate >>= (\t -> eitherRenderWith customFilters t env)
-}
  where
    customFilters = makeStandardTemplateHelpers standardCoders
