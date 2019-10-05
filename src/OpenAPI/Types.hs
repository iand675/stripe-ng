{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
module OpenAPI.Types where
import Control.Applicative hiding (optional)
import qualified Control.Applicative as App
import Control.Lens hiding ((.=))
import Control.Lens.Plated
import Control.Lens.TH
import Control.Monad.State.Strict
import Data.Aeson hiding (Encoding)
import qualified Data.Aeson.Encoding as E
import Data.Aeson.Types hiding (Encoding)
import qualified Data.Attoparsec.Text as Parse
import qualified Data.ByteString.Char8 as C
import Data.Char (digitToInt)
import Data.Hashable
import Data.Maybe (catMaybes, fromMaybe)
import Data.String
import Data.Text (Text)
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Text.EDE as EDE
import qualified Text.EDE.Filters as EDE
import Data.Vector (Vector)
import qualified Data.Vector as V
import GHC.Generics (Generic)

newtype PathTemplate = PathTemplate { pathSegments :: [PathSegment] }
  deriving (Eq, Ord, Hashable, Generic)

instance Show PathTemplate where
  show = show . mconcat . map (T.unpack . prettySegment) . pathSegments

instance IsString PathTemplate where
  fromString s = case parsePath $ T.pack s of
    Left e -> error e
    Right ok -> ok

data PathSegment
  = ConstSegment Text
  | NamedSegment Text
  deriving (Show, Eq, Generic, Ord)

instance Hashable PathSegment
instance ToJSONKey PathTemplate where
  toJSONKey = ToJSONKeyText
    (mconcat . map prettySegment . pathSegments)
    (E.text . mconcat . map prettySegment . pathSegments)
instance FromJSONKey PathTemplate where
  fromJSONKey = FromJSONKeyTextParser (either fail pure . parsePath)

prettySegment :: PathSegment -> Text
prettySegment (ConstSegment t) = "/" <> t
prettySegment (NamedSegment t) = "/{" <> t <> "}"

rawSegment :: PathSegment -> Text
rawSegment (ConstSegment t) = t
rawSegment (NamedSegment t) = t

parsePath :: Text -> Either String PathTemplate
parsePath = Parse.parseOnly $ fmap PathTemplate $ many (Parse.char '/' *> (parseNamed <|> parseConst))
  where
    parseConst = ConstSegment <$> Parse.takeWhile (/= '/')
    parseNamed = NamedSegment <$> (Parse.char '{' *> Parse.takeWhile1 (/= '}') <* Parse.char '}')

instance ToJSON PathTemplate where
  toJSON = toJSON . mconcat . map prettySegment . pathSegments
  toEncoding = E.text . mconcat . map prettySegment . pathSegments

instance FromJSON PathTemplate where
  parseJSON = withText "Path" $ \t -> case parsePath t of
    Left err -> fail err
    Right ok -> pure ok



killer :: StateT Object Parser a -> Object -> Parser a
killer p o = do
  (x, rem) <- runStateT p o
  if not (H.null rem)
    then fail $ show rem
    else pure x

require :: FromJSON a => Text -> StateT Object Parser a
require t = do
  o <- get
  r <- lift $ o .: t
  put $ H.delete t o
  pure r

optional :: FromJSON a => Text -> StateT Object Parser (Maybe a)
optional t = do
  o <- get
  r <- lift $ o .:? t
  put $ H.delete t o
  pure r

defaultOption :: FromJSON a => a -> Text -> StateT Object Parser a
defaultOption def t = fromMaybe def <$> optional t

takeExtensions :: StateT Object Parser (H.HashMap Text Value)
takeExtensions = do
  o <- get
  let extensions = H.filterWithKey (\k _ -> "x-" `T.isPrefixOf` k) o
  let rest = H.difference o extensions
  put rest
  return extensions

newtype CommonMark = CommonMark { fromCommonMark :: Text }
  deriving (Show, Eq, ToJSON, FromJSON)


type Path = PathTemplate
type Method = Text

data Root = Root
  { rootOpenapi :: Text
  , rootComponents :: Components
  , rootInfo :: Info
  , rootPaths :: H.HashMap Path (H.HashMap Method ApiEndpoint)
  , rootSecurity :: Vector Object
  , rootServers :: Vector Server
  , rootTags :: Vector Object
  , rootExternalDocs :: Maybe Object
  } deriving (Show, Eq)

instance FromJSON Root where
  parseJSON = withObject "Root" $ killer $ do
    rootOpenapi <- require "openapi"
    rootInfo <- require "info"
    rootServers <- fromMaybe V.empty <$> optional "servers"
    rootPaths <- require "paths"
    rootComponents <- require "components"
    rootSecurity <- require "security"
    rootTags <- fromMaybe V.empty <$> optional "tags"
    rootExternalDocs <- optional "externalDocs"
    pure $ Root{..}

data Info = Info
  { infoTitle :: Text
  , infoDescription :: Maybe CommonMark
  , infoTermsOfService :: Maybe Text
  , infoContact :: Maybe Contact
  , infoLicense :: Maybe License
  , infoVersion :: Text
  , infoExtensions :: H.HashMap Text Value
  } deriving (Show, Eq)

instance FromJSON Info where
  parseJSON = withObject "Info" $ killer $ do
    infoTitle <- require "title"
    infoDescription <- optional "description"
    infoTermsOfService <- optional "termsOfService"
    infoContact <- optional "contact"
    infoLicense <- optional "license"
    infoVersion <- require "version"
    infoExtensions <- takeExtensions
    pure $ Info{..}

data Contact = Contact
  { contactName :: Maybe Text
  , contactUrl :: Maybe Text
  , contactEmail :: Maybe Text
  , contactExtensions :: H.HashMap Text Value
  } deriving (Show, Eq)

instance FromJSON Contact where
  parseJSON = withObject "Contact" $ killer $ do
    contactName <- optional "name"
    contactUrl <- optional "url"
    contactEmail <- optional "email"
    contactExtensions <- takeExtensions
    pure Contact{..}

data License = License
  { licenseName :: Text
  , licenseUrl :: Maybe Text
  , licenseExtensions :: H.HashMap Text Value
  } deriving (Show, Eq)

instance FromJSON License where
  parseJSON = withObject "License" $ killer $ do
    licenseName <- require "name"
    licenseUrl <- optional "url"
    licenseExtensions <- takeExtensions
    pure License{..}

newtype Reference a = Reference Text
  deriving (Show, Eq, Ord, Functor)

instance ToJSON (Reference a) where
  toJSON (Reference r) = object [ "$ref" .= r ]

instance FromJSON (Reference a) where
  parseJSON = withObject "Reference" $ killer $ Reference <$> require "$ref"

data Referenceable a
  = Ref (Reference a)
  | Obj a
  deriving (Show, Eq, Functor, Ord)

instance Foldable Referenceable where
  foldr f z (Ref r) = z
  foldr f z (Obj a) = f a z

instance Traversable Referenceable where
  traverse f (Ref (Reference r)) = pure $ Ref $ Reference r
  traverse f (Obj a) = Obj <$> f a

instance ToJSON a => ToJSON (Referenceable a) where
  toJSON (Ref r) = toJSON r
  toJSON (Obj a) = toJSON a

instance FromJSON a => FromJSON (Referenceable a) where
  parseJSON v = (Ref <$> parseJSON v) <|> (Obj <$> parseJSON v)

type RefMap a = H.HashMap Text (Referenceable a)
data Components = Components
  { componentsSchemas :: RefMap Schema
  , componentsResponses :: RefMap Response
  , componentsParameters :: RefMap Parameter
  , componentsExamples :: RefMap Example
  , componentsRequestBodies :: RefMap RequestBody
  , componentsHeaders :: RefMap Header
  , componentsSecuritySchemes :: RefMap SecurityScheme
  , componentsLinks :: RefMap Link
  , componentsCallbacks :: RefMap Callback
  } deriving (Show, Eq)

instance FromJSON Components where
  parseJSON = withObject "Components" $ killer $ do
    componentsSchemas <- defaultOption H.empty "schemas"
    componentsResponses <- defaultOption H.empty "responses"
    componentsParameters <- defaultOption H.empty "parameters"
    componentsExamples <- defaultOption H.empty "examples"
    componentsRequestBodies <- defaultOption H.empty "requestBodies"
    componentsHeaders <- defaultOption H.empty "headers"
    componentsSecuritySchemes <- defaultOption H.empty "securitySchemes"
    componentsLinks <- defaultOption H.empty "links"
    componentsCallbacks <- defaultOption H.empty "callbacks"
    pure Components{..}

type Extensions = H.HashMap Text Value

data ParameterIn
  = ParameterInQuery
  | ParameterInHeader
  | ParameterInPath
  | ParameterInCookie
  deriving (Show, Eq)

instance ToJSON ParameterIn where
  toJSON = \case
    ParameterInQuery -> "query"
    ParameterInHeader -> "header"
    ParameterInPath -> "path"
    ParameterInCookie -> "cookie"

instance FromJSON ParameterIn where
  parseJSON = withText "ParameterIn" $ \case
    "query" -> pure ParameterInQuery
    "header" -> pure ParameterInHeader
    "path" -> pure ParameterInPath
    "cookie" -> pure ParameterInCookie
    str -> fail (show str <> " is not a valid value for the \"in\" parameter")

data ParameterStyle
  = ParameterStyleMatrix
  | ParameterStyleLabel
  | ParameterStyleForm
  | ParameterStyleSimple
  | ParameterStyleSpaceDelimited
  | ParameterStylePipeDelimited
  | ParameterStyleDeepObject
  deriving (Show, Eq)

instance ToJSON ParameterStyle where
  toJSON = \case
    ParameterStyleMatrix -> "matrix"
    ParameterStyleLabel -> "label"
    ParameterStyleForm -> "form"
    ParameterStyleSimple -> "simple"
    ParameterStyleSpaceDelimited -> "spaceDelimited"
    ParameterStylePipeDelimited -> "pipeDelimited"
    ParameterStyleDeepObject -> "deepObject"

instance FromJSON ParameterStyle where
  parseJSON = withText "ParameterStyle" $ \case
    "matrix" -> pure ParameterStyleMatrix
    "label" -> pure ParameterStyleLabel
    "form" -> pure ParameterStyleForm
    "simple" -> pure ParameterStyleSimple
    "spaceDelimited" -> pure ParameterStyleSpaceDelimited
    "pipeDelimited" -> pure ParameterStylePipeDelimited
    "deepObject" -> pure ParameterStyleDeepObject
    str -> fail (show str <> " is not a valid value for the \"style\" parameter")

data Parameter = Parameter
  { parameterName :: Text
  , parameterIn_ :: ParameterIn
  , parameterDescription :: Maybe CommonMark
  , parameterRequired :: Bool
  , parameterDeprecated :: Bool
  , parameterAllowEmptyValue :: Bool
  -- Can do schema and style, or content to increase flexibility
  , parameterStyle :: Maybe ParameterStyle
  , parameterExplode :: Bool
  , parameterAllowReserved :: Bool
  , parameterSchema :: Maybe (Referenceable Schema)
  , parameterExample :: Maybe Value
  , parameterExamples :: RefMap Example
  , parameterContent :: H.HashMap Text MediaType
  , parameterExtensions :: H.HashMap Text Value
  } deriving (Show, Eq)

instance ToJSON Parameter where
  toJSON Parameter{..} = object
    [ "name" .= parameterName
    , "in" .= parameterIn_
    , "description" .= parameterDescription
    , "required" .= parameterRequired
    , "deprecated" .= parameterDeprecated
    , "allowEmptyValue" .= parameterAllowEmptyValue
    , "style" .= parameterStyle
    , "explode" .= parameterExplode
    , "allowReserved" .= parameterAllowReserved
    , "schema" .= parameterSchema
    , "example" .= parameterExample
    , "examples" .= parameterExamples
    , "content" .= parameterContent
    ]

instance FromJSON Parameter where
  parseJSON = withObject "Parameter" $ killer $ do
    parameterName <- require "name"
    parameterIn_ <- require "in"
    parameterDescription <- optional "description"
    parameterRequired' <- optional "required"
    parameterRequired <- case parameterIn_ of
      ParameterInPath -> if (parameterRequired' == Just True)
        then pure True
        else fail "{\"in\": \"path\"} parameters MUST specify \"required\": true."
      _ -> pure $ fromMaybe False parameterRequired'
    parameterDeprecated <- defaultOption False "deprecated"
    parameterAllowEmptyValue <- defaultOption False "allowEmptyValue"
    parameterStyle <- optional "style"
    let explodeDefault = case parameterStyle of
          Just ParameterStyleForm -> True
          _ -> False
    parameterExplode <- defaultOption explodeDefault "explode"
    parameterAllowReserved <- defaultOption False "allowReserved"
    parameterSchema <- optional "schema"
    parameterExample <- optional "example"
    parameterExamples <- defaultOption H.empty "examples"
    parameterContent <- defaultOption H.empty "content"
    parameterExtensions <- takeExtensions
    pure Parameter{..}

type Header = Parameter

data SecuritySchemeType
  = SecuritySchemeAPIKey
  | SecuritySchemeHTTP
  | SecuritySchemeOAuth2
  | SecuritySchemeOpenIDConnect
  deriving (Show, Eq)

instance FromJSON SecuritySchemeType where
  parseJSON = withText "SecuritySchemeType" $ \case
    "apiKey" -> pure SecuritySchemeAPIKey
    "http" -> pure SecuritySchemeHTTP
    "oauth2" -> pure SecuritySchemeOAuth2
    "openIdConnect" -> pure SecuritySchemeOpenIDConnect
    str -> fail ("Invalid Security Scheme type: " <> show str)

data APIKeyLocation
  = APIKeyLocationQuery
  | APIKeyLocationHeader
  | APIKeyLocationCookie
  deriving (Show, Eq)

instance FromJSON APIKeyLocation where
  parseJSON = withText "APIKeyLocation" $ \case
    "query" -> pure APIKeyLocationQuery
    "header" -> pure APIKeyLocationHeader
    "cookie" -> pure APIKeyLocationCookie
    str -> fail ("Invalid API Key location: " <> show str)

{-
data OAuthFlows = OAuthFlows
  { oauthFlowsImplicit :: Maybe ImplicitFlow
  , oauthFlowsPassword :: Maybe PasswordFlow
  , oauthFlowsClientCredentials :: Maybe CredentialsFlow
  , oauthFlowsAuthorizationCode :: Maybe AuthorizationCodeFlow
  } deriving (Show, Eq)

data ImplicitFlow = ImplicitFlow
  { implicitFlowAuthorizationUrl :: Text
  , implicitFlowTokenUrl :: Text
  , implicitFlowRefreshUrl :: Maybe Text
  , implicitFlowScopes :: H.HashMap Text Text
  , implicitFlowExten
  } deriving (Show, Eq)

data PasswordFlow = PasswordFlow
  { passwordFlowTokenUrl :: Text
  , passwordFlowRefreshUrl ::
  }
-}
type OAuthFlows = Object

data SecurityScheme = SecurityScheme
  { securitySchemeType_ :: SecuritySchemeType
  , securitySchemeDescription :: Maybe CommonMark
  , securitySchemeName :: Maybe Text
  , securitySchemeIn_ :: Maybe APIKeyLocation
  , securitySchemeScheme :: Maybe Text
  , securitySchemeBearerFormat :: Maybe Text
  , securitySchemeFlows :: Maybe OAuthFlows
  , securitySchemeOpenIdConnectUrl :: Maybe Text
  , securitySchemeExtensions :: H.HashMap Text Value
  } deriving (Show, Eq)

instance FromJSON SecurityScheme where
  parseJSON = withObject "SecurityScheme" $ killer $ do
    securitySchemeType_ <- require "type"
    securitySchemeDescription <- optional "description"
    securitySchemeName <- optional "name"
    securitySchemeIn_ <- optional "in"
    securitySchemeScheme <- optional "scheme"
    securitySchemeBearerFormat <- optional "bearerFormat"
    securitySchemeFlows <- optional "flows"
    securitySchemeOpenIdConnectUrl <- optional "openIdConnectUrl"
    securitySchemeExtensions <- takeExtensions
    pure SecurityScheme{..}

type Expression = Text

data Link = Link
  { linkOperationRef :: Maybe Text
  , linkOperationId :: Maybe Text
  , linkParameters :: H.HashMap Text (Either Expression Value)
  , linkRequestBody :: Maybe (Either Expression Value)
  , linkDescription :: Maybe CommonMark
  , linkServer :: Maybe Server
  , linkExtensions :: Extensions
  } deriving (Show, Eq)

instance ToJSON Link where
  toJSON Link{..} = object
    [ "operationRef" .= linkOperationRef
    , "operationId" .= linkOperationId
    , "parameters" .= linkParameters
    , "requestBody" .= linkRequestBody
    , "description" .= linkDescription
    , "server" .= linkServer
    ]

instance FromJSON Link where
  parseJSON = withObject "Link" $ killer $ do
    linkOperationRef <- optional "operationRef"
    linkOperationId <- optional "operationId"
    linkParameters <- defaultOption H.empty "parameters"
    linkRequestBody <- optional "requestBody"
    linkDescription <- optional "description"
    linkServer <- optional "server"
    linkExtensions <- takeExtensions
    pure Link{..}

type Callback = Object

-- type Schema = Object
data Schema = Schema
  { schemaProperties :: RefMap Schema
  , schemaTitle :: Maybe Text
  , schemaDescription :: Maybe CommonMark
  , schemaType_ :: Maybe Text
  , schemaRequired :: Vector Text
  , schemaAnyOf :: Vector (Referenceable Schema)
  , schemaItems :: Maybe (Referenceable Schema) -- MUST be present if type is array
  , schemaMinLength :: Maybe Word
  , schemaMaxLength :: Maybe Word
  , schemaFormat :: Maybe Text
  , schemaEnum :: Maybe (Vector Value)
  , schemaAdditionalProperties :: Maybe AdditionalProperties
  , schemaPattern :: Maybe Text
  , schemaNullable :: Bool
  , schemaReadOnly :: Maybe Bool
  , schemaWriteOnly :: Maybe Bool
  , schemaExtensions :: Extensions
  } deriving (Show, Eq)

emptySchema :: Schema
emptySchema = Schema
  { schemaProperties = mempty
  , schemaTitle = Nothing
  , schemaDescription = Nothing
  , schemaType_ = Nothing
  , schemaRequired = mempty
  , schemaAnyOf = mempty
  , schemaItems = Nothing
  , schemaMinLength = Nothing
  , schemaMaxLength = Nothing
  , schemaFormat = Nothing
  , schemaEnum = Nothing
  , schemaAdditionalProperties = Nothing
  , schemaPattern = Nothing
  , schemaNullable = False
  , schemaReadOnly = Nothing
  , schemaWriteOnly = Nothing
  , schemaExtensions = mempty
  }

-- TODO this needs liberal doses of quickcheck applied.
instance ToJSON Schema where
  toJSON Schema{..} = object (baseFields ++ H.toList schemaExtensions)
    where
      baseFields = catMaybes
        [ if H.null schemaProperties
          then Nothing
          else Just ("properties" .= schemaProperties)
        , ("title" .=) <$> schemaTitle
        , ("description" .=) <$> schemaDescription
        , ("type" .=) <$> schemaType_
        , if V.null schemaRequired
          then Nothing
          else Just ("required" .= schemaRequired)
        , if V.null schemaAnyOf
          then Nothing
          else Just ("anyOf" .= schemaAnyOf)
        , ("items" .=) <$> schemaItems
        , ("minLength" .=) <$> schemaMinLength
        , ("maxLength" .=) <$> schemaMaxLength
        , ("format" .=) <$> schemaFormat
        , ("enum" .=) <$> schemaEnum
        , ("additionalProperties" .=) <$> schemaAdditionalProperties
        , ("pattern" .=) <$> schemaPattern
        , if schemaNullable
          then Just ("nullable" .= schemaNullable)
          else Nothing
        , ("readOnly" .=) <$> schemaReadOnly
        , ("writeOnly" .=) <$> schemaWriteOnly
        ]

instance FromJSON Schema where
  parseJSON = withObject "Schema" $ killer $ do
    schemaProperties <- defaultOption H.empty "properties"
    schemaTitle <- optional "title"
    schemaDescription <- optional "description"
    schemaType_ <- optional "type"
    schemaRequired <- defaultOption mempty "required"
    schemaAnyOf <- defaultOption mempty "anyOf"
    schemaItems <- optional "items"
    schemaMinLength <- optional "minLength"
    schemaMaxLength <- optional "maxLength"
    schemaFormat <- optional "format"
    schemaEnum <- optional "enum"
    schemaAdditionalProperties <- optional "additionalProperties"
    schemaPattern <- optional "pattern"
    schemaNullable <- defaultOption False "nullable"
    schemaReadOnly <- optional "readOnly"
    schemaWriteOnly <- optional "writeOnly"
    schemaExtensions <- takeExtensions
    pure Schema{..}

data AdditionalProperties
  = AdditionalPropertiesToggle Bool
  | AdditionalPropertiesSchema (Referenceable Schema)
  deriving (Show, Eq)

instance ToJSON AdditionalProperties where
  toJSON (AdditionalPropertiesToggle t) = toJSON t
  toJSON (AdditionalPropertiesSchema s) = toJSON s

instance FromJSON AdditionalProperties where
  parseJSON (Bool t) = pure $ AdditionalPropertiesToggle t
  parseJSON other = AdditionalPropertiesSchema <$> parseJSON other

data ApiEndpoint = ApiEndpoint
  { apiEndpointDescription :: CommonMark
  , apiEndpointOperationId :: Text
  , apiEndpointParameters :: Vector Parameter
  , apiEndpointRequestBody :: RequestBody
  , apiEndpointResponses :: Responses
  , apiEndpointServers :: Vector Server
  , apiEndpointDeprecated :: Bool
  } deriving (Show, Eq)

instance ToJSON ApiEndpoint where
  toJSON ApiEndpoint{..} = object
    [ "description" .= apiEndpointDescription
    , "operationId" .= apiEndpointOperationId
    , "parameters" .= apiEndpointParameters
    , "requestBody" .= apiEndpointRequestBody
    , "responses" .= apiEndpointResponses
    , "servers" .= apiEndpointServers
    , "deprecated" .= apiEndpointDeprecated
    ]
instance FromJSON ApiEndpoint where
  parseJSON = withObject "ApiEndpoint" $ killer $ do
    apiEndpointDescription <- require "description"
    apiEndpointOperationId <- require "operationId"
    apiEndpointParameters <- (fromMaybe mempty <$> optional "parameters")
    apiEndpointRequestBody <- require "requestBody"
    apiEndpointResponses <- require "responses"
    apiEndpointServers <- (fromMaybe mempty <$> optional "servers")
    apiEndpointDeprecated <- (fromMaybe False <$> optional "deprecated")
    pure ApiEndpoint{..}

data Server = Server
  { serverUrl :: Text
  , serverDescription :: Maybe CommonMark
  , serverVariables :: H.HashMap Text ServerVariable
  , serverExtensions :: H.HashMap Text Value
  } deriving (Show, Eq)

instance ToJSON Server where
  toJSON Server{..} = object
    [ "url" .= serverUrl
    , "description" .= serverDescription
    , "variables" .= serverVariables
    ]
instance FromJSON Server where
  parseJSON = withObject "Server" $ killer $ do
    serverUrl <- require "url"
    serverDescription <- optional "description"
    serverVariables <- fromMaybe H.empty <$> optional "variables"
    serverExtensions <- takeExtensions
    pure Server{..}

data ServerVariable = ServerVariable
  { serverVariableEnum :: Vector Text
  , serverVariableDefault_ :: Text
  , serverVariableDescription :: Maybe CommonMark
  , serverVariableExtensions :: H.HashMap Text Value
  } deriving (Show, Eq)

instance ToJSON ServerVariable where
  toJSON ServerVariable{..} = object
    [ "enum" .= serverVariableEnum
    , "default" .= serverVariableDefault_
    , "description" .= serverVariableDescription
    ]

instance FromJSON ServerVariable where
  parseJSON = withObject "ServerVariable" $ killer $ do
    serverVariableEnum <- fromMaybe mempty <$> optional "enum"
    serverVariableDefault_ <- require "default"
    serverVariableDescription <- optional "description"
    serverVariableExtensions <- takeExtensions
    pure ServerVariable{..}

data Example = Example
  { exampleSummary :: Maybe Text
  , exampleDescription :: Maybe CommonMark
  , exampleValue :: Maybe Value
  , exampleExternalValue :: Maybe Text
  , exampleExtensions :: H.HashMap Text Value
  } deriving (Show, Eq)

instance ToJSON Example where
  toJSON Example{..} = object
    [ "summary" .= exampleSummary
    , "description" .= exampleDescription
    , "value" .= exampleValue
    , "externalValue" .= exampleExternalValue
    ]

instance FromJSON Example where
  parseJSON = withObject "Example" $ killer $ do
    exampleSummary <- optional "summary"
    exampleDescription <- optional "description"
    exampleValue <- optional "value"
    exampleExternalValue <- optional "externalValue"
    exampleExtensions <- takeExtensions
    pure Example{..}

data RequestBody = RequestBody
  { requestBodyContent :: H.HashMap Text MediaType
  , requestBodyRequired :: Bool
  } deriving (Show, Eq)

instance ToJSON RequestBody where
  toJSON RequestBody{..} = object
    [ "content" .= requestBodyContent
    , "required" .= requestBodyRequired
    ]

instance FromJSON RequestBody where
  parseJSON = withObject "RequestBody" $ killer $
    RequestBody <$> require "content" <*> require "required"

data StatusPatternElem
  = Digit Int
  | Wildcard
  deriving (Eq, Generic)

instance Hashable StatusPatternElem

instance Show StatusPatternElem where
  show = \case
    Digit i -> show i
    Wildcard -> "X"

data ResponseKey
  = ConstStatus !Int
  | StatusPatternResponse StatusPatternElem StatusPatternElem StatusPatternElem
  | DefaultResponse
  deriving (Eq, Generic)

instance Hashable ResponseKey

instance Show ResponseKey where
  show = \case
    ConstStatus i -> show i
    StatusPatternResponse h m l -> concatMap show [h, m, l]
    DefaultResponse -> "default"

instance ToJSON ResponseKey where
  toJSON = toJSON . T.pack . show

instance ToJSONKey ResponseKey where
  toJSONKey = toJSONKeyText (T.pack . show)

instance FromJSON ResponseKey where
  parseJSON = withText "Response Key" $ \t -> either fail pure $ parseResponseKey t

instance FromJSONKey ResponseKey where
  fromJSONKey = FromJSONKeyTextParser $ \t -> either fail pure $ parseResponseKey t

instance EDE.Unquote ResponseKey

parseResponseKey :: Text -> Either String ResponseKey
parseResponseKey = Parse.parseOnly parser
  where
    parseChar = fmap (Digit . digitToInt) Parse.digit <|> (Parse.char 'X' *> pure Wildcard)
    parseConstOrPattern = do
      high <- parseChar
      middle <- parseChar
      low <- parseChar
      case (high, middle, low) of
        (Digit h, Digit m, Digit l) -> pure $ ConstStatus $ (h * 100) + (m * 10) + l
        _ -> pure $ StatusPatternResponse high middle low

    parser = (Parse.string "default" *> pure DefaultResponse) <|> parseConstOrPattern

data Responses = Responses
  { responsesResponses :: H.HashMap ResponseKey Response
  } deriving (Show, Eq)

instance ToJSON Responses where
  toJSON = toJSON . responsesResponses

instance FromJSON Responses where
  parseJSON v = Responses <$> parseJSON v


data Response = Response
  { responseContent :: RefMap MediaType
  , responseDescription :: CommonMark
  , responseHeaders :: RefMap Header
  , responseLinks :: RefMap Link
  } deriving (Show, Eq)

instance ToJSON Response where
  toJSON Response{..} = object
    [ "content" .= responseContent
    , "description" .= responseDescription
    , "headers" .= responseHeaders
    , "links" .= responseLinks
    ]

instance FromJSON Response where
  parseJSON = withObject "Response" $ killer $ do
    responseDescription <- require "description"
    responseContent <- defaultOption H.empty "content"
    responseHeaders <- defaultOption H.empty "headers"
    responseLinks <- defaultOption H.empty "links"
    pure Response{..}

data MediaType = MediaType
  { mediaTypeSchema :: Maybe (Referenceable Schema)
  , mediaTypeExample :: Maybe Value
  , mediaTypeExamples :: RefMap Example
  , mediaTypeEncoding :: H.HashMap Text Encoding
  } deriving (Show, Eq)

instance ToJSON MediaType where
  toJSON MediaType{..} = object
    [ "schema" .= mediaTypeSchema
    , "example" .= mediaTypeExample
    , "examples" .= mediaTypeExamples
    , "encoding" .= mediaTypeEncoding
    ]

instance FromJSON MediaType where
  parseJSON = withObject "MediaType" $ killer $ do
    mediaTypeSchema <- optional "schema"
    mediaTypeExample <- optional "example"
    mediaTypeExamples <- defaultOption H.empty "examples"
    mediaTypeEncoding <- defaultOption H.empty "encoding"
    pure MediaType{..}

data Encoding = Encoding
  { encodingContentType :: Maybe Text
  , encodingHeaders :: RefMap Header
  , encodingStyle :: Maybe ParameterStyle
  , encodingExplode :: Maybe Bool
  , encodingAllowReserved :: Maybe Bool
  , encodingExtensions :: H.HashMap Text Value
  } deriving (Show, Eq)

instance ToJSON Encoding where
  toJSON Encoding{..} = object
    [ "contentType" .= encodingContentType
    , "headers" .= encodingHeaders
    , "style" .= encodingStyle
    , "explode" .= encodingExplode
    , "allowReserved" .= encodingAllowReserved
    ]

instance FromJSON Encoding where
  parseJSON = withObject "Encoding" $ killer $ do
    encodingContentType <- optional "contentType"
    encodingHeaders <- defaultOption H.empty "headers"
    encodingStyle <- optional "style"
    encodingExplode <- optional "explode"
    encodingAllowReserved <- optional "allowReserved"
    encodingExtensions <- takeExtensions
    pure Encoding{..}

makeFields ''Root
makeFields ''Info
makeFields ''Contact
makeFields ''License
makeFields ''Reference
makePrisms ''Referenceable
makeFields ''Components
makeFields ''Parameter
makeFields ''SecurityScheme
makeFields ''Link
makeFields ''Schema
makeFields ''ApiEndpoint
makeFields ''Server
makeFields ''ServerVariable
makeFields ''Example
makeFields ''RequestBody
makeFields ''Responses
makeFields ''Response
makeFields ''MediaType
makeFields ''Encoding

instance Plated Schema where
  plate f Schema{..} = Schema <$>
    (traverse (traverse f) schemaProperties) <*>
    (pure schemaTitle) <*>
    (pure schemaDescription) <*>
    (pure schemaType_) <*>
    (pure schemaRequired) <*>
    (traverse (traverse f) schemaAnyOf) <*>
    (traverse (traverse f) schemaItems) <*>
    (pure schemaMinLength) <*>
    (pure schemaMaxLength) <*>
    (pure schemaFormat) <*>
    (pure schemaEnum) <*>
    (traverse (\ps -> case ps of
         AdditionalPropertiesToggle _ -> pure ps
         AdditionalPropertiesSchema rs -> AdditionalPropertiesSchema <$> traverse f rs
     ) schemaAdditionalProperties) <*>
    (pure schemaPattern) <*>
    (pure schemaNullable) <*>
    (pure schemaReadOnly) <*>
    (pure schemaWriteOnly) <*>
    (pure schemaExtensions)
