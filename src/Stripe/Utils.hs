{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RecordWildCards #-}
module Stripe.Utils
  ( module Stripe.Utils
  , module X
  , Text
  , A.FromJSON(..)
  , A.Object
  , A.withText
  , encodeUtf8
  , decodeUtf8
  ) where

import Control.Lens (Lens', view)
import Control.Monad.Reader
import Control.Monad.State.Lazy
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C
import Conduit (ConduitT, yieldMany)
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Typeable as X
import qualified Data.Vector as V
import GHC.Generics as X
import qualified Data.HashMap.Strict as H
import Network.HTTP.Conduit
import Network.HTTP.Client.TLS
import System.Environment
import Prelude hiding (id)

class HasId s a | s -> a where
  id :: Lens' s a

-- | Stripe utilizes cursor-based pagination via the starting_after and ending_before parameters. Both parameters take an existing object ID value (see below) and return objects in reverse chronological order. The ending_before parameter returns objects listed before the named object. The starting_after parameter returns objects listed after the named object. If both parameters are provided, only ending_before is used.
data Pagination a = Pagination
  { paginationLimit :: Maybe Int
  , paginationStartingAfter :: Maybe (Id a)
  , paginationEndingAfter :: Maybe (Id a)
  } deriving (Show, Eq, Generic, Typeable)

-- | No pagination parameters specified, returns the Stripe query with no pagination paramters set.
basePage :: Pagination a
basePage = Pagination Nothing Nothing Nothing

-- TODO may need to get fancier
enumerate :: (StripeMonad m, HasId a (Id a)) => (Pagination a -> m (List a)) -> ConduitT () a m ()
enumerate baseQuery = go basePage
  where
    go req = do
      result <- lift $ baseQuery req
      yieldMany $ listData result
      if listHasMore result
        then go $ basePage { paginationStartingAfter = Just $ view id $ V.last $ listData result }
        else return ()

paginationParams :: Pagination a -> [(ByteString, Maybe ByteString)]
paginationParams p = filter (isJust . snd)
  [ ("limit", (C.pack . show) <$> paginationLimit p)
  , ("starting_after", (encodeUtf8 . fromId) <$> paginationStartingAfter p)
  , ("ending_after", (encodeUtf8 . fromId) <$> paginationEndingAfter p)
  ]

data List a = List
  { listUrl :: Text
  , listHasMore :: Bool
  , listData :: V.Vector a
  , listTotalCount :: Maybe Int
  } deriving (Show, Eq, Generic, Typeable)

instance A.FromJSON a => A.FromJSON (List a) where
  parseJSON = parseObject "List" $ do
    assertObject "list"
    List
      <$> req "url"
      <*> req "has_more"
      <*> req "data"
      <*> opt "total_count"

newtype Id a = Id { fromId :: Text }
  deriving (Show, Eq, Generic, Typeable)

instance A.FromJSON (Id a) where
  parseJSON = A.withText "Id" (pure . Id)

newtype Timestamp = Timestamp Integer
  deriving (Show, Eq, Generic, Typeable)

instance A.FromJSON Timestamp where
  parseJSON = fmap Timestamp . A.parseJSON

type Metadata = H.HashMap Text Text
type Expandable a = Id a
type CountryCode = Text
type CurrencyCode = Text
data Application

data Address = Address
  { addressCity :: Maybe Text
  , addressCountry :: CountryCode -- TODO make country code
  , addressLine1 :: Text
  , addressLine2 :: Maybe Text
  , addressPostalCode :: Text
  , addressState :: Maybe Text
  } deriving (Show, Eq, Generic, Typeable)

instance A.FromJSON Address where
  parseJSON = parseObject "Address" $ do
    Address
      <$> opt "city"
      <*> req "country"
      <*> req "line1"
      <*> opt "line2"
      <*> req "postal_code"
      <*> opt "state"

type Parser = StateT A.Object A.Parser


parseObject :: String -> Parser a -> A.Value -> A.Parser a
parseObject n m = A.withObject n (\o -> runParser o m)

runParser :: A.Object -> Parser a -> A.Parser a
runParser o m = do
  (r, o') <- runStateT m o
  if H.null o'
    then pure r
    else fail ("Unused keys: " <> show o')

req :: A.FromJSON a => Text -> Parser a
req t = do
  o <- get
  r <- lift (o A..: t)
  modify (H.delete t)
  pure r

assertObject :: Text -> Parser ()
assertObject t = do
  str <- req "object"
  when (str /= t) $ fail ("object is of type " ++ show str ++ ", expected object of type " ++ show t)

opt :: A.FromJSON a => Text -> Parser (Maybe a)
opt t = do
  o <- get
  r <- lift (o A..:? t)
  modify (H.delete t)
  pure r

class Monad m => StripeMonad (m :: * -> *) where
  jsonGet :: A.FromJSON a => ByteString -> [(ByteString, Maybe ByteString)] -> m a
  -- jsonPost :: _

data StripeState = StripeState
  { stripeStateBaseRequest :: Request -- A.Object
  , stripeStateManager :: Manager
  , stripeStateVersion :: StripeVersion
  , stripeStateKey :: StripeKey
  }

newtype StripeVersion = StripeVersion ByteString

currentStripeVersion :: StripeVersion
currentStripeVersion = StripeVersion "2018-10-31"

newtype StripeKey = StripeKey ByteString

mkStripeState :: MonadIO m => StripeKey -> m StripeState
mkStripeState stripeStateKey = liftIO $ do
  stripeStateManager <- getGlobalManager
  stripeStateBaseRequest <- parseRequest "https://api.stripe.com/v1/"
  let stripeStateVersion = currentStripeVersion
  pure $ StripeState {..}

instance (MonadIO m) => StripeMonad (ReaderT StripeState m) where
  jsonGet b qps = do
    StripeState {..} <- ask
    let (StripeVersion v) = stripeStateVersion
        (StripeKey k) = stripeStateKey
    r <-
      liftIO $
      httpLbs
        (setQueryString qps $
        applyBasicAuth k "" $
        stripeStateBaseRequest
          { requestHeaders =
              ("Stripe-Version", v) : requestHeaders stripeStateBaseRequest
          , path = path stripeStateBaseRequest <> b
          })
        stripeStateManager
    case A.eitherDecode $ responseBody r of
      Left err -> fail err
      Right ok -> pure ok

runSimpleStripe :: (MonadIO m) => StripeState -> ReaderT StripeState m a -> m a
runSimpleStripe st m = runReaderT m st

-- | Get API key from STRIPE_SECRET_KEY
stripeWithEnv :: (MonadIO m) => ReaderT StripeState m a -> m a
stripeWithEnv m = do
  k <- liftIO $ getEnv "STRIPE_SECRET_KEY"
  st <- mkStripeState $ StripeKey $ C.pack k
  runSimpleStripe st m
