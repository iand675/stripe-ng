{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Stripe.Core where

import qualified Control.Exception as E
import Control.Lens (Lens')
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Coerce
import Data.Hashable
import Data.Scientific (toBoundedInteger)
import Data.String (IsString(..))
import Data.Tagged
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX
import Data.Typeable
import GHC.Generics
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Status
import Stripe.Errors
import Web.FormUrlEncoded
import Web.HttpApiData

class StripeResult t a where
  stripeResult :: (MonadStripe m) => Tagged t (Response L.ByteString) -> m a

instance (FromJSON t) => StripeResult t t where
  stripeResult (Tagged r) =
    case eitherDecode $ responseBody r of
      Left err -> fail err
      Right ok -> pure ok

instance (FromJSON t) => StripeResult t (Response t) where
  stripeResult (Tagged r) =
    case eitherDecode $ responseBody r of
      Left err -> fail err
      Right ok -> pure (ok <$ r)

instance StripeResult t () where
  stripeResult = const $ pure ()

instance StripeResult t (Response L.ByteString) where
  stripeResult = pure . unTagged

instance (StripeResult t r) => StripeResult t (Value, r) where
  stripeResult r'@(Tagged r) =
    case eitherDecode $ responseBody r of
      Left err -> fail err
      Right ok -> (,) <$> pure ok <*> stripeResult r'


class Monad m => MonadStripe m where
  askStripeState :: m StripeState

  throwStripeError :: StripeError -> m a
  default throwStripeError :: (MonadError e m, AsStripeError e) => StripeError -> m a
  throwStripeError = throwError . fromStripeError

  catchStripeError :: m a -> (StripeError -> m a) -> m a
  default catchStripeError :: (MonadError e m, AsStripeError e) => m a -> (StripeError -> m a) -> m a
  catchStripeError m f = catchError m (\e -> maybe (throwError e) f $ toStripeError e)

  stripeGet :: StripeResult t a => proxy t -> ByteString -> [(ByteString, Maybe ByteString)] -> m a
  default stripeGet :: (MonadIO m, StripeResult t a) => proxy t -> ByteString -> [(ByteString, Maybe ByteString)] -> m a
  stripeGet = standardStripeGet

  stripePost :: (ToForm a, StripeResult t b) => proxy t -> ByteString -> a -> m b
  default stripePost :: (MonadIO m, ToForm a, StripeResult t b) => proxy t -> ByteString -> a -> m b
  stripePost = standardStripePost

  stripeDelete :: StripeResult t a => proxy t -> ByteString -> [(ByteString, Maybe ByteString)] -> m a
  default stripeDelete :: (MonadIO m, StripeResult t a) => proxy t -> ByteString -> [(ByteString, Maybe ByteString)] -> m a
  stripeDelete = standardStripeDelete

instance (MonadIO m, MonadThrow m, MonadCatch m) => MonadStripe (ReaderT StripeState m) where
  askStripeState = ask
  throwStripeError = throwM
  catchStripeError = catch

instance (MonadStripe m) => MonadStripe (ExceptT StripeError m) where
  askStripeState = lift askStripeState
  stripeGet p b qps =
    ExceptT $ catchStripeError (Right <$> stripeGet p b qps) (pure . Left)
  stripePost p b d =
    ExceptT $ catchStripeError (Right <$> stripePost p b d) (pure . Left)
  stripeDelete p b qps =
    ExceptT $ catchStripeError (Right <$> stripeDelete p b qps) (pure . Left)
  throwStripeError = throwError
  catchStripeError = catchError

newtype StripeKey = StripeKey { fromStripeKey :: ByteString }
  deriving (Show, Eq, Ord)

implementedStripeVersion :: StripeVersion
implementedStripeVersion = StripeVersion "2018-10-31"

newtype StripeVersion = StripeVersion { fromStripeVersion :: ByteString }
  deriving (Show, Eq, Ord)


data StripeState = StripeState
  { stripeStateBaseRequest :: Request
  , stripeStateManager :: Manager
  , stripeStateVersion :: StripeVersion
  , stripeStateKey :: StripeKey
  }



newtype Id a = Id { fromId :: Text }
  deriving (Eq, Ord, Generic, Typeable, Hashable, ToJSON, FromJSON, ToHttpApiData, IsString)

class HasId s a | s -> a where
  id :: Lens' s a

instance Show (Id a) where
  show = show . fromId

instance Read (Id a) where
  readsPrec d r = coerce (readsPrec d r :: [(Text, String)])

-- | Stripe utilizes cursor-based pagination via the starting_after and ending_before parameters. Both parameters take an existing object ID value (see below) and return objects in reverse chronological order. The ending_before parameter returns objects listed before the named object. The starting_after parameter returns objects listed after the named object. If both parameters are provided, only ending_before is used.
data Pagination a = Pagination
  { paginationLimit :: Maybe Int
  , paginationStartingAfter :: Maybe (Id a)
  , paginationEndingAfter :: Maybe (Id a)
  } deriving (Show, Eq, Generic, Typeable)

-- | No pagination parameters specified, returns the Stripe query with no pagination paramters set.
basePage :: Pagination a
basePage = Pagination Nothing Nothing Nothing


newtype Timestamp = Timestamp { fromTimestamp :: UTCTime }
  deriving (Show, Eq, Generic, Typeable)

instance FromJSON Timestamp where
  parseJSON = withScientific "Timestamp" $ \n -> do
    case toBoundedInteger n of
      Nothing -> fail "Timestamp was not a bounded integer"
      Just ts -> pure $ Timestamp $ posixSecondsToUTCTime $ realToFrac (ts :: Word)

type Expandable a = Id a

buildRequest :: StripeState -> ByteString -> [(ByteString, Maybe ByteString)] -> Request
buildRequest StripeState{..} b qps =
  setQueryString qps $
  applyBasicAuth (fromStripeKey stripeStateKey) "" $
  stripeStateBaseRequest
    { requestHeaders = ("Stripe-Version", fromStripeVersion stripeStateVersion) : requestHeaders stripeStateBaseRequest
    , path = path stripeStateBaseRequest <> b
    }

extractResult :: (MonadStripe m, StripeResult t a) => proxy t -> Response L.ByteString -> m a
extractResult p r = if responseStatus r >= status400
  then case eitherDecode $ responseBody r of
    Left err -> E.throw $ StripeJsonError err
    Right ok -> throwStripeError (ok { stripeErrorContext = ErrorContext (responseStatus r) (responseHeaders r) (responseBody r)})
  else stripeResult $ tagWith p r

standardStripeGet :: (MonadStripe m, MonadIO m, StripeResult t a) => proxy t -> ByteString -> [(ByteString, Maybe ByteString)] -> m a
standardStripeGet p b qps = do
  st <- askStripeState
  r <- liftIO $ httpLbs (buildRequest st b qps) (stripeStateManager st)
  extractResult p r

standardStripePost :: (MonadStripe m, MonadIO m, ToForm a, StripeResult t b) => proxy t -> ByteString -> a -> m b
standardStripePost p b f = do
  st <- askStripeState
  r <-
    liftIO $
    httpLbs
      ((buildRequest st b []) { method = "POST", requestBody = RequestBodyLBS $ urlEncodeAsForm f })
      (stripeStateManager st)
  extractResult p r

standardStripeDelete :: (MonadStripe m, MonadIO m, StripeResult t a) => proxy t -> ByteString -> [(ByteString, Maybe ByteString)] -> m a
standardStripeDelete p b qps = do
  st <- askStripeState
  r <-
    liftIO $
    httpLbs
      ((buildRequest st b qps) { method = "DELETE" })
      (stripeStateManager st)
  extractResult p r

notFoundToMaybe :: MonadStripe m => m a -> m (Maybe a)
notFoundToMaybe m = catchStripeError (Just <$> m) $ \se -> if errorContextStatus (stripeErrorContext se) == notFound404
  then pure Nothing
  else throwStripeError se


mkStripeState :: MonadIO m => StripeKey -> m StripeState
mkStripeState stripeStateKey = liftIO $ do
  stripeStateManager <- getGlobalManager
  stripeStateBaseRequest <- parseRequest "https://api.stripe.com/v1/"
  let stripeStateVersion = implementedStripeVersion
  pure $ StripeState {..}
