-- TODO rename module to Stripe.FileLinks
module Stripe.FilesLinks where

import Stripe.Files
import Stripe.Utils

data FileLink = FileLink
  { fileLinkId :: Id FileLink
  , fileLinkCreated :: Timestamp
  , fileLinkExpired :: Bool
  , fileLinkExpiresAt :: Maybe Timestamp
  , fileLinkFile :: Expandable File
  , fileLinkLiveMode :: Bool
  , fileLinkMetadata :: Metadata
  , fileLinkUrl :: Text
  } deriving (Show, Eq, Generic, Typeable)

instance FromJSON FileLink where
  parseJSON = parseObject "FileLink" $ do
    assertObject "file_link"
    FileLink
      <$> req "id"
      <*> req "created"
      <*> req "expired"
      <*> opt "expires_at"
      <*> req "file"
      <*> req "livemode"
      <*> req "metadata"
      <*> req "url"

-- createFileLink

retrieveFileLink :: (StripeMonad m, StripeResult FileLink fileLink) => Id FileLink -> m fileLink
retrieveFileLink (Id fileLinkId) = jsonGet (Proxy @FileLink) ("file_links/" <> encodeUtf8 fileLinkId) []

-- updateFileLink

listAllFileLinks :: (StripeMonad m, StripeResult (List FileLink) fileLinkList) => m fileLinkList
listAllFileLinks = jsonGet (Proxy @(List FileLink)) "file_links" []

data CreateFileLink
data UpdateFileLink
