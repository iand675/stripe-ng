module Stripe.Files where
import {-# SOURCE #-} Stripe.FilesLinks
import Stripe.Utils

data FilePurpose
  = BusinessLogo
  | CustomerSignature
  | DisputeEvidence
  | FinanceReportRun
  | FoundersStockDocument
  | IdentityDocument
  | PCIDocument
  | SigmaScheduledQuery
  | TaxDocumentUserUpload
  deriving (Show, Eq, Generic, Typeable)

instance FromJSON FilePurpose where
  parseJSON = withText "FilePurpose" $ \t -> case t of
    "business_logo" -> pure BusinessLogo
    "customer_signature" -> pure CustomerSignature
    "dispute_evidence" -> pure DisputeEvidence
    "finance_report_run" -> pure FinanceReportRun
    "founders_stock_document" -> pure FoundersStockDocument
    "identity_document" -> pure IdentityDocument
    "pci_document" -> pure PCIDocument
    "sigma_scheduled_query" -> pure SigmaScheduledQuery
    "tax_document_user_upload" -> pure TaxDocumentUserUpload
    _ -> fail ("Invalid FilePurpose: " ++ show t)

data FileType
  = CSV
  | PDF
  | JPG
  | PNG
  | Other Text
  deriving (Show, Eq, Generic, Typeable)

instance FromJSON FileType where
  parseJSON = withText "FileType" $ \t -> case t of
    "csv" -> pure CSV
    "pdf" -> pure PDF
    "jpg" -> pure JPG
    "png" -> pure PNG
    other -> pure $ Other other

data File = File
  { fileId :: Id File
  , fileCreated :: Timestamp
  , fileFilename :: Text
  , fileLinks :: List FileLink
  , filePurpose :: FilePurpose
  , fileSize :: Integer
  , fileTitle :: Maybe Text
  , fileType_ :: FileType
  , fileUrl :: Text
  } deriving (Show, Eq, Generic, Typeable)

instance FromJSON File where
  parseJSON = parseObject "File" $ do
    assertObject "file"
    File
      <$> req "id"
      <*> req "created"
      <*> req "filename"
      <*> req "links"
      <*> req "purpose"
      <*> req "size"
      <*> opt "title"
      <*> req "type"
      <*> req "url"

-- createFile
retrieveFile :: (StripeMonad m) => Id File -> m File
retrieveFile (Id fileId) = jsonGet ("files/" <> encodeUtf8 fileId) []

listAllFiles :: (StripeMonad m) => Pagination File -> m (List File)
listAllFiles = jsonGet "files" . paginationParams
