module Stripe.PaymentMethods.BankAccounts where
import Stripe.Core
import Stripe.Customers
import Stripe.Utils

data AccountType
  = Individual
  | Company
  deriving (Show, Eq, Generic, Typeable)

instance FromJSON AccountType where
  parseJSON = withText "AccountType" $ \t -> case t of
    "individual" -> pure Individual
    "company" -> pure Company
    _ -> fail ("Invalid AccountType: " ++ show t)

data BankAccountStatus
  = New
  | Validated
  | Verified
  | VerificationFailed
  | Errored
  deriving (Show, Eq, Generic, Typeable)

instance FromJSON BankAccountStatus where
  parseJSON = withText "BankAccountStatus" $ \t -> case t of
    "new" -> pure New
    "validated" -> pure Validated
    "verified" -> pure Verified
    "verification_failed" -> pure VerificationFailed
    "errored" -> pure Errored
    _ -> fail ("Invalid BankAccountStatus: " ++ show t)

data BankAccount = BankAccount
  { bankAccountId :: Id BankAccount
  , bankAccountAccount :: Maybe Text
  , bankAccountAccountHolderName :: Text
  , bankAccountAccountHolderType :: AccountType
  , bankAccountBankName :: Text
  , bankAccountCountry :: CountryCode -- TODO Country code
  , bankAccountCurrency :: CurrencyCode -- TODO Currency code
  , bankAccountCustomer :: Maybe (Id Customer)
  , bankAccountDefaultForCurrency :: Maybe Bool
  , bankAccountFingerprint :: Maybe Text
  , bankAccountLast4 :: Text
  , bankAccountMetadata :: Metadata
  , bankAccountRoutingNumber :: Text
  , bankAccountStatus :: BankAccountStatus
  } deriving (Show, Eq, Generic, Typeable)

instance FromJSON BankAccount where
  parseJSON = parseObject "BankAccount" $ do
    assertObject "bank_account"
    BankAccount
      <$> req "id"
      <*> opt "account"
      <*> req "account_holder_name"
      <*> req "account_holder_type"
      <*> req "bank_name"
      <*> req "country"
      <*> req "currency"
      <*> opt "customer"
      <*> opt "default_for_currency"
      <*> opt "fingerprint"
      <*> req "last4"
      <*> req "metadata"
      <*> req "routing_number"
      <*> req "status"

data CreateBankAccount
data UpdateBankAccount
data VerifyBankAccount
data DeleteBankAccount

{-
createBankAccount
retrieveBankAccount
updateBankAccount
verifyBankAccount
deleteBankAccount
listAllBankAccounts
-}
