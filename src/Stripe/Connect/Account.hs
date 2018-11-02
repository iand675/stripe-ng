module Stripe.Connect.Account where
import Data.HashMap.Strict (HashMap)
import Stripe.PaymentMethods.BankAccounts hiding (AccountType)
import Stripe.PaymentMethods.Cards
import Stripe.Files
import Stripe.Utils

data AccountCapability
  = Active
  | Pending
  | Inactive
  deriving (Show, Eq, Generic, Typeable)

data DeclineChargeOn = DeclineChargeOn
  { declineChargeOnAvsFailure :: Bool
  , declineChargeOnCvcFailure :: Bool
  } deriving (Show, Eq, Generic, Typeable)

data AccountType
  = Standard
  | Custom
  | Express
  deriving (Show, Eq, Generic, Typeable)

data Account = Account
  { accountId :: Id Account
  , accountBusinessLogo :: Expandable File
  , accountBusinessName :: Text
  , accountBusinessPrimaryColor :: Maybe Text
  , accountBusinessUrl :: Maybe Text
  , accountCapabilities :: Maybe (HashMap Text AccountCapability)
  , accountChargesEnabled :: Bool
  , accountCountry :: CountryCode
  , accountCreated :: Timestamp
  , accountDebitNegativeBalances :: Maybe Bool
  , accountDeclineChargeOn :: Maybe DeclineChargeOn
  , accountDefaultCurrency :: CurrencyCode
  , accountDetailsSubmitted :: Bool
  , accountDisplayName :: Text
  , accountEmail :: Text
  , accountExternalAccounts :: List (Either BankAccount Card)
  -- TODO
  -- , accountLegalEntity ::
  , accountMetadata :: Metadata
  -- TODO
  -- , accountPayoutSchedule
  , accountPayoutsEnabled :: Bool
  , accountProductDescription :: Maybe Text
  , accountStatementDescriptor :: Maybe Text
  , accountSupportEmail :: Text
  , accountSupportPhone :: Text
  , accountSupportUrl :: Text
  , accountTimezone :: Text
  -- TODO
  -- , accountTosAcceptance ::
  , accountType_ :: AccountType
  -- , accountVerification ::
  } deriving (Show, Eq, Generic, Typeable)

{-
createAccount
retrieveAccountDetails
updateAccount
deleteAccount
rejectAccount
listAllConnectedAccounts
createLoginLink
-}
