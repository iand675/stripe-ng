module Stripe.Connect.CountrySpecs where
import Data.HashMap.Strict (HashMap)
import Stripe.Utils

data CountrySpec = CountrySpec
  { countrySpecId :: Id CountrySpec
  , countrySpecDefaultCurrency :: CurrencyCode
  , countrySpecSupportedBankAccountCurrencies :: HashMap CurrencyCode [CountryCode]
  , countrySpecSupportedPaymentCurrencies :: [CurrencyCode]
  , countrySpecSupportedPaymentMethods :: [Text]
  , countrySpecSupportedTransferCountries :: [CountryCode]
  , countrySpecVerificationFields :: VerificationFieldSpec
  } deriving (Show, Eq, Generic, Typeable)

instance FromJSON CountrySpec where
  parseJSON = parseObject "CountrySpec" $ do
    assertObject "country_spec"
    CountrySpec
      <$> req "id"
      <*> req "default_currency"
      <*> req "supported_bank_account_currencies"
      <*> req "supported_payment_currencies"
      <*> req "supported_payment_methods"
      <*> req "supported_transfer_countries"
      <*> req "verification_fields"

data VerificationFields = VerificationFields
  { verificationFieldsMinimum :: [Text]
  , verificationFieldsAdditional :: [Text]
  } deriving (Show, Eq, Generic, Typeable)

instance FromJSON VerificationFields where
  parseJSON = parseObject "VerificationFields" $ do
    VerificationFields
      <$> req "minimum"
      <*> req "additional"

data VerificationFieldSpec = VerificationFieldSpec
  { verificationFieldSpecIndividual :: VerificationFields
  , verificationFieldSpecCompany :: VerificationFields
  } deriving (Show, Eq, Generic, Typeable)

instance FromJSON VerificationFieldSpec where
  parseJSON = parseObject "VerificationFieldSpec" $ do
    VerificationFieldSpec
      <$> req "individual"
      <*> req "company"

listCountrySpecs :: StripeMonad m => m (List CountrySpec)
listCountrySpecs = jsonGet "country_specs" []

retrieveCountrySpec :: StripeMonad m => CountryCode -> m (List CountryCode)
retrieveCountrySpec cc = jsonGet ("country_specs/" <> encodeUtf8 cc) []
