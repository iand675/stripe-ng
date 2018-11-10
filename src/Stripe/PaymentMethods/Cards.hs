module Stripe.PaymentMethods.Cards where
import Stripe.Customers
import Stripe.Utils

data CardCheck
  = Pass
  | Fail
  | Unavailable
  | Unchecked
  deriving (Show, Eq, Generic, Typeable)

instance FromJSON CardCheck where
  parseJSON = withText "CardCheck" $ \t -> case t of
    "pass" -> pure Pass
    "fail" -> pure Fail
    "unavailable" -> pure Unavailable
    "unchecked" -> pure Unchecked
    _ -> fail ("Invalid CardCheck: " ++ show t)

data AvailablePayoutMethods
  = Standard
  | Instant
  deriving (Show, Eq, Generic, Typeable)

instance FromJSON AvailablePayoutMethods where
  parseJSON = withText "AvailablePayoutMethods" $ \t -> case t of
    "standard" -> pure Standard
    "instant" -> pure Instant
    _ -> fail ("Invalid AvailablePayoutMethods: " ++ show t)

data CardBrand
  = AmericanExpress
  | DinersClub
  | Discover
  | JCB
  | MasterCard
  | UnionPay
  | Visa
  | UnknownCardBrand
  deriving (Show, Eq, Generic, Typeable)

instance FromJSON CardBrand where
  parseJSON = withText "CardBrand" $ \t -> case t of
    "American Express" -> pure AmericanExpress
    "Diners Club" -> pure DinersClub
    "Discover" -> pure Discover
    "JCB" -> pure JCB
    "MasterCard" -> pure MasterCard
    "UnionPay" -> pure UnionPay
    "Visa" -> pure Visa
    "Unknown" -> pure UnknownCardBrand
    _ -> fail ("Invalid CardBrand: " ++ show t)

data CardFunding
  = Credit
  | Debit
  | Prepaid
  | UnknownCardFunding
  deriving (Show, Eq, Generic, Typeable)

instance FromJSON CardFunding where
  parseJSON = withText "CardFunding" $ \t -> case t of
    "credit" -> pure Credit
    "debit" -> pure Debit
    "prepaid" -> pure Prepaid
    _ -> fail ("Invalid CardFunding: " ++ show t)

data TokenizationMethod
  = ApplePay
  | AndroidPay
  deriving (Show, Eq, Generic, Typeable)

instance FromJSON TokenizationMethod where
  parseJSON = withText "TokenizationMethod" $ \t -> case t of
    "apple_pay" -> pure ApplePay
    "android_pay" -> pure AndroidPay
    _ -> fail ("Invalid TokenizationMethod: " ++ show t)

data Card = Card
  { cardId :: Id Card
  -- TODO Expandable, says ''Custom Only'
  -- , cardAccount :: Maybe
  , cardAddressCity :: Maybe Text
  , cardAddressCountry :: Maybe Text
  , cardAddressLine1 :: Maybe Text
  , cardAddressLine1Check :: Maybe CardCheck
  , cardAddressLine2 :: Maybe Text
  , cardAddressState :: Maybe Text
  , cardAddressZip :: Maybe Text
  , cardAddressZipCheck :: Maybe CardCheck
  , cardAvailablePayoutMethods :: Maybe [AvailablePayoutMethods]
  , cardBrand :: CardBrand
  , cardCountry :: Text
  -- TODO Custom Only
  -- , cardCurrency
  -- TODO Expandable
  , cardCustomer :: Maybe (Expandable Customer)
  , cardCvcCheck :: Maybe CardCheck
  -- TODO Custom Only
  -- , cardDefaultForCurrency
  , cardDynamicLast4 :: Maybe Text
  , cardExpMonth :: Word
  , cardExpYear :: Word
  , cardFingerprint :: Maybe Text
  , cardFunding :: CardFunding
  -- TODO always present?
  , cardLast4 :: Maybe Text
  , cardMetadata :: Metadata
  , cardName :: Maybe Text
  -- TODO Expandable
  , cardRecipient :: Maybe (Expandable Customer)
  , cardTokenizationMethod :: Maybe TokenizationMethod
  } deriving (Show, Eq, Generic, Typeable)

instance FromJSON Card where
  parseJSON = parseObject "Card" $ do
    assertObject "card"
    Card
      <$> req "id"
      <*> opt "address_city"
      <*> opt "address_country"
      <*> opt "address_line1"
      <*> opt "address_line1_check"
      <*> opt "address_line2"
      <*> opt "address_state"
      <*> opt "address_zip"
      <*> opt "address_zip_check"
      <*> opt "available_payout_methods"
      <*> req "brand"
      <*> req "country"
      <*> opt "customer"
      <*> opt "cvc_check"
      -- <*> opt "default_for_currency"
      <*> opt "dynamic_last4"
      <*> req "exp_month"
      <*> req "exp_year"
      <*> opt "fingerprint"
      <*> req "funding"
      <*> opt "last4"
      <*> req "metadata"
      <*> opt "name"
      <*> opt "recipient"
      <*> opt "tokenization_method"

data CreateCard
data UpdateCard
data DeleteCard

{-
createCard
retrieveCard
updateCard
deleteCard
listAllCards
-}
