{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Stripe.Tokens where
import Stripe.Customers
import Stripe.PaymentMethods.BankAccounts
import Stripe.PaymentMethods.Cards
import Stripe.Utils

data TokenType
  = AccountToken
  | BankAccountToken
  | CardToken
  | PIIToken
  deriving (Show, Eq, Generic, Typeable)

instance FromJSON TokenType where
  parseJSON = withText "TokenType" $ \t -> case t of
    "account" -> pure AccountToken
    "bank_account" -> pure BankAccountToken
    "card" -> pure CardToken
    "pii" -> pure PIIToken
    _ -> fail ("Invalid TokenType: " ++ show t)

data Token = Token
  { tokenId :: Id Token
  , tokenBankAccount :: Maybe BankAccount
  , tokenCard :: Maybe Card
  , tokenClientIp :: Maybe Text
  , tokenLiveMode :: Bool
  , tokenType_ :: TokenType
  , tokenUsed :: Bool
  } deriving (Show, Eq, Generic, Typeable)

instance FromJSON Token where
  parseJSON = parseObject "Token" $ do
    Token
      <$> req "id"
      <*> opt "bank_account"
      <*> opt "card"
      <*> opt "client_ip"
      <*> req "livemode"
      <*> req "type"
      <*> req "used"

newtype ExpMonth = ExpMonth { fromExpMonth :: Word }
  deriving (Show, Eq, Generic, Typeable, ToHttpApiData)

newtype ExpYear = ExpYear { fromExpYear :: Word }
  deriving (Show, Eq, Generic, Typeable, ToHttpApiData)

newtype CardNumber = CardNumber { fromCardNumber :: Text }
  deriving (Show, Eq, Generic, Typeable, ToHttpApiData)

newtype Cvc = Cvc { fromCvc :: Text }
  deriving (Show, Eq, Generic, Typeable, ToHttpApiData)

data NewCard = NewCard
  { newCardExpMonth :: ExpMonth
  , newCardExpYear :: ExpYear
  , newCardNumber :: CardNumber
  , newCardCurrency :: Maybe CurrencyCode
  , newCardCvc :: Maybe Text
  , newCardName :: Maybe Text
  , newCardAddressLine1 :: Maybe Text
  , newCardAddressLine2 :: Maybe Text
  , newCardAddressCity :: Maybe Text
  , newCardAddressState :: Maybe Text
  , newCardAddressZip :: Maybe Text
  , newCardAddressCountry :: Maybe Text
  } deriving (Show, Eq, Generic, Typeable)

newCard :: CardNumber -> ExpMonth -> ExpYear -> NewCard
newCard n m y = NewCard
  { newCardExpMonth = m
  , newCardExpYear = y
  , newCardNumber = n
  , newCardCurrency = Nothing
  , newCardCvc = Nothing
  , newCardName = Nothing
  , newCardAddressLine1 = Nothing
  , newCardAddressLine2 = Nothing
  , newCardAddressCity = Nothing
  , newCardAddressState = Nothing
  , newCardAddressZip = Nothing
  , newCardAddressCountry = Nothing
  }

instance ToForm NewCard where
  toForm NewCard{..} = mconcat
    [ reqParam "exp_month" newCardExpMonth
    , reqParam "exp_year" newCardExpYear
    , reqParam "number" newCardNumber
    , optParam "currency" newCardCurrency
    , optParam "cvc" newCardCvc
    , optParam "name" newCardName
    , optParam "address_line1" newCardAddressLine1
    , optParam "address_line2" newCardAddressLine2
    , optParam "address_city" newCardAddressCity
    , optParam "address_state" newCardAddressState
    , optParam "address_zip" newCardAddressZip
    , optParam "address_country" newCardAddressCountry
    ]

data CreateCardToken = CreateCardToken
  { createCardTokenCard :: NewCard
  , createCardTokenCustomer :: Maybe (Id Customer)
  }

instance ToForm CreateCardToken where
  toForm CreateCardToken{..} = mconcat
    [ dictParams "card" createCardTokenCard
    , optParam "customer" createCardTokenCustomer
    ]

data CreateBankAccount
data CreatePIIToken
data CreateAccountToken

createCardToken :: (MonadStripe m, StripeResult Token token) => CreateCardToken -> m token
createCardToken = stripePost (Proxy @Token) "tokens"

{-
createBankAccountToken
createPIIToken
createAccountToken
retrieveToken
-}
