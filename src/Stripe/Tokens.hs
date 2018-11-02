module Stripe.Tokens where
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

{-
createCardToken
createBankAccountToken
createPIIToken
createAccountToken
retrieveToken
-}
