{-# LANGUAGE ScopedTypeVariables #-}
module BalanceTest where
import Control.Lens
import Data.Proxy
import qualified Stripe.Lens as S
import Stripe.Balance
import Stripe.Utils
import Test.Tasty.Hspec
import qualified Data.Vector as V

spec_balance :: Spec
spec_balance = specify "Retrieve balance" $ do
  b <- stripeWithEnv $ retrieveBalance
  print b
  -- shouldNotBe True $ balanceLiveMode b
  shouldSatisfy b (not . null . balanceAvailable)

spec_balance_history :: Spec
spec_balance_history = do
  specify "List history" $ do
    h <- stripeWithEnv $ listAllBalanceHistory basePage
    shouldSatisfy (h `asProxyTypeOf` Proxy @(List BalanceTransaction)) ((>= 0) . length . listData_)
  specify "Get balance transaction" $ do
    h <- stripeWithEnv $ listAllBalanceHistory basePage
    shouldSatisfy h ((>= 1) . length . listData_)
    t <- stripeWithEnv $ retrieveBalanceTransaction $ balanceTransactionId $ V.head $ listData_ h
    print t
    balanceTransactionCurrency t `shouldBe` USD
