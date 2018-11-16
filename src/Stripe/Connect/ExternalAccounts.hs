module Stripe.Connect.ExternalAccounts where

import Stripe.Core
import Stripe.PaymentMethods.BankAccounts
import Stripe.Utils

-- createBankAccount

retrieveBankAccount :: (MonadStripe m, StripeResult BankAccount bankAccount) => Id () -> Id BankAccount -> m bankAccount
retrieveBankAccount (Id accountId) (Id bankAccountId) =
  stripeGet
    (Proxy @BankAccount)
    ("accounts/" <> encodeUtf8 accountId <> "/external_accounts/" <>
     encodeUtf8 bankAccountId)
    []

data CreateBankAccount
data UpdateBankAccount
data DeleteBankAccount
data CreateCard
data UpdateCard
data DeleteCard


-- updateBankAccount
-- deleteBankAccount
-- listAllBankAccounts :: (StripeMonad m) => m (List BankAccount)
-- listAllBankAccounts
-- createCard
-- retrieveCard
-- updateCard
-- deleteCard
-- listAllCards
