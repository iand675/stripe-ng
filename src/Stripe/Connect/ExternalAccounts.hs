module Stripe.Connect.ExternalAccounts where

import Stripe.PaymentMethods.BankAccounts
import Stripe.Utils

-- createBankAccount

retrieveBankAccount :: (StripeMonad m) => Id () -> Id BankAccount -> m BankAccount
retrieveBankAccount (Id accountId) (Id bankAccountId) =
  jsonGet
    ("accounts/" <> encodeUtf8 accountId <> "/external_accounts/" <>
     encodeUtf8 bankAccountId)
    []

-- updateBankAccount
-- deleteBankAccount
-- listAllBankAccounts :: (StripeMonad m) => m (List BankAccount)
-- listAllBankAccounts
-- createCard
-- retrieveCard
-- updateCard
-- deleteCard
-- listAllCards
