{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
module Stripe.Lens where

import Control.Lens.TH

import Stripe.Balance
import Stripe.Charges
import Stripe.Core
import Stripe.Customers
import Stripe.Disputes
import Stripe.Files
import Stripe.FilesLinks
import Stripe.Orders
import Stripe.Payouts
import Stripe.Refunds
import Stripe.Tokens
import Stripe.Billing.Coupons
import Stripe.Billing.Discounts
import Stripe.Billing.InvoiceItems
import Stripe.Billing.Invoices
import Stripe.Billing.Plans
import Stripe.Billing.Products
import Stripe.Billing.SubscriptionItems
import Stripe.Billing.Subscriptions
import Stripe.Billing.UsageRecords
import Stripe.Connect.Account
import Stripe.Connect.ApplicationFeeRefunds
import Stripe.Connect.ApplicationFees
import Stripe.Connect.CountrySpecs
import Stripe.Connect.ExternalAccounts
import Stripe.Connect.TopUps
import Stripe.Connect.TransferReversals
import Stripe.Connect.Transfers
import Stripe.Fraud.RadarReviews
import Stripe.Issuing.Authorizations
import Stripe.Issuing.Cardholders
import Stripe.Issuing.Cards
import Stripe.Issuing.Disputes
import Stripe.Issuing.Transactions
import Stripe.Orders.OrderItems
import Stripe.Orders.Returns
import Stripe.Orders.SKUs
import Stripe.PaymentMethods.BankAccounts
import Stripe.PaymentMethods.Cards
import Stripe.PaymentMethods.Sources
import Stripe.Sigma.ScheduledQueries
import Stripe.Terminal.ConnectionTokens
import Stripe.Terminal.Locations
import Stripe.Terminal.PaymentIntents
import Stripe.Terminal.Readers
import Stripe.Utils

-- Stripe.Utils
makeFields ''List

-- Stripe.Balance
makeFields ''BalanceFunds
makeFields ''Balance
makeFields ''FeeDetails
makeFields ''BalanceTransaction

-- Stripe.Charges
makeFields ''FraudDetails
makeFields ''Outcome
makeFields ''Charge

-- Stripe.Customers
makeFields ''Customer

-- Stripe.Disputes
makeFields ''Evidence
makeFields ''EvidenceDetails
makeFields ''Dispute

-- Stripe.Events TODO

-- Stripe.Files
makeFields ''File

-- Stripe.FileLinks
makeFields ''FileLink

-- Stripe.Orders
makeFields ''OrderItem
makeFields ''StatusTransitions
makeFields ''ShippingDetails
makeFields ''ExactDelivery
makeFields ''RangeDelivery
makeFields ''ShippingMethod
makeFields ''Order

-- Stripe.Payouts
makeFields ''Payout

-- Stripe.Refunds
makeFields ''Refund

-- Stripe.Tokens
makeFields ''Token

-- Stripe.Billing.Coupons
makeFields ''Coupon

-- Stripe.Billing.Discounts
makeFields ''Discount

-- Stripe.Billing.InvoiceItems
makeFields ''InvoiceItem

-- Stripe.Billing.Invoices
makeFields ''InvoiceLinePeriod
makeFields ''InvoiceLine
makeFields ''Invoice

-- Stripe.Billing.Plans
makeFields ''Tier
makeFields ''TransformUsage
makeFields ''Plan

-- Stripe.Billing.Products
makeFields ''PackageDimensions
makeFields ''Product

-- Stripe.Billing.SubscriptionItems
makeFields ''SubscriptionItem

-- Stripe.Billing.Subscriptions
makeFields ''Subscription

-- Stripe.Billing.UsageRecords
makeFields ''UsageRecord

-- Stripe.Connect.Account
makeFields ''DeclineChargeOn
makeFields ''Account

-- Stripe.Connect.ApplicationFeeRefunds
makeFields ''FeeRefund

-- Stripe.Connect.ApplicationFees
makeFields ''Fee

-- Stripe.Connect.CountrySpecs
makeFields ''CountrySpec
makeFields ''VerificationFields
makeFields ''VerificationFieldSpec

-- Stripe.Connect.ExternalAccounts

-- Stripe.Connect.TopUps
-- Stripe.Connect.TransferReversals
makeFields ''TransferReversal

-- Stripe.Connect.Transfers
makeFields ''Transfer

-- Stripe.Fraud.RadarReviews
-- Stripe.Issuing.Authorizations
-- Stripe.Issuing.Cardholders
-- Stripe.Issuing.Cards
-- Stripe.Issuing.Disputes
-- Stripe.Issuing.Transactions
-- Stripe.Orders.OrderItems
-- Stripe.Orders.Returns
-- Stripe.Orders.SKUs
-- Stripe.PaymentMethods.BankAccounts
makeFields ''BankAccount

-- Stripe.PaymentMethods.Cards
makeFields ''Card

-- Stripe.PaymentMethods.Sources
makeFields ''CodeVerification
makeFields ''Owner
makeFields ''Redirect
makeFields ''Receiver
makeFields ''Source

-- Stripe.Sigma.ScheduledQueries
-- Stripe.Terminal.ConnectionTokens
-- Stripe.Terminal.Locations
-- Stripe.Terminal.PaymentIntents
-- Stripe.Terminal.Readers
