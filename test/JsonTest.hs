{-# LANGUAGE TypeApplications #-}
module JsonTest where
import Data.Aeson (Value, Result(..), fromJSON)
import Data.Aeson.QQ
import Stripe.Balance (Balance, BalanceTransaction)
import Stripe.Charges (Charge)
import Stripe.Billing.Coupons (Coupon)
import Stripe.Billing.Discounts (Discount)
import Stripe.Billing.Invoices (Invoice)
import Stripe.Billing.InvoiceItems (InvoiceItem)
import Stripe.Billing.Plans (Plan)
import Stripe.Billing.Products (Product)
import Stripe.Billing.Subscriptions (Subscription)
import Stripe.Billing.SubscriptionItems (SubscriptionItem)
import Stripe.Billing.UsageRecords (UsageRecord)
import Stripe.PaymentMethods.BankAccounts (BankAccount)
import Stripe.PaymentMethods.Cards (Card)
import Stripe.PaymentMethods.Sources (Source)
import Test.Tasty.Hspec

-- --------------------------------------------------------
-- Core Resources
-- --------------------------------------------------------
exampleBalance :: Value
exampleBalance = [aesonQQ|
{
  "object": "balance",
  "available": [
    {
      "currency": "usd",
      "amount": 12311683,
      "source_types": {
        "card": 12311683
      }
    }
  ],
  "livemode": false,
  "pending": [
    {
      "currency": "usd",
      "amount": 3144759,
      "source_types": {
        "card": 3144759
      }
    }
  ]
}
|]

spec_balance :: Spec
spec_balance = do
  specify "should successfully decode all JSON fields" $ do
    case fromJSON exampleBalance :: Result Balance of
      Error err -> expectationFailure err
      Data.Aeson.Success _ -> pure ()

exampleBalanceTransaction :: Value
exampleBalanceTransaction = [aesonQQ|
{
  "id": "txn_1DQm5AG8nEOA8lO2AQetngKM",
  "object": "balance_transaction",
  "amount": 19900,
  "available_on": 1541030400,
  "created": 1540863108,
  "currency": "usd",
  "description": "Payment for invoice 02716CE-0002",
  "exchange_rate": null,
  "fee": 607,
  "fee_details": [
    {
      "amount": 607,
      "application": null,
      "currency": "usd",
      "description": "Stripe processing fees",
      "type": "stripe_fee"
    }
  ],
  "net": 19293,
  "source": "ch_1DQm5AG8nEOA8lO2mo5sIOtg",
  "status": "available",
  "type": "charge"
}
|]

spec_balance_transaction :: Spec
spec_balance_transaction = do
  specify "should successfully decode all JSON fields" $ do
    case fromJSON exampleBalanceTransaction :: Result BalanceTransaction of
      Error err -> expectationFailure err
      Data.Aeson.Success _ -> pure ()

-- --------------------------------------------------------
-- Payment Methods
-- --------------------------------------------------------
exampleBankAccount :: Value
exampleBankAccount = [aesonQQ|
{
  "id": "ba_1DRUGFG8nEOA8lO20yc4BSj2",
  "object": "bank_account",
  "account_holder_name": "Jane Austen",
  "account_holder_type": "individual",
  "bank_name": "STRIPE TEST BANK",
  "country": "US",
  "currency": "usd",
  "customer": null,
  "fingerprint": "YWOvjbuOMsjXIL9v",
  "last4": "6789",
  "metadata": {
  },
  "routing_number": "110000000",
  "status": "new"
}
|]

spec_bank_account :: Spec
spec_bank_account = do
  specify "should successfully decode all JSON fields" $ do
    case fromJSON exampleBankAccount :: Result BankAccount of
      Error err -> expectationFailure err
      Data.Aeson.Success _ -> pure ()

exampleCard :: Value
exampleCard = [aesonQQ|
{
  "id": "card_1DRUGEG8nEOA8lO2TrTyn4To",
  "object": "card",
  "address_city": null,
  "address_country": null,
  "address_line1": null,
  "address_line1_check": null,
  "address_line2": null,
  "address_state": null,
  "address_zip": null,
  "address_zip_check": null,
  "brand": "Visa",
  "country": "US",
  "customer": null,
  "cvc_check": null,
  "dynamic_last4": null,
  "exp_month": 8,
  "exp_year": 2019,
  "fingerprint": "oIAQkFMx7gOSgN3g",
  "funding": "credit",
  "last4": "4242",
  "metadata": {
  },
  "name": null,
  "tokenization_method": null
}
|]

spec_card :: Spec
spec_card = do
  specify "should successfully decode all JSON fields" $ do
    case fromJSON exampleCard :: Result Card of
      Error err -> expectationFailure err
      Data.Aeson.Success _ -> pure ()

exampleSource :: Value
exampleSource = [aesonQQ|
{
  "id": "src_1DRUGFG8nEOA8lO2h20tljHZ",
  "object": "source",
  "ach_credit_transfer": {
    "account_number": "test_52796e3294dc",
    "routing_number": "110000000",
    "fingerprint": "ecpwEzmBOSMOqQTL",
    "bank_name": "TEST BANK",
    "swift_code": "TSTEZ122"
  },
  "amount": null,
  "client_secret": "src_client_secret_DtGnoUIGgf3jGLqNpW322GbG",
  "created": 1541032931,
  "currency": "usd",
  "flow": "receiver",
  "livemode": false,
  "metadata": {
  },
  "owner": {
    "address": null,
    "email": "jenny.rosen@example.com",
    "name": null,
    "phone": null,
    "verified_address": null,
    "verified_email": null,
    "verified_name": null,
    "verified_phone": null
  },
  "receiver": {
    "address": "121042882-38381234567890123",
    "amount_charged": 0,
    "amount_received": 0,
    "amount_returned": 0,
    "refund_attributes_method": "email",
    "refund_attributes_status": "missing"
  },
  "statement_descriptor": null,
  "status": "pending",
  "type": "ach_credit_transfer",
  "usage": "reusable"
}
|]

spec_source :: Spec
spec_source = do
  specify "should successfully decode all JSON fields" $ do
    case fromJSON exampleSource :: Result Source of
      Error err -> expectationFailure err
      Data.Aeson.Success _ -> pure ()

-- --------------------------------------------------------
-- Billing
-- --------------------------------------------------------
exampleCoupon :: Value
exampleCoupon = [aesonQQ|
{
  "id": "10xfactory",
  "object": "coupon",
  "amount_off": 15000,
  "created": 1514875158,
  "currency": "usd",
  "duration": "forever",
  "duration_in_months": null,
  "livemode": false,
  "max_redemptions": null,
  "metadata": {
  },
  "name": null,
  "percent_off": null,
  "redeem_by": null,
  "times_redeemed": 0,
  "valid": true
}
|]

spec_coupons :: Spec
spec_coupons = do
  specify "should successfully decode all JSON fields" $ do
    case fromJSON exampleCoupon :: Result Coupon of
      Error err -> expectationFailure err
      Data.Aeson.Success _ -> pure ()

exampleDiscount :: Value
exampleDiscount = [aesonQQ|
{
  "object": "discount",
  "coupon": {
    "id": "10xfactory",
    "object": "coupon",
    "amount_off": 15000,
    "created": 1514875158,
    "currency": "usd",
    "duration": "forever",
    "duration_in_months": null,
    "livemode": false,
    "max_redemptions": null,
    "metadata": {
    },
    "name": null,
    "percent_off": null,
    "redeem_by": null,
    "times_redeemed": 0,
    "valid": true
  },
  "customer": "cus_CfGedwq0SH7l9B",
  "end": null,
  "start": 1540953724,
  "subscription": null
}
|]

spec_discounts :: Spec
spec_discounts = do
  specify "should successfully decode all JSON fields" $ do
    case fromJSON exampleDiscount :: Result Discount of
      Error err -> expectationFailure err
      Data.Aeson.Success _ -> pure ()

exampleInvoiceItem :: Value
exampleInvoiceItem = [aesonQQ|
{
  "id": "ii_1D4MxHG8nEOA8lO2aVfkSnti",
  "object": "invoiceitem",
  "amount": 19900,
  "currency": "usd",
  "customer": "cus_CfGedwq0SH7l9B",
  "date": 1535523303,
  "description": "Unused time on Unlimited - 10 Jobs - Annually - 20180102 after 29 Aug 2018",
  "discountable": false,
  "invoice": "in_1D4MxHG8nEOA8lO2BOn9BpAf",
  "livemode": false,
  "metadata": {
  },
  "period": {
    "start": 1535523303,
    "end": 1567059246
  },
  "plan": {
    "id": "unlimited-annually-20180102",
    "object": "plan",
    "active": true,
    "aggregate_usage": null,
    "amount": 42600,
    "billing_scheme": "per_unit",
    "created": 1514871625,
    "currency": "usd",
    "interval": "year",
    "interval_count": 1,
    "livemode": false,
    "metadata": {
    },
    "nickname": null,
    "product": "prod_C3qCL7d51wPKsh",
    "tiers": null,
    "tiers_mode": null,
    "transform_usage": null,
    "trial_period_days": null,
    "usage_type": "licensed"
  },
  "proration": true,
  "quantity": 1,
  "subscription": "sub_DVNhgyRAEzE22m",
  "subscription_item": "si_DVNhektg1PLDID",
  "unit_amount": 19900
}
|]

spec_invoice_items :: Spec
spec_invoice_items = do
  specify "should successfully decode all JSON fields" $ do
     case fromJSON exampleInvoiceItem :: Result InvoiceItem of
      Error err -> expectationFailure err
      Data.Aeson.Success _ -> pure ()

exampleInvoice :: Value
exampleInvoice = [aesonQQ|
{
  "id": "in_1DRBKRG8nEOA8lO2zmf7MfHE",
  "object": "invoice",
  "amount_due": 19900,
  "amount_paid": 0,
  "amount_remaining": 19900,
  "application_fee": null,
  "attempt_count": 0,
  "attempted": false,
  "auto_advance": true,
  "billing": "charge_automatically",
  "billing_reason": "subscription_cycle",
  "charge": null,
  "closed": false,
  "currency": "usd",
  "customer": "cus_CfGedwq0SH7l9B",
  "date": 1540960155,
  "description": "",
  "discount": null,
  "due_date": null,
  "ending_balance": null,
  "forgiven": false,
  "hosted_invoice_url": null,
  "invoice_pdf": null,
  "lines": {
    "data": [
      {
        "id": "sli_4c7f9ef08f6ca8",
        "object": "line_item",
        "amount": 7100,
        "currency": "usd",
        "description": "1 × Unlimited - 10 Jobs - Monthly - 20180102 (at $71.00 / month)",
        "discountable": true,
        "livemode": false,
        "metadata": {
        },
        "period": {
          "end": 1543730399,
          "start": 1541138399
        },
        "plan": {
          "id": "unlimited-monthly-20180102",
          "object": "plan",
          "active": true,
          "aggregate_usage": null,
          "amount": 7100,
          "billing_scheme": "per_unit",
          "created": 1514871624,
          "currency": "usd",
          "interval": "month",
          "interval_count": 1,
          "livemode": false,
          "metadata": {
          },
          "nickname": null,
          "product": "prod_C3qCByeLNhEaMI",
          "tiers": null,
          "tiers_mode": null,
          "transform_usage": null,
          "trial_period_days": null,
          "usage_type": "licensed"
        },
        "proration": false,
        "quantity": 1,
        "subscription": "sub_C3qVxQ4j9VIJyR",
        "subscription_item": "si_C3qV95FkGjyDoY",
        "type": "subscription"
      }
    ],
    "has_more": false,
    "object": "list",
    "url": "/v1/invoices/in_1DRBKRG8nEOA8lO2zmf7MfHE/lines"
  },
  "livemode": false,
  "metadata": {
  },
  "next_payment_attempt": 1540963755,
  "number": "CABE86D-0004",
  "paid": false,
  "period_end": 1540959979,
  "period_start": 1538281579,
  "receipt_number": null,
  "starting_balance": 0,
  "statement_descriptor": null,
  "subscription": "sub_DKUP5OtBVHtqMu",
  "subtotal": 19900,
  "tax": 0,
  "tax_percent": null,
  "total": 19900,
  "webhooks_delivered_at": 1540960156
}
|]

spec_invoices :: Spec
spec_invoices = do
  specify "should successfully decode all JSON fields" $ do
     case fromJSON exampleInvoice :: Result Invoice of
      Error err -> expectationFailure err
      Data.Aeson.Success _ -> pure ()

examplePlan :: Value
examplePlan = [aesonQQ|
{
  "id": "solo-monthly-20180102",
  "object": "plan",
  "active": true,
  "aggregate_usage": null,
  "amount": 1900,
  "billing_scheme": "per_unit",
  "created": 1514871620,
  "currency": "usd",
  "interval": "month",
  "interval_count": 1,
  "livemode": false,
  "metadata": {
  },
  "nickname": null,
  "product": "prod_C3qCuG4AHiXV2m",
  "tiers": null,
  "tiers_mode": null,
  "transform_usage": null,
  "trial_period_days": null,
  "usage_type": "licensed"
}
|]

spec_plans :: Spec
spec_plans = do
  specify "should successfully decode all JSON fields" $ do
     case fromJSON examplePlan :: Result Plan of
      Error err -> expectationFailure err
      Data.Aeson.Success _ -> pure ()

exampleProduct :: Value
exampleProduct = [aesonQQ|
{
  "id": "prod_C3qCt13L4zlbem",
  "object": "product",
  "active": true,
  "attributes": [

  ],
  "caption": null,
  "created": 1514871644,
  "deactivate_on": [

  ],
  "description": null,
  "images": [

  ],
  "livemode": false,
  "metadata": {
  },
  "name": "Enterprise Plan - 50 Jobs - Monthly",
  "package_dimensions": null,
  "shippable": null,
  "statement_descriptor": null,
  "type": "service",
  "unit_label": null,
  "updated": 1514871644,
  "url": null
}
|]

spec_products :: Spec
spec_products = do
  specify "should successfully decode all JSON fields" $ do
     case fromJSON exampleProduct :: Result Product of
      Error err -> expectationFailure err
      Data.Aeson.Success _ -> pure ()

exampleSubscriptionItem :: Value
exampleSubscriptionItem = [aesonQQ|
{
  "id": "si_DsvV4LqwSXMv6P",
  "object": "subscription_item",
  "created": 1540953725,
  "metadata": {
  },
  "plan": {
    "id": "solo-monthly-20180102",
    "object": "plan",
    "active": true,
    "aggregate_usage": null,
    "amount": 1900,
    "billing_scheme": "per_unit",
    "created": 1514871620,
    "currency": "usd",
    "interval": "month",
    "interval_count": 1,
    "livemode": false,
    "metadata": {
    },
    "nickname": null,
    "product": "prod_C3qCuG4AHiXV2m",
    "tiers": null,
    "tiers_mode": null,
    "transform_usage": null,
    "trial_period_days": null,
    "usage_type": "licensed"
  },
  "quantity": 1,
  "subscription": "sub_DsvVeYkwbgDTFO"
}
|]

spec_subscription_items :: Spec
spec_subscription_items = do
  specify "should successfully decode all JSON fields" $ do
     case fromJSON exampleSubscriptionItem :: Result SubscriptionItem of
      Error err -> expectationFailure err
      Data.Aeson.Success _ -> pure ()

exampleSubscription :: Value
exampleSubscription = [aesonQQ|
{
  "id": "sub_C3qVxQ4j9VIJyR",
  "object": "subscription",
  "application_fee_percent": null,
  "billing": "charge_automatically",
  "billing_cycle_anchor": 1514872799,
  "cancel_at_period_end": false,
  "canceled_at": null,
  "created": 1514872799,
  "current_period_end": 1541138399,
  "current_period_start": 1538459999,
  "customer": "cus_C3qVwQEVxDXVcO",
  "days_until_due": null,
  "discount": null,
  "ended_at": null,
  "items": {
    "object": "list",
    "data": [
      {
        "id": "si_C3qV95FkGjyDoY",
        "object": "subscription_item",
        "created": 1514872800,
        "metadata": {
        },
        "plan": {
          "id": "unlimited-monthly-20180102",
          "object": "plan",
          "active": true,
          "aggregate_usage": null,
          "amount": 7100,
          "billing_scheme": "per_unit",
          "created": 1514871624,
          "currency": "usd",
          "interval": "month",
          "interval_count": 1,
          "livemode": false,
          "metadata": {
          },
          "nickname": null,
          "product": "prod_C3qCByeLNhEaMI",
          "tiers": null,
          "tiers_mode": null,
          "transform_usage": null,
          "trial_period_days": null,
          "usage_type": "licensed"
        },
        "quantity": 1,
        "subscription": "sub_C3qVxQ4j9VIJyR"
      }
    ],
    "has_more": false,
    "total_count": 1,
    "url": "/v1/subscription_items?subscription=sub_C3qVxQ4j9VIJyR"
  },
  "livemode": false,
  "metadata": {
  },
  "plan": {
    "id": "unlimited-monthly-20180102",
    "object": "plan",
    "active": true,
    "aggregate_usage": null,
    "amount": 7100,
    "billing_scheme": "per_unit",
    "created": 1514871624,
    "currency": "usd",
    "interval": "month",
    "interval_count": 1,
    "livemode": false,
    "metadata": {
    },
    "nickname": null,
    "product": "prod_C3qCByeLNhEaMI",
    "tiers": null,
    "tiers_mode": null,
    "transform_usage": null,
    "trial_period_days": null,
    "usage_type": "licensed"
  },
  "quantity": 1,
  "start": 1514872799,
  "status": "active",
  "tax_percent": null,
  "trial_end": null,
  "trial_start": null
}
|]

spec_subscriptions :: Spec
spec_subscriptions = do
  specify "should successfully decode all JSON fields" $ do
     case fromJSON exampleSubscription :: Result Subscription of
      Error err -> expectationFailure err
      Data.Aeson.Success _ -> pure ()

exampleUsageRecord :: Value
exampleUsageRecord = [aesonQQ|
{
  "id": "mbur_1DR9eiG8nEOA8lO2PWIOpQt2",
  "object": "usage_record",
  "livemode": false,
  "quantity": 100,
  "subscription_item": "si_DsvVpB0oKOR5pI",
  "timestamp": 1540953724
}
|]

spec_usage_records :: Spec
spec_usage_records = do
  specify "should successfully decode all JSON fields" $ do
     case fromJSON exampleUsageRecord :: Result UsageRecord of
      Error err -> expectationFailure err
      Data.Aeson.Success _ -> pure ()
{-
exampleBalance :: Value
exampleBalance = [aesonQQ|
{
  "object": "balance",
  "available": [
    {
      "currency": "usd",
      "amount": 7006108,
      "source_types": {
        "card": 7006108
      }
    }
  ],
  "livemode": false,
  "pending": [
    {
      "currency": "usd",
      "amount": 5807193,
      "source_types": {
        "card": 5807193
      }
    }
  ]
}
|]

spec_balance :: Spec
spec_balance = do
  specify "should successfully decode all JSON fields" $ do
     case fromJSON exampleBalance :: Result Balance of
      Error err -> expectationFailure err
      Data.Aeson.Success _ -> pure ()
-}

exampleCharge :: Value
exampleCharge = [aesonQQ|
{
  "id": "ch_1D9v37G8nEOA8lO2Cq6Oznr1",
  "object": "charge",
  "amount": 19900,
  "amount_refunded": 0,
  "application": null,
  "application_fee": null,
  "balance_transaction": "txn_1DPgiaG8nEOA8lO2ctqltqFh",
  "captured": true,
  "created": 1536846001,
  "currency": "usd",
  "customer": "cus_DPUImCueugPsCX",
  "description": null,
  "destination": null,
  "dispute": null,
  "failure_code": null,
  "failure_message": null,
  "fraud_details": {
  },
  "invoice": "in_1D9u6vG8nEOA8lO2LpmjSu7V",
  "livemode": false,
  "metadata": {
  },
  "on_behalf_of": null,
  "order": null,
  "outcome": {
    "network_status": "approved_by_network",
    "reason": null,
    "risk_level": "normal",
    "risk_score": 27,
    "seller_message": "Payment complete.",
    "type": "authorized"
  },
  "paid": true,
  "payment_intent": null,
  "receipt_email": null,
  "receipt_number": null,
  "refunded": false,
  "refunds": {
    "object": "list",
    "data": [

    ],
    "has_more": false,
    "total_count": 0,
    "url": "/v1/charges/ch_1D9v37G8nEOA8lO2Cq6Oznr1/refunds"
  },
  "review": null,
  "shipping": null,
  "source": {
    "id": "card_1CyfJfG8nEOA8lO2SqOULUtf",
    "object": "card",
    "address_city": null,
    "address_country": null,
    "address_line1": null,
    "address_line1_check": null,
    "address_line2": null,
    "address_state": null,
    "address_zip": null,
    "address_zip_check": null,
    "brand": "Visa",
    "country": "US",
    "customer": "cus_DPUImCueugPsCX",
    "cvc_check": null,
    "dynamic_last4": null,
    "exp_month": 12,
    "exp_year": 2019,
    "fingerprint": "oIAQkFMx7gOSgN3g",
    "funding": "credit",
    "last4": "4242",
    "metadata": {
    },
    "name": null,
    "tokenization_method": null
  },
  "source_transfer": null,
  "statement_descriptor": null,
  "status": "succeeded",
  "transfer_group": null
}
|]

spec_charges :: Spec
spec_charges = do
  specify "should successfully decode all JSON fields" $ do
     case fromJSON exampleCharge :: Result Charge of
      Error err -> expectationFailure err
      Data.Aeson.Success _ -> pure ()

-- --------------------------------------------------------
-- Connect
-- --------------------------------------------------------

-- --------------------------------------------------------
-- Fraud
-- --------------------------------------------------------

-- --------------------------------------------------------
-- Issuing
-- --------------------------------------------------------

-- --------------------------------------------------------
-- Terminal
-- --------------------------------------------------------

-- --------------------------------------------------------
-- Orders
-- --------------------------------------------------------

-- --------------------------------------------------------
-- Sigma
-- --------------------------------------------------------
