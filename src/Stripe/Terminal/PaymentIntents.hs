module Stripe.Terminal.PaymentIntents where

import Stripe.Core
import Stripe.Customers
import Stripe.Utils

data PaymentIntent = PaymentIntent
  { paymentIntentId :: Id PaymentIntent
  , paymentIntentAllowedSourceTypes :: [Text]
  , paymentIntentAmount :: Integer
  , paymentIntentAmountCapturable :: Integer
  , paymentIntentAmountReceived :: Integer
  , paymentIntentApplication :: Maybe (Expandable ())
  , paymentIntentApplicationFeeAmount :: Maybe Integer
  , paymentIntentCancledAt :: Maybe Timestamp
  , paymentIntentCancellationReason :: Text
  , paymentIntentCaptureMethod :: Text
  , paymentIntentCharges :: List ()
  , paymentIntentClientSecret :: Maybe Text
  , paymentIntentConfirmationMethod :: Text
  , paymentIntentCreated :: Timestamp
  , paymentIntentCurrency :: CurrencyCode
  , paymentIntentCustomer :: Maybe (Id Customer)
  , paymentIntentDescription :: Maybe Text
  , paymentIntentLiveMode :: Bool
  , paymentIntentMetadata :: Metadata
  {-
  , paymentIntent
  , paymentIntent
  , paymentIntent
  , paymentIntent
  , paymentIntent
  , paymentIntent
  , paymentIntent
  , paymentIntent
  , paymentIntent
-}
  }

-- createPaymentIntent
-- retrievePaymentIntent
-- confirmPaymentIntent
-- capturePaymentIntent
-- cancelPaymentIntent
-- listAllPaymentIntents
