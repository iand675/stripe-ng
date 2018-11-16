module Stripe.Errors where

import Control.Exception
import Data.Aeson
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import GHC.Generics
import Data.Typeable
import qualified Data.HashMap.Strict as H
import Network.HTTP.Types
import qualified Data.ByteString.Lazy as L

data StripeErrorType
  = ApiConnectionError
  | ApiError
  | AuthenticationError
  | CardError
  | IdempotencyError
  | InvalidRequestError
  | RateLimitError
  | ValidationError
  deriving (Show, Eq, Generic, Typeable)

instance FromJSON StripeErrorType where
  parseJSON = withText "StripeErrorType" $ \e -> case e of
    "api_connection_error" -> pure ApiConnectionError
    "api_error" -> pure ApiError
    "authentication_error" -> pure AuthenticationError
    "card_error" -> pure CardError
    "idempotency_error" -> pure IdempotencyError
    "invalid_request_error" -> pure InvalidRequestError
    "rate_limit_error" -> pure RateLimitError
    "validation_error" -> pure ValidationError
    _ -> fail (show e <> " is not a recognized error type.")

data DeclineCode
  = ApproveWithId
  | CallIssuer
  | CardNotSupported
  | CardVelocityExceeded
  | CurrencyNotSupported
  | DoNotHonor
  | DoNotTryAgain
  | DuplicateTransaction
  | ExpiredCardDecline
  | Fraudulent
  | GenericDecline
  | IncorrectCvcDecline
  | IncorrectPinDecline
  | IncorrectZipDecline
  | InsufficientFunds
  | InvalidAccount
  | InvalidAmount
  | InvalidCvcDecline
  | InvalidExpiryYearDecline
  | InvalidNumberDecline
  | InvalidPin
  | IssuerNotAvailable
  | LostCard
  | MerchantBlacklist
  | NewAccountInformationAvailable
  | NoActionTaken
  | NotPermitted
  | PickupCard
  | PinTryExceeded
  | ProcessingErrorDecline
  | ReenterTransaction
  | RestrictedCard
  | RevocationOfAllAuthorizations
  | RevocationOfAuthorization
  | SecurityViolation
  | ServiceNotAllowed
  | StolenCard
  | StopPaymentOrder
  | TestmodeDecline
  | TransactionNotALlowed
  | TryAgainLater
  | WithdrawalCountLimitExceeded
  | OtherDecline Text
  deriving (Show, Eq, Generic, Typeable)

data ErrorCode
  = AccountAlreadyExists
  | AccountCountryInvalidAddress
  | AccountInvalid
  | AccountNumberInvalid
  | AlipayUpgradeRequired
  | AmountTooLarge
  | ApiKeyExpired
  | BalanceInsufficient
  | BankAccountExists
  | BankAccountUnusable
  | BankAccountUnverified
  | BitcoinUpgradeRequired
  | CardDeclined
  | ChargeAlreadyCaptured
  | ChargeAlreadyRefunded
  | ChargeDisputed
  | ChargeExceedsSourceLimit
  | ChargeExpiredForCapture
  | CountryUnsupported
  | CouponExpired
  | CustomerMaxSubscriptions
  | EmailInvalid
  | ExpiredCard
  | IdempotencyKeyInUse
  | IncorrectAddress
  | IncorrectCvc
  | IncorrectNumber
  | IncorrectZip
  | InstantPayoutsUnsupported
  | InvalidCardType
  | InvalidChargeAmount
  | InvalidCvc
  | InvalidExpiryMonth
  | InvalidExpiryYear
  | InvalidNumber
  | InvalidSourceUsage
  | InvoiceNoCustomerLineItems
  | InvoiceNoSubscriptionLineItems
  | InvoiceNotEditable
  | InvoiceUpcomingNone
  | LivemodeMismatch
  | Missing
  | NotAllowedOnStandardAccount
  | OrderCreationFailed
  | OrderRequiredSettings
  | OrderStatusInvalid
  | OrderUpstreamTimeout
  | OutOfInventory
  | ParameterInvalidEmpty
  | ParameterInvalidInteger
  | ParameterInvalidStringBlank
  | ParameterInvalidStringEmpty
  | ParameterMissing
  | ParameterUnknown
  | PaymentIntentAuthenticationFailure
  | PaymentIntentUnexpectedState
  | PaymentMethodUnactivated
  | PayoutsNotAllowed
  | PlatformApiKeyExpired
  | PostalCodeInvalid
  | ProcessingError
  | ProductInactive
  | RateLimit
  | ResourceAlreadyExists
  | ResourceMissing
  | RoutingNumberInvalid
  | SecretKeyRequired
  | SepaUnsupportedAccount
  | ShippingCalculationFailed
  | SkuInactive
  | StateUnsupported
  | TaxIdInvalid
  | TaxesCalculationFailed
  | TestmodeChargesOnly
  | TlsVersionUnsupported
  | TokenAlreadyUsed
  | TokenInUse
  | TransfersNotAllowed
  | UpstreamOrderCreationFailed
  | UrlInvalid
  | UnknownErrorCode Text
  deriving (Show, Eq, Generic, Typeable)

instance FromJSON ErrorCode where
  parseJSON = withText "ErrorCode" (pure . lookupErrorCode)

lookupErrorCode :: Text -> ErrorCode
lookupErrorCode t = fromMaybe (UnknownErrorCode t) $ H.lookup t errorCodeMap

errorCodeMap :: H.HashMap Text ErrorCode
errorCodeMap = H.fromList
  [ ("account_already_exists", AccountAlreadyExists)
  , ("account_country_invalid_address", AccountCountryInvalidAddress)
  , ("account_invalid", AccountInvalid)
  , ("account_number_invalid", AccountNumberInvalid)
  , ("alipay_upgrade_required", AlipayUpgradeRequired)
  , ("amount_too_large", AmountTooLarge)
  , ("api_key_expired", ApiKeyExpired)
  , ("balance_insufficient", BalanceInsufficient)
  , ("bank_account_exists", BankAccountExists)
  , ("bank_account_unusable", BankAccountUnusable)
  , ("bank_account_unverified", BankAccountUnverified)
  , ("bitcoin_upgrade_required", BitcoinUpgradeRequired)
  , ("card_declined", CardDeclined)
  , ("charge_already_captured", ChargeAlreadyCaptured)
  , ("charge_already_refunded", ChargeAlreadyRefunded)
  , ("charge_disputed", ChargeDisputed)
  , ("charge_exceeds_source_limit", ChargeExceedsSourceLimit)
  , ("charge_expired_for_capture", ChargeExpiredForCapture)
  , ("country_unsupported", CountryUnsupported)
  , ("coupon_expired", CouponExpired)
  , ("customer_max_subscriptions", CustomerMaxSubscriptions)
  , ("email_invalid", EmailInvalid)
  , ("expired_card", ExpiredCard)
  , ("idempotency_key_in_use", IdempotencyKeyInUse)
  , ("incorrect_address", IncorrectAddress)
  , ("incorrect_cvc", IncorrectCvc)
  , ("incorrect_number", IncorrectNumber)
  , ("incorrect_zip", IncorrectZip)
  , ("instant_payouts_unsupported", InstantPayoutsUnsupported)
  , ("invalid_card_type", InvalidCardType)
  , ("invalid_charge_amount", InvalidChargeAmount)
  , ("invalid_cvc", InvalidCvc)
  , ("invalid_expiry_month", InvalidExpiryMonth)
  , ("invalid_expiry_year", InvalidExpiryYear)
  , ("invalid_number", InvalidNumber)
  , ("invalid_source_usage", InvalidSourceUsage)
  , ("invoice_no_customer_line_items", InvoiceNoCustomerLineItems)
  , ("invoice_no_subscription_line_items", InvoiceNoSubscriptionLineItems)
  , ("invoice_not_editable", InvoiceNotEditable)
  , ("invoice_upcoming_none", InvoiceUpcomingNone)
  , ("livemode_mismatch", LivemodeMismatch)
  , ("missing", Missing)
  , ("not_allowed_on_standard_account", NotAllowedOnStandardAccount)
  , ("order_creation_failed", OrderCreationFailed)
  , ("order_required_settings", OrderRequiredSettings)
  , ("order_status_invalid", OrderStatusInvalid)
  , ("order_upstream_timeout", OrderUpstreamTimeout)
  , ("out_of_inventory", OutOfInventory)
  , ("parameter_invalid_empty", ParameterInvalidEmpty)
  , ("parameter_invalid_integer", ParameterInvalidInteger)
  , ("parameter_invalid_string_blank", ParameterInvalidStringBlank)
  , ("parameter_invalid_string_empty", ParameterInvalidStringEmpty)
  , ("parameter_missing", ParameterMissing)
  , ("parameter_unknown", ParameterUnknown)
  , ("payment_intent_authentication_failure", PaymentIntentAuthenticationFailure)
  , ("payment_intent_unexpected_state", PaymentIntentUnexpectedState)
  , ("payment_method_unactivated", PaymentMethodUnactivated)
  , ("payouts_not_allowed", PayoutsNotAllowed)
  , ("platform_api_key_expired", PlatformApiKeyExpired)
  , ("postal_code_invalid", PostalCodeInvalid)
  , ("processing_error", ProcessingError)
  , ("product_inactive", ProductInactive)
  , ("rate_limit", RateLimit)
  , ("resource_already_exists", ResourceAlreadyExists)
  , ("resource_missing", ResourceMissing)
  , ("routing_number_invalid", RoutingNumberInvalid)
  , ("secret_key_required", SecretKeyRequired)
  , ("sepa_unsupported_account", SepaUnsupportedAccount)
  , ("shipping_calculation_failed", ShippingCalculationFailed)
  , ("sku_inactive", SkuInactive)
  , ("state_unsupported", StateUnsupported)
  , ("tax_id_invalid", TaxIdInvalid)
  , ("taxes_calculation_failed", TaxesCalculationFailed)
  , ("testmode_charges_only", TestmodeChargesOnly)
  , ("tls_version_unsupported", TlsVersionUnsupported)
  , ("token_already_used", TokenAlreadyUsed)
  , ("token_in_use", TokenInUse)
  , ("transfers_not_allowed", TransfersNotAllowed)
  , ("upstream_order_creation_failed", UpstreamOrderCreationFailed)
  , ("url_invalid", UrlInvalid)
  ]

data ErrorContext = ErrorContext
  { errorContextStatus :: Status
  , errorContextResponseHeaders :: ResponseHeaders
  , errorContextFullBody :: L.ByteString
  } deriving (Show, Eq, Generic, Typeable)

data StripeError = StripeError
  { stripeErrorType :: StripeErrorType
  -- TODO
  -- , stripeErrorCharge :: Maybe (Id Charge) -- ^ For card errors, the ID of the failed charge.
  , stripeErrorCode :: Maybe ErrorCode -- ^ For some errors that could be handled programmatically, a short string indicating the error code reported.
  , stripeErrorDeclineCode :: Maybe Text -- ^ For card errors resulting from a card issuer decline, a short string indicating the card issuer’s reason for the decline if they provide one.
  , stripeErrorDocUrl :: Maybe Text -- ^ A URL to more information about the error code reported.
  , stripeErrorMessage :: Maybe Text -- ^ A human-readable message providing more details about the error. For card errors, these messages can be shown to your users.
  , stripeErrorParam :: Maybe Text -- ^ If the error is parameter-specific, the parameter related to the error. For example, you can use this to display a message near the correct form field.
  , stripeErrorSource :: Maybe Object -- ^ The source object for errors returned on a request involving a source.
  , stripeErrorContext :: ErrorContext
  } deriving (Show, Eq, Generic, Typeable)

instance Exception StripeError

instance FromJSON StripeError where
  parseJSON = withObject "StripeError" $ \o -> do
    e <- o .: "error"
    StripeError
      <$> e .: "type"
      -- <*> e .: "charge"
      <*> e .:? "code"
      <*> e .:? "decline_code"
      <*> e .:? "doc_url"
      <*> e .:? "message"
      <*> e .:? "param"
      <*> e .:? "source"
      <*> pure (ErrorContext (Status 0 "Unknown") [] "")

data StripeJsonError = StripeJsonError String
  deriving (Show, Eq, Generic, Typeable)

instance Exception StripeJsonError

class AsStripeError e where
  fromStripeError :: StripeError -> e
  toStripeError :: e -> Maybe StripeError

instance AsStripeError StripeError where
  fromStripeError e = e
  toStripeError = pure
