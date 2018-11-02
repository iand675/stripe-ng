{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}
module Stripe.Event where
import Data.Aeson
import Data.Text (Text)
import Stripe.Utils

newtype EventType a = EventType { fromEventType :: Text }

newtype ChangedAttributes a = ChangedAttributes { fromChangedAttributes :: Object }

data EventData a = EventData
  { eventDataObject :: a
  , eventDataPreviousAttributes :: Maybe (ChangedAttributes a)
  }

newtype RequestId = RequestId Text
newtype IdempotencyKey = IdempotencyKey Text

data EventRequest = EventRequest
  { eventRequestId :: Maybe RequestId
  , eventRequestIdempotencyKey :: Maybe IdempotencyKey
  }

data Event a = Event
  { eventId :: Id (Event a)
  , eventObject :: Text
  , eventApiVersion :: Text
  , eventData :: EventData a
  , eventLivemode :: Bool
  , eventPendingWebhooks :: Word
  , eventRequest :: EventRequest
  , eventType :: Text
  }

{-
{
  "object": {
    "id": "sub_CCIwSga6rdU9QA",
     "plan": {
       "id": "solo-monthly-20180102",
       "name": "Solo - 1 Job - Monthly - 20180102",
       "tiers": null,
       "active": true,
       "amount": 1900,
       "object": "plan",
       "created": 1514852049,
       "product": "prod_C3kvvHzPAOGWN8",
       "currency": "usd",
       "interval": "month",
       "livemode": true,
       "metadata": {},
       "nickname": null,
       "tiers_mode": null,
       "usage_type": "licensed",
       "billing_scheme": "per_unit",
       "interval_count": 1,
       "aggregate_usage": null,
       "transform_usage": null,
       "trial_period_days": null,
       "statement_descriptor": null
    },
    "items": {
      "url": "/v1/subscription_items?subscription=sub_CCIwSga6rdU9QA",
      "data": [
        {"id": "si_CCIwr5OpU9VTsA", "plan": {"id": "solo-monthly-20180102", "name": "Solo - 1 Job - Monthly - 20180102", "tiers": null, "active": true, "amount": 1900, "object": "plan", "created": 1514852049, "product": "prod_C3kvvHzPAOGWN8", "currency": "usd", "interval": "month", "livemode": true, "metadata": {}, "nickname": null, "tiers_mode": null, "usage_type": "licensed", "billing_scheme": "per_unit", "interval_count": 1, "aggregate_usage": null, "transform_usage": null, "trial_period_days": null, "statement_descriptor": null}, "object": "subscription_item", "created": 1516823681, "metadata": {}, "quantity": 1, "subscription": "sub_CCIwSga6rdU9QA"}
      ],
      "object": "list",
      "has_more": false,
      "total_count": 1
    },
    "start": 1516823681,
    "object": "subscription",
    "status": "active",
    "billing": "charge_automatically",
    "created": 1516823681,
    "customer": "cus_CCIw2iuU7zy7pp",
    "discount": null,
    "ended_at": null,
    "livemode": true,
    "metadata": {},
    "quantity": 1,
    "trial_end": null,
    "canceled_at": null,
    "tax_percent": null,
    "trial_start": null,
    "days_until_due": null,
    "current_period_end": 1543089281,
    "billing_cycle_anchor": 1516823681,
    "cancel_at_period_end": false,
    "current_period_start": 1540410881,
    "application_fee_percent": null
  },
  "previous_attributes": {"status": "past_due"}
}
-}
data SomeEvent = forall a. SomeEvent (Event a)


data EventDetails a where

  AccountUpdated :: EventDetails a
-- account.updated
-- ^ describes an account
--   Occurs whenever an account status or property has changed.

  AccountApplicationAuthorized :: EventDetails a
-- account.application.authorized
-- ^ describes an "application"
--   Occurs whenever a user authorizes an application. Sent to the related application only.

  AccountApplicationDeauthorized :: EventDetails a
-- account.application.deauthorized
-- ^ describes an "application"
--   Occurs whenever a user deauthorizes an application. Sent to the related application only.

  AccountExternalAccountCreated :: EventDetails a
-- account.external_account.created
-- ^ describes an external account (e.g., card or bank account)
--   Occurs whenever an external account is created.

  AccountExternalAccountDeleted :: EventDetails a
-- account.external_account.deleted
-- ^ describes an external account (e.g., card or bank account)
--   Occurs whenever an external account is deleted.

  AccountExternalAccountUpdated :: EventDetails a
-- account.external_account.updated
-- ^ describes an external account (e.g., card or bank account)
--   Occurs whenever an external account is updated.

  ApplicationFeeCreated :: EventDetails a
-- application_fee.created
-- ^ describes an application fee
--   Occurs whenever an application fee is created on a charge.

  ApplicationFeeRefunded :: EventDetails a
-- application_fee.refunded
-- ^ describes an application fee
--   Occurs whenever an application fee is refunded, whether from refunding a charge or from refunding the application fee directly. This includes partial refunds.

  ApplicationFeeRefundUpdated :: EventDetails a
-- application_fee.refund.updated
-- ^ describes a fee refund
--   Occurs whenever an application fee refund is updated.

  BalanceAvailable :: EventDetails a
-- balance.available
-- ^ describes a balance
--   Occurs whenever your Stripe balance has been updated (e.g., when a charge is available to be paid out). By default, Stripe automatically transfers funds in your balance to your bank account on a daily basis.

  ChargeCaptured :: EventDetails a
-- charge.captured
-- ^ describes a charge
--   Occurs whenever a previously uncaptured charge is captured.

  ChargeExpired :: EventDetails a
-- charge.expired
-- ^ describes a charge
--   Occurs whenever an uncaptured charge expires.

  ChargeFailed :: EventDetails a
-- charge.failed
-- ^ describes a charge
--   Occurs whenever a failed charge attempt occurs.

  ChargePending :: EventDetails a
-- charge.pending
-- ^ describes a charge
--   Occurs whenever a pending charge is created.

  ChargeRefunded :: EventDetails a
-- charge.refunded
-- ^ describes a charge
--   Occurs whenever a charge is refunded, including partial refunds.

  ChargeSucceeded :: EventDetails a
-- charge.succeeded
-- ^ describes a charge
--   Occurs whenever a new charge is created and is successful.

  ChargeUpdated :: EventDetails a
-- charge.updated
-- ^ describes a charge
--   Occurs whenever a charge description or metadata is updated.

  ChargeDisputeClosed :: EventDetails a
-- charge.dispute.closed
-- ^ describes a dispute
--   Occurs when a dispute is closed and the dispute status changes to charge_refunded, lost, warning_closed, or won.

  ChargeDisputeCreated :: EventDetails a
-- charge.dispute.created
-- ^ describes a dispute
--   Occurs whenever a customer disputes a charge with their bank.

  ChargeDisputeFundsReinstated :: EventDetails a
-- charge.dispute.funds_reinstated
-- ^ describes a dispute
--   Occurs when funds are reinstated to your account after a dispute is closed. This includes partially refunded payments.

  ChargeDisputeFundsWithdrawn :: EventDetails a
-- charge.dispute.funds_withdrawn
-- ^ describes a dispute
--   Occurs when funds are removed from your account due to a dispute.

  ChargeDisputeUpdated :: EventDetails a
-- charge.dispute.updated
-- ^ describes a dispute
--   Occurs when the dispute is updated (usually with evidence).

  ChargeRefundUpdated :: EventDetails a
-- charge.refund.updated
-- ^ describes a refund
--   Occurs whenever a refund is updated, on selected payment methods.

  CouponCreated :: EventDetails a
-- coupon.created
-- ^ describes a coupon
--   Occurs whenever a coupon is created.

  CouponDeleted :: EventDetails a
-- coupon.deleted
-- ^ describes a coupon
--   Occurs whenever a coupon is deleted.

  CouponUpdated :: EventDetails a
-- coupon.updated
-- ^ describes a coupon
--   Occurs whenever a coupon is updated.

  CustomerCreated :: EventDetails a
-- customer.created
-- ^ describes a customer
--   Occurs whenever a new customer is created.

  CustomerDeleted :: EventDetails a
-- customer.deleted
-- ^ describes a customer
--   Occurs whenever a customer is deleted.

  CustomerUpdated :: EventDetails a
-- customer.updated
-- ^ describes a customer
--   Occurs whenever any property of a customer changes.

  CustomerDiscountCreated :: EventDetails a
-- customer.discount.created
-- ^ describes a discount
--   Occurs whenever a coupon is attached to a customer.

  CustomerDiscountDeleted :: EventDetails a
-- customer.discount.deleted
-- ^ describes a discount
--   Occurs whenever a coupon is removed from a customer.

  CustomerDiscountUpdated :: EventDetails a
-- customer.discount.updated
-- ^ describes a discount
--   Occurs whenever a customer is switched from one coupon to another.

  CustomerSourceCreated :: EventDetails a
-- customer.source.created
-- ^ describes a source (e.g., card)
--   Occurs whenever a new source is created for a customer.

  CustomerSourceDeleted :: EventDetails a
-- customer.source.deleted
-- ^ describes a source (e.g., card)
--   Occurs whenever a source is removed from a customer.

  CustomerSourceExpiring :: EventDetails a
-- customer.source.expiring
-- ^ describes a source (e.g., card)
--   Occurs whenever a source will expire at the end of the month.

  CustomerSourceUpdated :: EventDetails a
-- customer.source.updated
-- ^ describes a source (e.g., card)
--   Occurs whenever a source's details are changed.

  CustomerSubscriptionCreated :: EventDetails a
-- customer.subscription.created
-- ^ describes a subscription
--   Occurs whenever a customer is signed up for a new plan.

  CustomerSubscriptionDeleted :: EventDetails a
-- customer.subscription.deleted
-- ^ describes a subscription
--   Occurs whenever a customer's subscription ends.

  CustomerSubscriptionTrialWillEnd :: EventDetails a
-- customer.subscription.trial_will_end
-- ^ describes a subscription
--   Occurs three days before a subscription's trial period is scheduled to end, or when a trial is ended immediately (using trial_end=now).

  CustomerSubscriptionUpdated :: EventDetails a
-- customer.subscription.updated
-- ^ describes a subscription
--   Occurs whenever a subscription changes (e.g., switching from one plan to another, or changing the status from trial to active).

  FileCreated :: EventDetails a
-- file.created
-- ^ describes a file
--   Occurs whenever a new Stripe-generated file is available for your account.

  InvoiceCreated :: EventDetails a
-- invoice.created
-- ^ describes an invoice
--   Occurs whenever a new invoice is created. To learn how webhooks can be used with this event, and how they can affect it, see Using Webhooks with Subscriptions.

  InvoiceDeleted :: EventDetails a
-- invoice.deleted
-- ^ describes an invoice
--   Occurs whenever a draft invoice is deleted.

  InvoiceMarkedUncollectible :: EventDetails a
-- invoice.marked_uncollectible
-- ^ describes an invoice
--   Occurs whenever an invoice is marked uncollectible.

  InvoicePaymentFailed :: EventDetails a
-- invoice.payment_failed
-- ^ describes an invoice
--   Occurs whenever an invoice payment attempt fails, due either to a declined payment or to the lack of a stored payment method.

  InvoicePaymentSucceeded :: EventDetails a
-- invoice.payment_succeeded
-- ^ describes an invoice
--   Occurs whenever an invoice payment attempt succeeds.

  InvoiceSent :: EventDetails a
-- invoice.sent
-- ^ describes an invoice
--   Occurs whenever an invoice email is sent out.

  InvoiceUpcoming :: EventDetails a
-- invoice.upcoming
-- ^ describes an invoice
--   Occurs X number of days before a subscription is scheduled to create an invoice that is automatically charged—where X is determined by your subscriptions settings. Note: The received Invoice object will not have an invoice ID.

  InvoiceUpdated :: EventDetails a
-- invoice.updated
-- ^ describes an invoice
--   Occurs whenever an invoice changes (e.g., the invoice amount).

  InvoiceVoided :: EventDetails a
-- invoice.voided
-- ^ describes an invoice
--   Occurs whenever an invoice is voided.

  InvoiceitemCreated :: EventDetails a
-- invoiceitem.created
-- ^ describes an invoiceitem
--   Occurs whenever an invoice item is created.

  InvoiceitemDeleted :: EventDetails a
-- invoiceitem.deleted
-- ^ describes an invoiceitem
--   Occurs whenever an invoice item is deleted.

  InvoiceitemUpdated :: EventDetails a
-- invoiceitem.updated
-- ^ describes an invoiceitem
--   Occurs whenever an invoice item is updated.

  IssuerFraudRecordCreated :: EventDetails a
-- issuer_fraud_record.created
-- ^ describes an issuer fraud record
--   Occurs whenever an issuer fraud record is created.

  IssuingAuthorizationCreated :: EventDetails a
-- issuing_authorization.created
-- ^ describes an issuing authorization
--   Occurs whenever an authorization is created.

  IssuingAuthorizationUpdated :: EventDetails a
-- issuing_authorization.updated
-- ^ describes an issuing authorization
--   Occurs whenever an authorization is updated.

  IssuingCardCreated :: EventDetails a
-- issuing_card.created
-- ^ describes an issuing card
--   Occurs whenever a card is created.

  IssuingCardUpdated :: EventDetails a
-- issuing_card.updated
-- ^ describes an issuing card
--   Occurs whenever a card is updated.

  IssuingCardholderCreated :: EventDetails a
-- issuing_cardholder.created
-- ^ describes an issuing cardholder
--   Occurs whenever a cardholder is created.

  IssuingCardholderUpdated :: EventDetails a
-- issuing_cardholder.updated
-- ^ describes an issuing cardholder
--   Occurs whenever a cardholder is updated.

  IssuingDisputeCreated :: EventDetails a
-- issuing_dispute.created
-- ^ describes an issuing dispute
--   Occurs whenever a dispute is created.

  IssuingDisputeUpdated :: EventDetails a
-- issuing_dispute.updated
-- ^ describes an issuing dispute
--   Occurs whenever a dispute is updated.

  IssuingTransactionCreated :: EventDetails a
-- issuing_transaction.created
-- ^ describes an issuing transaction
--   Occurs whenever an issuing transaction is created.

  IssuingTransactionUpdated :: EventDetails a
-- issuing_transaction.updated
-- ^ describes an issuing transaction
--   Occurs whenever an issuing transaction is updated.

  OrderCreated :: EventDetails a
-- order.created
-- ^ describes an order
--   Occurs whenever an order is created.

  OrderPaymentFailed :: EventDetails a
-- order.payment_failed
-- ^ describes an order
--   Occurs whenever an order payment attempt fails.

  OrderPaymentSucceeded :: EventDetails a
-- order.payment_succeeded
-- ^ describes an order
--   Occurs whenever an order payment attempt succeeds.

  OrderUpdated :: EventDetails a
-- order.updated
-- ^ describes an order
--   Occurs whenever an order is updated.

  OrderReturnCreated :: EventDetails a
-- order_return.created
-- ^ describes an order return
--   Occurs whenever an order return is created.

  PaymentIntentAmountCapturableUpdated :: EventDetails a
-- payment_intent.amount_capturable_updated
-- ^ describes a payment intent
--   Occurs when a PaymentIntent is updated.

  PaymentIntentCreated :: EventDetails a
-- payment_intent.created
-- ^ describes a payment intent
--   Occurs when a new PaymentIntent is created.

  PaymentIntentPaymentFailed :: EventDetails a
-- payment_intent.payment_failed
-- ^ describes a payment intent
--   Occurs when a PaymentIntent has failed the attempt to create a source or a payment.

  PaymentIntentRequiresCapture :: EventDetails a
-- payment_intent.requires_capture
-- ^ describes a payment intent
--   Occurs when a PaymentIntent is ready to be captured.

  PaymentIntentSucceeded :: EventDetails a
-- payment_intent.succeeded
-- ^ describes a payment intent
--   Occurs when a PaymentIntent has been successfully fulfilled.

  PayoutCanceled :: EventDetails a
-- payout.canceled
-- ^ describes a payout
--   Occurs whenever a payout is canceled.

  PayoutCreated :: EventDetails a
-- payout.created
-- ^ describes a payout
--   Occurs whenever a payout is created.

  PayoutFailed :: EventDetails a
-- payout.failed
-- ^ describes a payout
--   Occurs whenever a payout attempt fails.

  PayoutPaid :: EventDetails a
-- payout.paid
-- ^ describes a payout
--   Occurs whenever a payout is expected to be available in the destination account. If the payout fails, a payout.failed notification is also sent, at a later time.

  PayoutUpdated :: EventDetails a
-- payout.updated
-- ^ describes a payout
--   Occurs whenever a payout's metadata is updated.

  PlanCreated :: EventDetails a
-- plan.created
-- ^ describes a plan
--   Occurs whenever a plan is created.

  PlanDeleted :: EventDetails a
-- plan.deleted
-- ^ describes a plan
--   Occurs whenever a plan is deleted.

  PlanUpdated :: EventDetails a
-- plan.updated
-- ^ describes a plan
--   Occurs whenever a plan is updated.

  ProductCreated :: EventDetails a
-- product.created
-- ^ describes a product
--   Occurs whenever a product is created.

  ProductDeleted :: EventDetails a
-- product.deleted
-- ^ describes a product
--   Occurs whenever a product is deleted.

  ProductUpdated :: EventDetails a
-- product.updated
-- ^ describes a product
--   Occurs whenever a product is updated.

  RecipientCreated :: EventDetails a
-- recipient.created
-- ^ describes a recipient
--   Occurs whenever a recipient is created.

  RecipientDeleted :: EventDetails a
-- recipient.deleted
-- ^ describes a recipient
--   Occurs whenever a recipient is deleted.

  RecipientUpdated :: EventDetails a
-- recipient.updated
-- ^ describes a recipient
--   Occurs whenever a recipient is updated.

  ReportingReportRunFailed :: EventDetails a
-- reporting.report_run.failed
-- ^ describes a report run
--   Occurs whenever a requested **ReportRun** failed to complete.

  ReportingReportRunSucceeded :: EventDetails a
-- reporting.report_run.succeeded
-- ^ describes a report run
--   Occurs whenever a requested **ReportRun** completed succesfully.

  ReportingReportTypeUpdated :: EventDetails a
-- reporting.report_type.updated
-- ^ describes a report type
--   Occurs whenever a **ReportType** is updated (typically to indicate that a new day's data has come available).

  ReviewClosed :: EventDetails a
-- review.closed
-- ^ describes a review
--   Occurs whenever a review is closed. The review's reason field indicates why: approved, disputed, refunded, or refunded_as_fraud.

  ReviewOpened :: EventDetails a
-- review.opened
-- ^ describes a review
--   Occurs whenever a review is opened.

  SigmaScheduledQueryRunCreated :: EventDetails a
-- sigma.scheduled_query_run.created
-- ^ describes a scheduled query run
--   Occurs whenever a Sigma scheduled query run finishes.

  SkuCreated :: EventDetails a
-- sku.created
-- ^ describes a sku
--   Occurs whenever a SKU is created.

  SkuDeleted :: EventDetails a
-- sku.deleted
-- ^ describes a sku
--   Occurs whenever a SKU is deleted.

  SkuUpdated :: EventDetails a
-- sku.updated
-- ^ describes a sku
--   Occurs whenever a SKU is updated.

  SourceCanceled :: EventDetails a
-- source.canceled
-- ^ describes a source (e.g., card)
--   Occurs whenever a source is canceled.

  SourceChargeable :: EventDetails a
-- source.chargeable
-- ^ describes a source (e.g., card)
--   Occurs whenever a source transitions to chargeable.

  SourceFailed :: EventDetails a
-- source.failed
-- ^ describes a source (e.g., card)
--   Occurs whenever a source fails.

  SourceMandateNotification :: EventDetails a
-- source.mandate_notification
-- ^ describes a source (e.g., card)
--   Occurs whenever a source mandate notification method is set to manual.

  SourceRefundAttributesRequired :: EventDetails a
-- source.refund_attributes_required
-- ^ describes a source (e.g., card)
--   Occurs whenever the refund attributes are required on a receiver source to process a refund or a mispayment.

  SourceTransactionCreated :: EventDetails a
-- source.transaction.created
-- ^ describes a source transaction
--   Occurs whenever a source transaction is created.

  SourceTransactionUpdated :: EventDetails a
-- source.transaction.updated
-- ^ describes a source transaction
--   Occurs whenever a source transaction is updated.

  TopupCanceled :: EventDetails a
-- topup.canceled
-- ^ describes a topup
--   Occurs whenever a top-up is canceled.

  TopupCreated :: EventDetails a
-- topup.created
-- ^ describes a topup
--   Occurs whenever a top-up is created.

  TopupFailed :: EventDetails a
-- topup.failed
-- ^ describes a topup
--   Occurs whenever a top-up fails.

  TopupReversed :: EventDetails a
-- topup.reversed
-- ^ describes a topup
--   Occurs whenever a top-up is reversed.

  TopupSucceeded :: EventDetails a
-- topup.succeeded
-- ^ describes a topup
--   Occurs whenever a top-up succeeds.

  TransferCreated :: EventDetails a
-- transfer.created
-- ^ describes a transfer
--   Occurs whenever a transfer is created.

  TransferReversed :: EventDetails a
-- transfer.reversed
-- ^ describes a transfer
--   Occurs whenever a transfer is reversed, including partial reversals.

  TransferUpdated :: EventDetails a
-- transfer.updated
-- ^ describes a transfer
--   Occurs whenever a transfer's description or metadata is updated.
