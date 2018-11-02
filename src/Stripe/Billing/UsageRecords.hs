module Stripe.Billing.UsageRecords where
import Stripe.Billing.SubscriptionItems
import Stripe.Utils

data UsageRecord = UsageRecord
  { usageRecordId :: Id UsageRecord
  , usageRecordLiveMode :: Bool
  , usageRecordQuantity :: Word
  , usageRecordSubscriptionItem :: Id SubscriptionItem
  , usageRecordTimestamp :: Timestamp
  } deriving (Show, Eq, Generic, Typeable)

instance FromJSON UsageRecord where
  parseJSON = parseObject "UsageRecord" $ do
    assertObject "usage_record"
    UsageRecord
      <$> req "id"
      <*> req "livemode"
      <*> req "quantity"
      <*> req "subscription_item"
      <*> req "timestamp"

-- createUsageRecord

listAllSubscriptionItemPeriodSummaries :: (StripeMonad m) => Id SubscriptionItem -> m (List SubscriptionItem)
listAllSubscriptionItemPeriodSummaries (Id subscriptionItemId) =
  jsonGet
    ("subscription_items/" <> encodeUtf8 subscriptionItemId <> "/usage_record_summaries")
    []
