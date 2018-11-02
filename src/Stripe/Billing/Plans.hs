module Stripe.Billing.Plans where
import Stripe.Billing.Products
import Stripe.Utils

data AggregateUsage
  = Sum
  | LastDuringPeriod
  | LastEver
  | Max
  deriving (Show, Eq, Generic, Typeable)

instance FromJSON AggregateUsage where
  parseJSON = withText "AggregateUsage" $ \t -> case t of
    "sum" -> pure Sum
    "last_during_period" -> pure LastDuringPeriod
    "last_ever" -> pure LastEver
    "max" -> pure Max
    _ -> fail ("Invalid AggregateUsage: " ++ show t)

data BillingScheme
  = PerUnit
  | Tiered
  deriving (Show, Eq, Generic, Typeable)

instance FromJSON BillingScheme where
  parseJSON = withText "BillingScheme" $ \t -> case t of
    "per_unit" -> pure PerUnit
    "tiered" -> pure Tiered
    _ -> fail ("Invalid BillingScheme: " ++ show t)

data BillingInterval
  = Day
  | Week
  | Month
  | Year
  deriving (Show, Eq, Generic, Typeable)

instance FromJSON BillingInterval where
  parseJSON = withText "BillingInterval" $ \t -> case t of
    "day" -> pure Day
    "week" -> pure Week
    "month" -> pure Month
    "year" -> pure Year
    _ -> fail ("Invalid BillingInterval: " ++ show t)

data Tier = Tier
  { tierFlatAmount :: Integer
  , tierUnitAmount :: Integer
  , tierUpTo :: Integer
  } deriving (Show, Eq, Generic, Typeable)

instance FromJSON Tier where
  parseJSON = parseObject "Tier" $ do
    Tier
      <$> req "flat_amount"
      <*> req "unit_amount"
      <*> req "up_to"

data TiersMode
  = Graduated
  | Volume
  deriving (Show, Eq, Generic, Typeable)

instance FromJSON TiersMode where
  parseJSON = withText "TiersMode" $ \t -> case t of
    "graduated" -> pure Graduated
    "volume" -> pure Volume
    _ -> fail ("Invalid TiersMode: " ++ show t)

data TransformRounding
  = RoundUp
  | RoundDown
  deriving (Show, Eq, Generic, Typeable)

instance FromJSON TransformRounding where
  parseJSON = withText "TransformRounding" $ \t -> case t of
    "up" -> pure RoundUp
    "down" -> pure RoundDown
    _ -> fail ("Invalid TransformRounding: " ++ show t)

data TransformUsage = TransformUsage
  { transformUsageDivideBy :: Integer
  , transformUsageRound :: TransformRounding
  } deriving (Show, Eq, Generic, Typeable)

instance FromJSON TransformUsage where
  parseJSON = parseObject "TransformUsage" $ do
    TransformUsage
      <$> req "divide_by"
      <*> req "round"

data UsageType
  = Metered
  | Licensed
  deriving (Show, Eq, Generic, Typeable)

instance FromJSON UsageType where
  parseJSON = withText "UsageType" $ \t -> case t of
    "metered" -> pure Metered
    "licensed" -> pure Licensed
    _ -> fail ("Invalid UsageType: " ++ show t)

data Plan = Plan
  { planId :: Id Plan
  , planActive :: Bool
  , planAggregateUsage :: Maybe AggregateUsage
  , planAmount :: Int
  , planBillingscheme :: BillingScheme
  , planCreated :: Timestamp
  , planCurrency :: Text -- TODO Make into currency
  , planInterval :: BillingInterval
  , planIntervalCount :: Word
  , planLiveMode :: Bool
  , planMetadata :: Metadata
  , planNickname :: Maybe Text
  , planProduct :: Expandable Product -- TODO expandable
  , planTiers :: Maybe [Tier]
  , planTiersMode :: Maybe TiersMode
  , planTransformUsage :: Maybe TransformUsage
  , planTrialPeriodDays :: Maybe Word
  , planUsageType :: UsageType
  } deriving (Show, Eq, Generic, Typeable)

instance FromJSON Plan where
  parseJSON = parseObject "Plan" $ do
    assertObject "plan"
    Plan
      <$> req "id"
      <*> req "active"
      <*> opt "aggregate_usage"
      <*> req "amount"
      <*> req "billing_scheme"
      <*> req "created"
      <*> req "currency"
      <*> req "interval"
      <*> req "interval_count"
      <*> req "livemode"
      <*> req "metadata"
      <*> opt "nickname"
      <*> req "product"
      <*> opt "tiers"
      <*> opt "tiers_mode"
      <*> opt "transform_usage"
      <*> opt "trial_period_days"
      <*> req "usage_type"

-- createPlan

retrievePlan :: (StripeMonad m) => Id Plan -> m Plan
retrievePlan (Id planId) = jsonGet ("plans/" <> encodeUtf8 planId) []

-- updatePlan
-- deletePan

listAllPlans :: (StripeMonad m) => Pagination Plan -> m (List Plan)
listAllPlans = jsonGet "plans" . paginationParams
