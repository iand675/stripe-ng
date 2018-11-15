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

instance ToHttpApiData BillingScheme where
  toQueryParam s = case s of
    PerUnit -> "per_unit"
    Tiered -> "tiered"

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

instance ToHttpApiData BillingInterval where
  toQueryParam i = case i of
    Day -> "day"
    Week -> "week"
    Month -> "month"
    Year -> "year"

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

instance ToHttpApiData UsageType where
  toQueryParam u = case u of
    Metered -> "metered"
    Licensed -> "licensed"

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

data NewPlan = NewPlan
  { newPlanId :: Maybe (Id Plan)
  , newPlanCurrency :: CurrencyCode
  , newPlanInterval :: BillingInterval
  -- TODO can also be a subset of NewProduct fields
  , newPlanProduct :: Id Product
  , newPlanActive :: Maybe Bool
  , newPlanAggregateUsage :: Maybe AggregateUsage
  , newPlanAmount :: Maybe Integer
  , newPlanBillingScheme :: Maybe BillingScheme
  , newPlanIntervalCount :: Maybe Integer
  , newPlanMetadata :: Metadata
  , newPlanNickname :: Maybe Text
  , newPlanTiers :: [Tier]
  , newPlanTiersMode :: Maybe TiersMode
  , newPlanTransformUsage :: Maybe TransformUsage
  , newPlanTrialPeriodDays :: Maybe Integer
  , newPlanUsageType :: Maybe UsageType
  }

instance ToForm NewPlan where
  toForm NewPlan{..} = mconcat
    [ optParam "id" newPlanId
    , reqParam "currency" newPlanCurrency
    , reqParam "interval" newPlanInterval
    , reqParam "product" newPlanProduct
    -- , optParam "active"
    -- , optParam ""
    , optParam "amount" newPlanAmount
    , optParam "billing_scheme" newPlanBillingScheme
    , optParam "interval_count" newPlanIntervalCount
    ]

-- createPlan

retrievePlan :: (MonadStripe m, StripeResult Plan plan) => Id Plan -> m plan
retrievePlan (Id planId) = stripeGet (Proxy @Plan) ("plans/" <> encodeUtf8 planId) []

-- updatePlan
-- deletePan

listAllPlans :: (MonadStripe m, StripeResult (List Plan) planList) => Pagination Plan -> m planList
listAllPlans = stripeGet (Proxy @(List Plan)) "plans" . paginationParams
