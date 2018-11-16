module Stripe.Orders where
import Stripe.Core
import Stripe.Customers
import Stripe.Charges
import Stripe.Utils

data ItemType
  = SKUItem
  | TaxItem
  | ShippingItem
  | DiscountItem
  deriving (Show, Eq, Generic, Typeable)

data OrderItem = OrderItem
  { orderItemAmount :: Integer
  , orderItemCurrency :: CurrencyCode
  , orderItemDescription :: Maybe Text
  -- TODO what is this
  , orderItemParent :: Maybe (Expandable Text)
  , orderItemQuantity :: Maybe Integer
  , orderItemType_ :: ItemType
  } deriving (Show, Eq, Generic, Typeable)

data OrderStatus
  = OrderCreated
  | OrderPaid
  | OrderCanceled
  | OrderFulfilled
  | OrderReturned
  deriving (Show, Eq, Generic, Typeable)

data StatusTransitions = StatusTransitions
  { statusTransitionsCanceled :: Maybe Timestamp
  , statusTransitionsFulfilled :: Maybe Timestamp
  , statusTransitionsPaid :: Maybe Timestamp
  , statusTransitionsReturned :: Maybe Timestamp
  } deriving (Show, Eq, Generic, Typeable)

data ShippingDetails = ShippingDetails
  { shippingDetailsAddress :: Address
  , shippingDetailsCarrier :: Text
  , shippingDetailsName :: Text
  , shippingDetailsPhone :: Maybe Text
  , shippingDetailsTrackingNumber :: Maybe Text
  } deriving (Show, Eq, Generic, Typeable)

instance FromJSON ShippingDetails where
  parseJSON = parseObject "ShippingDetails" $ do
    ShippingDetails
      <$> req "address"
      <*> req "carrier"
      <*> req "name"
      <*> opt "phone"
      <*> opt "tracking_number"

data ExactDelivery = ExactDelivery
  { exactDeliveryDate :: Text
  } deriving (Show, Eq, Generic, Typeable)

data RangeDelivery = RangeDelivery
  { rangeDeliveryEarliest :: Text
  , rangeDeliveryLatest :: Text
  } deriving (Show, Eq, Generic, Typeable)

data DeliveryEstimate
  = Exact ExactDelivery
  | Range RangeDelivery
  deriving (Show, Eq, Generic, Typeable)

data ShippingMethod = ShippingMethod
  { shippingMethodId :: Id ShippingMethod
  , shippingMethodAmount :: Integer
  , shippingMethodCurrency :: CurrencyCode
  , shippingMethodDeliveryEstimate :: DeliveryEstimate
  , shippingMethodDescription :: Maybe Text
  } deriving (Show, Eq, Generic, Typeable)

data Order = Order
  { orderId :: Id Order
  , orderAmount :: Integer
  , orderAmountReturned :: Maybe Integer
  , orderApplication :: Maybe (Id Application)
  , orderApplicationFee :: Maybe Integer
  , orderCharge :: Maybe (Expandable Charge)
  , orderCreated :: Timestamp
  , orderCurrency :: CurrencyCode
  , orderCustomer :: Expandable Customer
  , orderEmail :: Text
  , orderExternalCouponCode :: Maybe Text
  , orderItems :: [OrderItem]
  , orderLiveMode :: Bool
  , orderMetadata :: Metadata
  -- TODO , orderReturns :: List _
  , orderSelectedShippingMethod :: Maybe (Id ShippingMethod)
  , orderShipping :: Maybe ShippingDetails
  , orderShippingMethods :: Maybe ShippingMethod
  , orderStatus :: OrderStatus
  , orderStatusTransitions :: StatusTransitions
  , orderUpdated :: Maybe Timestamp
  , orderUpstreamId :: Maybe Text
  } deriving (Show, Eq, Generic, Typeable)

{-
createOrder
updateOrder
payOrder
listAllOrders
returnOrder
-}
