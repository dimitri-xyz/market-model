-- {-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Market.Types
  ( module Market.Types
  , Bitcoin(..)
  )
  where

import Razao.Util
import Bitcoin

import Data.Word
import Data.List


--------------------------------------------------------------------------------
{- NOTE: [Order book Comparison]

See QuoteBook type.

1) We do NOT compare 'counter's or timestamps for order book comparisons.
   We only look at the book itself.

2) The implementation of the orderbook must allow us to obtain two lists:
- a list of bids in decreasing price order (more specifically, non-increasing)
- a list of asks in increasing price order (more specifically, non-decreasing)

INVARIANT:
    ** We assume the lists are always ordered as described above **

3) If all orders had distinct prices, comparing two order books would simply
required comparing those lists. However, it is possible for 2 orders to have
exactly the same price and in that case, the order between them is undefined.

So, to compare two orderbooks we need to "normalize" them first. This means
coalescing all volume quote under a single price in a single list element.

-}
--------------------------------------------------------------------------------

-------------------
-- Units

-- FIX ME! These need to be newtypes.
type Volume         = Bitcoin
type Price          = Double
type Cost           = Double
type Revenue        = Cost
type Profit         = Revenue

type CurrencyVolume = Cost
type BTCVolume      = Volume

type TransactionID  = Word64
type Timestamp      = Word64 -- in posix seconds

type CostAndOrVolume = Either Volume (Maybe Volume, Cost)

newtype BTCWallet        = BTCWallet { address :: String } deriving (Show,Eq)
newtype BTCTransactionID = BTCTransactionID String deriving (Show,Eq)
-------------------

data OrderSide = Bid | Ask deriving (Show, Eq, Enum, Ord)


data Quote a = Quote { side   :: OrderSide
                     , price  :: Price
                     , volume :: Volume
                     , qtail  :: a
                     } deriving (Eq,Show)

data QuoteBook qtail counter = QuoteBook{ bids::[Quote qtail]
                                        , asks::[Quote qtail]
                                        , counter :: counter
                                        } deriving (Show)


-- See NOTE: [Order book Comparison]
instance Eq qtail => Eq (QuoteBook qtail counter) where
    x == y = normalizeQuotes (asks x) == normalizeQuotes (asks y) &&
             normalizeQuotes (bids x) == normalizeQuotes (bids y)

normalizeQuotes :: [Quote a] -> [Quote ()]
normalizeQuotes xs =
  let xxs = groupBy (\q q' -> price q == price q') xs
      addVols :: [Quote a] -> Volume
      addVols  = sum . fmap volume
      getPrice = price . head
      getSide  = side  . head
      coalesce qs = Quote{ side   = getSide  qs
                         , price  = getPrice qs
                         , volume = addVols  qs
                         , qtail  = () }
   in fmap coalesce xxs



------------------------ Actions -------------------------
-- Actions that can be performed by trading strategies

data Action
    = NewLimitOrder
        { acSide    :: OrderSide
        , acPrice   :: Price
        , acVolume  :: Volume }
    | NewMarketOrder
        { acSide             :: OrderSide
        , acVolumeAndOrFunds :: Either Volume (Maybe Volume, Cost) }
    | CancelLimitOrder
        { acOrderID :: OrderID }
    | TransferBTC
        { acVolume     :: Volume
        , acTransferTo :: BTCWallet
        }
    | PANIC String
    deriving (Eq, Show)

type Reasoning = String
data StrategyAdvice = ToDo [Action] Reasoning deriving (Eq, Show)


------------------------- Orders -------------------------

data OrderID = OID { hw :: Word64, lw :: Word64 } deriving (Show, Eq, Ord)

-- | This is a *partition* of the OrderStatus space
data OrderStatus = Active | Inactive | ActivePartiallyExecuted deriving (Show, Eq, Enum, Ord)
----------
data Confirmation = Conf { orderID    :: OrderID
                         , mTimestamp :: Maybe Timestamp -- The Exchanges should, but don't always give me this

                         -- orders that have been confirmed may be partially executed.
                         , mExecuted       :: Maybe (Price, Volume) -- (average price, vol), nothing means "don't know"
                         , mOrderStatus    :: Maybe OrderStatus     -- Nothing means "don't know"
                         }
                         deriving (Show, Eq, Ord)

data Order ack
    = LimitOrder
        { oSide            :: OrderSide
        , limitPrice       :: Price
        , limitVolume      :: Volume      -- volume requested when order was placed
        , aConfirmation    :: ack
        }
    | MarketOrder
        { oSide            :: OrderSide
        , volumeAndOrFunds :: Either Volume (Maybe Volume, Cost)
        , aConfirmation    :: ack
        }
    deriving (Show, Ord)

instance Eq ack => Eq (Order ack) where
    a == b = isEqualOrder a b


isEqualOrder :: Eq ack => Order ack -> Order ack -> Bool
isEqualOrder a@(LimitOrder{}) b@(LimitOrder{}) =
  oSide          a        == oSide          b        &&
  limitVolume    a        == limitVolume    b        &&
  aConfirmation  a        == aConfirmation  b        &&
  round5dp (limitPrice a) == round5dp (limitPrice b)    -- 5 dp precision on price

isEqualOrder a@(MarketOrder{}) b@(MarketOrder{}) =
  oSide            a      == oSide            b      &&
  volumeAndOrFunds a      == volumeAndOrFunds b      && -- FIX ME! should be 5 dp precision on Cost
  aConfirmation    a      == aConfirmation    b      &&
  round5dp (limitPrice a) == round5dp (limitPrice b)    -- 5 dp precision on price

isEqualOrder _ _ = False

newtype OrderPlacement    = Placement    {toOrder   :: Order Confirmation} deriving (Show, Eq)
newtype OrderCancellation = Cancellation {toOID     :: OrderID}            deriving (Show, Eq)
newtype OrderFill         = OrderFilled  {toFills   :: [Fill]}             deriving (Show, Eq)
-------------------
type FillID = Word64

{- This representation is failing in tests because of extreme values
   I require 5dp precision in comparisons of the fee, but the fee is calculated based on the
   volume and price and for very large volumes (fees in the 1E8 range) the mantissa is just not big enough!
-}

data Fill = Fill { fillID       :: FillID       -- must be unique
                 , mFillTime    :: Maybe Timestamp  -- 'Maybe' here is *kinda* wrong, but the exchanges may not return
                                                    -- the exact time the fill happened and instead just tell me it did.
                                                    -- So, even for a fill that has a FillID, I may not have this information.
                                                    -- Ideally, all fills should have a timestamp for when they were executed.

                 , fillVolume   :: Volume       -- the volume executed in this fill
                 , fillPrice    :: Price        -- the price that was actually used
                 , fillFee      :: Cost         -- the fee charged for this transaction in dollars (or appropriate currency)
                 , orderId      :: OrderID
                 }
                 deriving (Show, Ord)

instance Eq Fill where
    a == b =  fillID     a           == fillID     b           &&
              mFillTime  a           == mFillTime  b           &&
              fillVolume a           == fillVolume b           &&
              orderId    a           == orderId    b           &&
              round5dp (fillPrice a) == round5dp (fillPrice b) && -- 5 dp precision on money
              round5dp (fillFee   a) == round5dp (fillFee   b)    -- 5 dp precision on money


---------------------------------------------------------------------
class Exchange config exception | config -> exception where

    -- to place limit orders
    placeLimit :: config -> OrderSide -> Price -> Volume -> IO (Either exception Confirmation)

    -- to place market orders
    -- This should fail with a run-time exception if a requested limiting
    -- factor cannot be enforced at the exchange. For example, if the
    -- client asks to limit the market order by overall cost, but the exchange
    -- can only limit by volume.
    placeMarket :: config -> OrderSide -> CostAndOrVolume -> IO (Either exception Confirmation)

    -- | returns order info as it stood immediately AFTER cancellation; or an exception if it wasn't able to complete
    --   the request for any other reason (i.e. order already canceled, already executed or network failure)
    cancelOrder :: config -> OrderID -> IO (Either exception Confirmation)

    -- | returns pending [Order] or an Exception if can't get that information
    getPendingOrders :: config -> IO (Either exception [Order Confirmation])

    -- | returns [Order] of the right type or an Exception if can't get that information. Maybe parameters are optional.
    -- timestamps are start and end time.
    getOrders :: config -> Maybe OrderSide -> Maybe Timestamp -> Maybe Timestamp -> IO (Either exception [Order Confirmation])

    getFunds :: config -> IO (Either exception (CurrencyVolume , BTCVolume , Timestamp))

    transferBTC :: config -> Volume -> BTCWallet -> IO (Either exception BTCTransactionID)
