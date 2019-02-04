{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

module Market.Types
  where

import Control.Applicative
import Control.DeepSeq
import GHC.Generics (Generic)

import Data.Word
import Data.List
import Data.Hashable

-------------------
class (Show coin, RealFrac coin, NFData coin, Hashable coin) => Coin coin where
  coinSymbol :: coin -> String
  showBare   :: coin -> String
  readBare   :: String -> coin

-------------------
-- Units

newtype Vol   a = Vol   a deriving (Show, Eq, Ord, Num, Fractional, Real, RealFrac, NFData, Hashable)
newtype Price a = Price a deriving (Show, Eq, Ord, Num, Fractional, Real, RealFrac, NFData, Hashable)
newtype Cost  a = Cost  a deriving (Show, Eq, Ord, Num, Fractional, Real, RealFrac, NFData, Hashable)

type Revenue = Cost
type Profit  = Revenue

type CurrencyVol = Cost
type BTCVol      = Vol

type AndOr vol cost = Either vol (Maybe vol, cost)

type TransactionID = Word64
type Timestamp     = Word64 -- in posix seconds

newtype Wallet vol = Wallet {address :: String} deriving (Show, Eq)
newtype TransferID = TransferID String          deriving (Show, Eq)

-------------------
data OrderSide = Bid | Ask deriving (Show, Eq, Enum, Ord, Generic)
instance NFData OrderSide
instance Hashable OrderSide where
    hashWithSalt = hashUsing fromEnum

data Quote p v tail
  = Quote
    { side   :: OrderSide
    , price  :: Price p
    , volume :: Vol   v
    , qtail  :: tail
    } deriving (Show, Eq, Generic)

instance (NFData p, NFData v, NFData t) => NFData (Quote p v t)

data QuoteBook p v qtail counter
  = QuoteBook
    { bids::[Quote p v qtail]
    , asks::[Quote p v qtail]
    , counter :: counter
    } deriving (Show, Generic)

instance (NFData p, NFData v, NFData t, NFData c) => NFData (QuoteBook p v t c)

{-------------------------------------------------------------------------------
  NOTE: [Order book Comparison]

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

----------------------------------------------------------------------------- -}
instance (Eq p, Num p, Eq v, Num v, Eq qtail) => Eq (QuoteBook p v qtail counter) where
    x == y = normalizeQuotes (asks x) == normalizeQuotes (asks y) &&
             normalizeQuotes (bids x) == normalizeQuotes (bids y)

normalizeQuotes
  :: (Eq p, Num p, Num v)
  => [Quote p v tail]
  -> [Quote p v ()]
normalizeQuotes xs = fmap coalesce xxs
  where
    xxs = groupBy (\q q' -> price q == price q') xs
    addVols :: (Num price, Num vol) => [Quote price vol tail] -> Vol vol
    addVols  = sum . fmap volume
    getPrice = price . head
    getSide  = side  . head
    coalesce qs = Quote{ side   = getSide  qs
                       , price  = getPrice qs
                       , volume = addVols  qs
                       , qtail  = () }


------------------------ Actions -------------------------
-- Actions that can be performed by trading strategies

data Action price vol
    = NewLimitOrder
        { acSide    :: OrderSide
        , acPrice   :: Price price
        , acVolume  :: Vol   vol
        , acmClientOID :: Maybe OrderID }
    | NewMarketOrder
        { acSide          :: OrderSide
        , acVolAndOrFunds :: AndOr (Vol vol) (Cost price)
        , acmClientOID    :: Maybe OrderID }
    | CancelLimitOrder
        { acClientOID     :: OrderID }
    | Transfer
        { acVolume     :: Vol vol
        , acTransferTo :: Wallet vol
        }
    | PANIC String
    deriving (Eq, Show)

type Reasoning = String
newtype StrategyAdvice action = Advice (Reasoning, ZipList action) deriving (Show, Eq)

-- We may need to refine this in the future if we are to combine orders 
-- between themselves before issuing them.
instance Semigroup (StrategyAdvice action) where
    Advice (l, ZipList ls) <> Advice (r, ZipList rs) = Advice (l <> r, ZipList (ls <> rs))

instance Monoid (StrategyAdvice action) where
    mempty = Advice ("", ZipList [])

-- FIX ME! should just derive these, but...
instance Functor StrategyAdvice where
    fmap f (Advice (r, as)) = Advice (r, fmap f as)

instance Applicative StrategyAdvice where
    pure a = Advice (mempty, pure a)
    liftA2 f (Advice (l, as)) (Advice (r, bs)) = Advice (l <> r, liftA2 f as bs)



------------------------- Orders -------------------------
data OrderID = OID { hw :: Word64, lw :: Word64 } deriving (Show, Eq, Ord, Generic)

instance Hashable OrderID

-- | This is a *partition* of the OrderStatus space
data OrderStatus = Active | Inactive | ActivePartiallyExecuted deriving (Show, Eq, Enum, Ord)
----------
data Confirmation price vol
  = Conf
    { orderID    :: OrderID
    , mTimestamp :: Maybe Timestamp -- The Exchanges should, but don't always give me this
    -- orders that have been confirmed may be partially executed.
    , mExecuted       :: Maybe (Price price, Vol vol) -- (average price, vol), nothing means "don't know"
    , mOrderStatus    :: Maybe OrderStatus  -- Nothing means "don't know"
    }
    deriving (Show, Eq, Ord)

data Order price vol ack
    = LimitOrder
        { oSide            :: OrderSide
        , limitPrice       :: Price price
        , limitVolume      :: Vol vol         -- volume requested when order was placed
        , aConfirmation    :: ack
        }
    | MarketOrder
        { oSide            :: OrderSide
        , volumeAndOrFunds :: AndOr (Vol vol) (Cost price)
        , aConfirmation    :: ack
        }
    deriving (Show, Eq, Ord)

newtype OrderPlacement p v = Placement    {toOrder :: Order p v (Confirmation p v)} deriving (Show, Eq)
newtype OrderCancellation  = Cancellation {toOID   :: OrderID}                      deriving (Show, Eq)
newtype OrderFill      p v = OrderFilled  {toFills :: [Fill p v]}                   deriving (Show, Eq)

-- unifying wrapper for trading events
data TradingE p v q c
    = TP {toPlace  :: OrderPlacement    p v}
    | TC {toCancel :: OrderCancellation    }
    | TF {toFill   :: OrderFill         p v}
    | TB {toBook   :: QuoteBook         p v q c}
    deriving (Show, Eq)

-------------------
type FillID = Word64

{- The representation was failing in tests because of extreme values represented as Double
   I require 5dp precision in comparisons of the fee, but the fee is calculated based on the
   volume and price and for very large volumes (fees in the 1E8 range) the mantissa is just not big enough!
-}

data Fill price vol
  = Fill
    { fillID       :: FillID           -- must be unique
    , mFillTime    :: Maybe Timestamp  -- 'Maybe' here is *kinda* wrong, but the exchanges may not return
                                       -- the exact time the fill happened and instead just tell me it did.
                                       -- So, even for a fill that has a FillID, I may not have this information.
                                       -- Ideally, all fills should have a timestamp for when they were executed.

    , fillVolume   :: Vol   vol     -- the volume executed in this fill
    , fillPrice    :: Price price   -- the price that was actually used
    , fillFee      :: Cost  price   -- the fee charged for this transaction in dollars (or appropriate currency)
    , orderId      :: OrderID
    }
    deriving (Show, Ord, Eq)
