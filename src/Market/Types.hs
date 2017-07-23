{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

module Market.Types
  ( module Market.Types
  , Bitcoin(..)
  )
  where

import Control.DeepSeq
import GHC.Generics (Generic)

import Data.Word
import Data.List

import Razao.Util
import Bitcoin

-------------------
deriving instance NFData (Bitcoin)

type    BTC = Bitcoin
-------------------
newtype LTC = LTC Bitcoin deriving (Num, Fractional, Real, RealFrac, Eq, Ord, NFData)
instance Show LTC where
  show (LTC vol) = show vol
-------------------
-- FIX ME! This will do for now, but ether has more precision. decimal-arithmetic package is an option.
newtype ETH = ETH Bitcoin deriving (Eq, Ord, Num, Fractional, Real, RealFrac, NFData)
instance Show ETH where
  show (ETH vol) = show vol
-------------------
newtype USD = USD Double  deriving (Num, Fractional, Real, RealFrac, Show, NFData)

instance Eq USD where
  USD x == USD y = round2dp x == round2dp y

instance Ord USD where
  USD x `compare` USD y = round2dp x `compare` round2dp y
-------------------
newtype BRL = BRL Double  deriving (Show, Num, Fractional, Real, RealFrac, NFData)

instance Eq BRL where
  BRL x == BRL y = round5dp x == round5dp y

instance Ord BRL where
  BRL x `compare` BRL y = round5dp x `compare` round5dp y

-------------------
class (NFData coin, RealFrac coin, Show coin) => Coin coin where
  name :: coin -> String

instance Coin USD where
  name _ = "USD"

instance Coin BRL where
  name _ = "BRL"

instance Coin BTC where
  name _ = "BTC"

instance Coin LTC where
  name _ = "LTC"

instance Coin ETH where
  name _ = "ETH"
-------------------
-- Units

newtype Vol   a = Vol   a deriving (Show, Eq, Ord, Num, Fractional, Real, RealFrac, NFData)
newtype Price a = Price a deriving (Show, Eq, Ord, Num, Fractional, Real, RealFrac, NFData)
newtype Cost  a = Cost  a deriving (Show, Eq, Ord, Num, Fractional, Real, RealFrac, NFData)

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
        , acVolume  :: Vol   vol }
    | NewMarketOrder
        { acSide          :: OrderSide
        , acVolAndOrFunds :: AndOr (Vol vol) (Cost price)}
    | CancelLimitOrder
        { acOrderID :: OrderID }
    | Transfer
        { acVolume     :: Vol vol
        , acTransferTo :: Wallet vol
        }
    | PANIC String
    deriving (Eq, Show)

type Reasoning = String
data StrategyAdvice price vol = ToDo [Action price vol] Reasoning deriving (Show, Eq)


------------------------- Orders -------------------------
data OrderID = OID { hw :: Word64, lw :: Word64 } deriving (Show, Eq, Ord)

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
    = TP (OrderPlacement    p v)
    | TC (OrderCancellation    )
    | TF (OrderFill         p v)
    | TB (QuoteBook         p v q c)
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
