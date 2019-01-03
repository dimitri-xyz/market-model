{-# LANGUAGE ScopedTypeVariables #-}

module Market.Util where

import Data.List.Extended
import Market.Types

{-------------------------------------------------------------------------------
Conventions used in this file in the order of function arguments and in pairs:
1. Asks before Bids          e.g. totalValue asks bids = ...
2. Prices before Volumes     e.g. (price, volume)
----------------------------------------------------------------------------- -}

{-------------------------------------------------------------------------------

DOUBLE AUCTION

The following function are useful for dealing with the double-auctions provided
by all the major exchanges. First, let me remind you of a few key points:

- The list of asks has non-decreasing (i.e increasing or stable) price and represents
a *marginal supply* curve. It tells us how much we have to pay to acquire each
extra unit of bitcoin.

- Similarly, the list of bids has non-increasing price and represents a *marginal demand* curve.
It tells us for how much we can sell each extra units of bitcoin we acquire.

We view a list of asks of increasing price with each ask pair (price,volume) specifying
Price = Dy / Volume = Dx. So, that to get an extra Dx bitcoins at that point, we need to pay an
extra Dy dollars.

Each ask is described by a volume and a price in currency/BTC (e.g. dollars/BTC).
The units of the price give us a clue that this curve is a derivative (a finite
difference) with respect to the quantity of bitcoins. In fact, we can "integrate"
(i.e. sum up) this curve and the bids curve to obtain aggregate supply (asks) and
aggregate demand (bids)

These aggregate quantities will make our calculations easier. So, we provide a function to
calculate them below.

Think of these quantities in graphs where:
* the quantity (volume) is the X-axis
* the price is the Y-axis

If we assume that we can always fulfill any fraction of a quote (bid or ask) up to the
given total amount, then the aggregate demand and supply curves produced are picewise linear.
In other words, they are made up of a bunch of line segments of different slopes that meet at
the connecting points (given by the list).

the returned list should be interpreted as follows:

* We're adding up the volumes. The last volume on the list is the total volume available.
* The prices are aggregated. Example:

asks:                           returned list:

[ (Price 420, Volume 0.5),      [ (210,            Volume 0.5),
  (Price 500, Volume  1 ),        (210+500=710,    Volume 1.5),
  (Price 550, Volume 0.2),        (210+500+110=820,Volume 1.7)]

----------------------------------------------------------------------------- -}


----------------------------------------------
-- | console debug output
-- show counter and top N bids and asks
-- prints a total of (2*n+2) lines
showTopN :: (Show p, Show v, Show c) => Int -> QuoteBook p v qtail c -> String
showTopN n b =
    let bids' =           take n $ bids b
        asks' = reverse $ take n $ asks b
        formatRow :: (Show p, Show v) => Quote p v a -> String
        formatRow x = show (side x) ++
            " " ++ show (volume x) ++
            " " ++ show (price  x) ++ "            \n"

     in show (counter b) ++
            "                                             \n" ++
            concatMap formatRow asks' ++
            "---                                          \n" ++
            concatMap formatRow bids'
----------------------------------------------

isActive
    :: (Show p, Show v)
    => Order p v (Confirmation p v) -> Bool
isActive o =
  case mOrderStatus (aConfirmation o) of
    Just st  -> st == Active || st == ActivePartiallyExecuted
    Nothing -> error $ "Order Status unknown. Cannot determine if order is active. " ++ show o

isBid :: Order price vol ack -> Bool
isBid = (== Bid) . oSide

isAsk :: Order price vol ack -> Bool
isAsk = (== Ask) . oSide

isLimitOrder :: Order price vol ack -> Bool
isLimitOrder LimitOrder{} = True
isLimitOrder _            = False

getOrderID :: Order price vol (Confirmation price vol) -> OrderID
getOrderID = orderID . aConfirmation

sameOrderID :: OrderID -> Order price vol (Confirmation price vol) -> Bool
sameOrderID oid order = (oid == getOrderID order)

getDoneVol
    :: (Show p, Show v)
    => Order p v (Confirmation p v)
    -> Vol v
getDoneVol order = case aConfirmation order of
    Conf { mExecuted = Just (_, vol)} -> vol
    _ -> error $ "Cannot determine already executed volume in order without volume info. " ++ show order

---------
-- Market orders can be larger than the whole orderbook or limited by funds
getOpenVol
    :: (Show price, Show vol, Num vol)
    => Order price vol (Confirmation price vol)
    -> Vol vol
-- limit order: ok
getOpenVol order@(LimitOrder{}) = limitVolume order - getDoneVol order

-- market order *only* limited by Volume: ok
getOpenVol order@(MarketOrder{volumeAndOrFunds = Left v}) = v - getDoneVol order

-- market order *also* limited by Funds: we get an upper bound
getOpenVol order@(MarketOrder{volumeAndOrFunds = Right (Just v, _)}) = v - getDoneVol order

-- market order *only* limited by Funds: error
getOpenVol order@(MarketOrder{volumeAndOrFunds = Right (Nothing, _)}) =
  error $ "Cannot determine open volume for market order without volume constraint. " ++ show order
---------

getExecutedPrice
    :: (Show price, Show vol)
    => Order price vol (Confirmation price vol)
    -> Price price
getExecutedPrice order = case aConfirmation order of
    Conf { mExecuted = Just (p, _)} -> p
    _ -> error $ "Cannot determine already executed ave price in order without price info. " ++ show order

attachConfirmation :: Order p v ack -> (Confirmation p v) -> Order p v (Confirmation p v)
attachConfirmation order conf = order { aConfirmation = conf }

limitOrderMatches
    :: (Eq p, Eq v)
    => OrderSide
    -> Price p
    -> Vol v
    -> Order p v ack
    -> Bool
limitOrderMatches oSide' p v order@(LimitOrder{}) =
    oSide       order == oSide' &&
    limitPrice  order == p      &&
    limitVolume order == v

limitOrderMatches _s _p _v _ = False

--------------------------------------------
aggregateQuotes :: (Real v, RealFrac p) => [Quote p v tail] -> [(Cost p, Vol v)]
aggregateQuotes xs = aggregate $ map quoteToPair xs

quoteToPair :: Quote p v t -> (Price p, Vol v)
quoteToPair (Quote {price = p, volume = v }) = (p, v)

toQuote :: OrderSide -> (Price p, Vol v) -> Quote p v ()
toQuote sd (p, v) = Quote sd p v ()

-- | Calculates "cumulative volume book"
aggregate :: (Real v, RealFrac p) => [(Price p, Vol v)] -> [(Cost p, Vol v)]
aggregate xs = aggregate' 0 0 xs
  where
    aggregate'
      :: (Real v, RealFrac p)
      => Cost p
      -> Vol  v
      -> [(Price p, Vol v)]
      -> [(Cost  p, Vol v)]
    aggregate' _ _ [] = []
    aggregate' accC accV ((p,v):ls) =
      let accC' = accC + realToFrac (p * realToFrac v)
          accV' = accV + v
       in (accC',accV') : aggregate' accC' accV' ls


-----------------------
-- FIX ME! this and `aggregate` are folds.
-- List assumed to be *monotonically strictly increasing* in volume and cost
-- **zero volume entries are NOT allowed even as first entry**
disaggregate :: (Real v, RealFrac p) => [(Cost p, Vol v)] -> [(Price p, Vol v)]
disaggregate xs = disaggregate' 0 0 xs
  where
    disaggregate'
      :: (Real v, RealFrac p)
      => Cost p
      -> Vol  v
      -> [(Cost  p, Vol v)]
      -> [(Price p, Vol v)]
    disaggregate' _ _ [] = []
    disaggregate' initC initV ((c,v):ls) =
      let p' = realToFrac ( (c - initC) / realToFrac v')
          v' =  v - initV
       in (p', v') : disaggregate' c v ls

-----------------------
-- This is only used in Tax calculations right now. It's a little simpler than aggregate
-- FIX ME! Should these be unified?
accumulate :: (Num a, Num b) => [(a,b)] -> [(a,b)]
accumulate xs = accumulate' 0 0 xs
  where
    accumulate' :: (Num a, Num b) => a -> b -> [(a,b)] -> [(a,b)]
    accumulate' _ _ [] = []
    accumulate' accX accY ((x,y):ls) = let accX' = accX + x
                                           accY' = accY + y
                                           in  (accX',accY') : accumulate' accX' accY' ls


--------------------------------------------------------------------------------
{-
   What's a profitable transaction?
   Well, we can continue buying while the cost of buying one extra unit is smaller than the price
   we can sell it for. In other words, until marginal supply equals marginal demand.

   At that price point, we may have a little more supply or demand available. We use the smaller
   volume to buy and sell.

   From the aggregate curves we can calculate the total volume bought and sold,
   total cost and total revenue. So it's easy to calculate total profit from them
   (once we find the appropriate price point).

-}


-- | finds volume for which marginal demand and marginal supply cross.
-- assuming increasing prices for asks and decreasing prices for bids, walks list while
-- price bid > price ask (strictly greater)
-- returns the overall volume for which price bids > price asks

findProfitableVolume :: (Ord p, Ord v, Num v) => [Quote p v a] -> [Quote p v b] -> Vol v
findProfitableVolume asks' bids' = findCrossover (map quoteToPair asks') (map quoteToPair bids')


findCrossover
    :: (Ord p, Num v, Ord v)
    => [(Price p, Vol v)]
    -> [(Price p, Vol v)]
    -> Vol v
findCrossover [] _ = 0  -- no more supply or demand
findCrossover _ [] = 0

findCrossover (a:as) (b:bs)
  | fst a < fst b =   --- (Price, Volume)
      let
          pa = fst a -- price of ask
          va = snd a -- volume of ask
          pb = fst b -- price of bid
          vb = snd b -- volume of bid

       in case compare va vb of
            EQ -> va + findCrossover as bs     -- perfect volume match

            LT -> let leftover = ( pb , vb - va )  -- va was smaller, so we have left over volume in bids
                   in va + findCrossover as (leftover:bs)

            GT -> let leftover = ( pa , va - vb )  -- vb was smaller, so we have left over volume in asks
                   in vb + findCrossover (leftover:as) bs

  | otherwise = 0


--------------------------------------------------------------------------------
{- | Finds total value for given volume
volume assumed to be second coordinate in argument list (first is total cost)
list assumed to be aggregated. I.e. monotonically non-decreasing (total cost, total volume) pairs

Left is error, Right success

Function is made complicated by:
  - there may not be enough supply/demand in the list to match requested volume.
    In this case, we mark the error by returning a Left with the total (cost,volume) available

  - the fact that the given volume may be in between two points on the list.
    In this case, we have to do some interpolation to find the right value.
    (This is actually the most common case)

We assume vol parameter is non-negative.
-}

totalValue
  :: (Ord v, Real v, Fractional p)
  => Vol v
  -> [(Cost p, Vol v)]
  -> Either (Cost p, Vol v) (Cost p, Vol v)
totalValue 0 _  = Right (0, 0)
totalValue _ [] = Left  (0, 0)
-- list is not empty and v is not zero if we get here
totalValue vol xs =
    case (notEnoughs, overs) of
      (_,[]) ->
          -- the *strict* less than comparison in `isNotEnoughVolume` implies that
          -- all volume available is *not* sufficient to fulfill requested volume
          Left (last xs)
      (lows,h:_) ->
          -- either first bid in list is "larger than or equal to" requested volume
          -- or requested volume falls between two bids (may equal second one)
          let (c1,v1) = if null lows then (0,0) else last lows
              (c2,v2) = h
              totalCost = c1 + (c2 - c1) * realToFrac (toRational (vol - v1) / toRational (v2 - v1))
           in Right (totalCost, vol)
  where
    -- v = realToFrac vol
    isNotEnoughVolume (_, curV) = curV < vol -- strict comparison
    (notEnoughs, overs) = span isNotEnoughVolume xs


--------------------------------------------------------------------------------
{-

 a fee of 8.7% is input as 1.087
 a margin of 2% is input as 1.02

(i.e. both as gross returns)

** fee assumed positive and larger than 1 **

This function makes bids offer less money and asks request more money.
In other words, they make things more expensive and mimic the fees we pay.
-}
shave :: (Real p, Fractional p) => Double -> Quote p v t -> Quote p v t
shave fee quote =
  case quote of
    Quote Bid p v t -> Quote Bid (realToFrac (realToFrac p / fee)) v t
    Quote Ask p v t -> Quote Ask (realToFrac (realToFrac p * fee)) v t

feeBook :: (Real p, Fractional p) => Double -> QuoteBook p v q c -> QuoteBook p v q c
feeBook fee bk@(QuoteBook{ bids = bs, asks = as }) =
    bk{ bids = shave fee <$> bs
      , asks = shave fee <$> as }

--------------------------------------------------------------------------------
{- | returns:
 - how much money (i.e. 'profit') can be made with the given asks and bids
 - how much 'volume' (in BTCs) should be traded to obtain it and
 - how much they will 'cost'.
Assuming the percentage fees are given by 'fees' (this is a percentage NOT Dollars!)
and the money wishes to have a rate of return of at least 'margin' per transaction.
-}

-- FIX ME! This is clearly procedural... needs some TLC
-- | availableProfit :: fee -> margin -> asks -> bids -> (profit,volume,cost)
availableProfit
    :: (RealFrac p, Real v)
    => Double
    -> Double
    -> [Quote p v a]
    -> [Quote p v b]
    -> (Cost p, Vol v, Cost p)
availableProfit fee margin as bs = (tap , vtap, cost)
  where
    -- Calculate initial estimate for profitable trading volume
    -- at requested margin of return on investment.
    profitableBids     = map (shave fee) bs
    profitableAtMargin = map (shave margin) profitableBids
    -- initial estimate (may be smaller)
    vtap'              = findProfitableVolume as profitableAtMargin

    -- Calculate available volumes
    aggCosts           = aggregateQuotes as
    aggRevenues        = aggregateQuotes profitableBids
    ( _ , cVol')       = case (totalValue vtap' aggCosts) of
                            Left  costPair -> costPair
                            Right costPair -> costPair
    ( _ , rVol')       = case (totalValue vtap' aggRevenues) of
                            Left  revPair -> revPair
                            Right revPair -> revPair
    -- viable volume = minimum of 3 available volumes
    vtap               = minimum [vtap', cVol', rVol']

    -- recalculate with correct volume
    ( cost , _ )    = case (totalValue vtap aggCosts) of
                            Left  costPair -> costPair
                            Right costPair -> costPair
    ( revenue , _ ) = case (totalValue vtap aggRevenues) of
                            Left  revPair -> revPair
                            Right revPair -> revPair

    tap                = revenue - cost


---------------------------------------
-- | Returns how much we have to pay or can get (i.e. the total cost or revenue) to buy/sell 1 Bitcoin
--   from/to the given quotes (fixed 1 BTC constant makes this cost equal to the average price)
--   This returns nothing if we can't get that much volume from the quotes
getBestPrice
    :: (RealFrac p, Real v)
    => [Quote p v t]
    -> Maybe (Price p)
getBestPrice qs = case totalValue 1 (aggregateQuotes qs) of
        Left   _           -> Nothing
        Right (cost, _vol) -> Just $ realToFrac cost

-- | returns the best price on this side of the book independent of volume
-- (even if only 1 satoshi is available at this price, it's still the best price)
getBestPrice' :: [Quote p v t] -> Maybe (Price p)
getBestPrice' = fmap price . safeHead

----------------------------------------
-- | This function turns a market for BTC that is settled in dollars
-- into a market for dollars that is settled in BTC
--
-- An order to buy     3 BTCs at   2000 USD/BTC each is equivalent to
-- an order to sell 6000 USDs at 0.0005 BTC/USD each
-- swapping volume and cost dominations and OrderSide "inverts" the orderbook.
invert :: (RealFrac p, RealFrac v) => QuoteBook p v q c -> QuoteBook v p q c
invert bk@(QuoteBook{ bids = bs, asks = as }) =
    bk{ bids = invert' <$> as
      , asks = invert' <$> bs }
  where
    invert' q@(Quote{side = sd, price = p, volume = v}) =
        q{ side   = case sd of {Bid -> Ask; Ask -> Bid}
         , price  = realToFrac $ recip $ toRational p
         , volume = realToFrac $ toRational v * toRational p }

----------------------------------------
-- USD-BTC `merge` USD-LTC = BTC-LTC
merge
    :: forall p1 v1 q1 c1 v2 q2 c2.
       (RealFrac p1, RealFrac v1, RealFrac v2)
    => QuoteBook p1 v1 q1 c1
    -> QuoteBook p1 v2 q2 c2
    -> QuoteBook v1 v2 () ()
merge bk1 bk2 =
    QuoteBook
        { bids = toQuote Bid <$> disaggregate (merge' 0 0 0 cumA1s cumB2s)
        , asks = toQuote Ask <$> disaggregate (merge' 0 0 0 cumB1s cumA2s)
        , counter = ()}
  where
    cumA1s, cumB1s :: [(Cost p1, Vol v1)]
    cumA1s = aggregateQuotes (asks bk1)
    cumB1s = aggregateQuotes (bids bk1)

    cumA2s, cumB2s :: [(Cost p1, Vol v2)]
    cumA2s = aggregateQuotes (asks bk2)
    cumB2s = aggregateQuotes (bids bk2)

    -- initial cost and volumes required as curves are cumulative
    merge' :: Cost p1 -> Vol v1 -> Vol v2 -> [(Cost p1, Vol v1)] -> [(Cost p1, Vol v2)] -> [(Cost v1, Vol v2)]
    merge' _ _ _ [] _ = []
    merge' _ _ _ _ [] = []
    merge' ic iv1 iv2 ((c1,v1):xs) ((c2,v2):ys)
      | c1 == c2 = (realToFrac v1, v2) : merge' c1 v1 v2 xs ys
      | c1 < c2  = let r   = toRational (c1 - ic) / toRational (c2 - ic)
                       v2' = iv2 + realToFrac (r * toRational (v2 - iv2))
                    in (realToFrac v1, v2') : merge' c1 v1 v2' xs ((c2,v2):ys)

      | c1 > c2  = let r   = toRational (c2 - ic) / toRational (c1 - ic)
                       v1' = iv1 + realToFrac (r * toRational (v1 - iv1))
                    in (realToFrac v1', v2) : merge' c2 v1' v2 ((c1,v1):xs) ys
