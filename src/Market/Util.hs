module Market.Util where

import Data.List.Extended
import Razao.Util
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
showTopN :: Show b => Int -> QuoteBook a b -> String
showTopN n b =
    let bids' =           take n $ bids b
        asks' = reverse $ take n $ asks b
        formatRow :: Quote a -> String
        formatRow x = show (side x) ++
            "   vol: " ++ show (volume x) ++
            " price: " ++ show (price  x) ++ "            \n"

     in show (counter b) ++
            "                                             \n" ++
            concatMap formatRow asks' ++
            "---                                          \n" ++
            concatMap formatRow bids'
----------------------------------------------


isActive :: Order Confirmation -> Bool
isActive o =
  case mOrderStatus (aConfirmation o) of
    Just st  -> st == Active || st == ActivePartiallyExecuted
    Nothing -> error $ "Order Status unknown. Cannot determine if order is active. " ++ show o

isBid :: Order a -> Bool
isBid = (== Bid) . oSide

isAsk :: Order a -> Bool
isAsk = (== Ask) . oSide

isLimitOrder :: Order a -> Bool
isLimitOrder LimitOrder{} = True
isLimitOrder _            = False

getOrderID :: Order Confirmation -> OrderID
getOrderID = orderID . aConfirmation

sameOrderID :: OrderID -> Order Confirmation -> Bool
sameOrderID oid order = (oid == getOrderID order)

getDoneVol :: Order Confirmation -> Volume
getDoneVol order = case aConfirmation order of
    Conf { mExecuted = Just (_, vol)} -> vol
    _ -> error $ "Cannot determine already executed volume in order without volume info. " ++ show order

---------
-- Market orders can be larger than the whole orderbook or limited by funds
getOpenVol :: Order Confirmation -> Volume
-- limit order: ok
getOpenVol order@(LimitOrder{}) = limitVolume order - getDoneVol order

-- market order *only* limited by Volume: ok
getOpenVol order@(MarketOrder{volumeAndOrFunds = Left v}) = v - getDoneVol order

-- market order *also* limited by Funds: we get an upper bound
getOpenVol order@(MarketOrder{volumeAndOrFunds = Right (Just v, _)}) = v - getDoneVol order

-- market order *only* limited by Funds: error
getOpenVol order@(MarketOrder{volumeAndOrFunds = Right (Just v, _)}) =
  error $ "Cannot determine open volume for market order without volume constraint. " ++ show order
---------

getExecutedPrice :: Order Confirmation -> Price
getExecutedPrice order = case aConfirmation order of
    Conf { mExecuted = Just (p, _)} -> p
    _ -> error $ "Cannot determine already executed ave price in order without price info. " ++ show order

attachConfirmation :: Order a -> Confirmation -> Order Confirmation
attachConfirmation order conf = order { aConfirmation = conf }

limitOrderMatches :: OrderSide -> Price -> Volume -> Order a -> Bool
limitOrderMatches oSide' p v order@(LimitOrder{}) =
    oSide       order           == oSide' &&
    limitVolume order           == v      &&
    round5dp (limitPrice order) == round5dp p

limitOrderMatches _s _p _v _ = False

--------------------------------------------
-- FIX ME! the return value should be specified in the same currency
-- and volume units, for now, it's just Double
aggregateQuotes :: [Quote a] -> [(Double, Volume)]
aggregateQuotes xs = aggregate $ map quoteToPair xs

quoteToPair :: Quote a -> (Double, Volume)
quoteToPair (Quote {price = p, volume = v }) = (p, v)

-- | Calculates "cumulative volume book"
aggregate :: [(Price,Volume)] -> [(Cost,Volume)]  -- FIX ME! I should be using different  Units here
aggregate xs = aggregate' 0 0 xs
  where
    aggregate'
      :: (Fractional cost, Real vol, RealFrac price)
      => cost
      -> vol
      -> [(price, vol)]
      -> [(cost,  vol)]
    aggregate' _ _ [] = []
    aggregate' accC accV ((p,v):ls) =
      let accC' = accC + realToFrac (p * realToFrac v)
          accV' = accV + v
       in (accC',accV') : aggregate' accC' accV' ls


-----------------------
-- FIX ME! this and `aggregate` are folds.
-- List assumed to be *monotonically strictly increasing* in volume and cost
-- **zero volume entries are NOT allowed even as first entry**
disaggregate :: [(Cost,Volume)] -> [(Price,Volume)]  -- FIX ME! I should be using Units here
disaggregate xs = disaggregate' 0 0 xs
  where
    disaggregate'
      :: (Fractional price, Real vol, RealFrac cost)
      => cost
      -> vol
      -> [(cost,  vol)]
      -> [(price, vol)]
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

findProfitableVolume :: [Quote a] -> [Quote b] -> Volume
findProfitableVolume asks' bids' = findCrossover (map quoteToPair asks') (map quoteToPair bids')


findCrossover
    :: (Ord price, Num vol, Ord vol)
    => [(price, vol)]
    -> [(price, vol)]
    -> vol
findCrossover [] _ = 0  -- no more supply or demand
findCrossover _ [] = 0

findCrossover (ask:asks) (bid:bids)
  | fst ask < fst bid =   --- (Price, Volume)
      let
          pa = fst ask -- price of ask
          va = snd ask -- volume of ask
          pb = fst bid -- price of bid
          vb = snd bid -- volume of bid

       in case compare va vb of
            EQ -> va + findCrossover asks bids     -- perfect volume match

            LT -> let leftover = ( pb , vb - va )  -- va was smaller, so we have left over volume in bids
                   in va + findCrossover asks (leftover:bids)

            GT -> let leftover = ( pa , va - vb )  -- vb was smaller, so we have left over volume in asks
                   in vb + findCrossover (leftover:asks) bids

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
  :: (Ord vol, Real vol, Fractional cost)
  => vol
  -> [(cost, vol)]
  -> Either (cost, vol) (cost, vol)
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
shave :: Double -> Quote a -> Quote a
shave fee quote = case quote of
                       Quote Bid p v t -> Quote Bid (p/fee) v t
                       Quote Ask p v t -> Quote Ask (p*fee) v t


--------------------------------------------------------------------------------
{- | returns:
 - how much money (i.e. 'profit') can be made with the given asks and bids
 - how much 'volume' (in BTCs) should be traded to obtain it and
 - how much they will 'cost'.
Assuming the percentage fees are given by 'fees' (this is a percentage NOT Dollars!)
and the money wishes to have a rate of return of at least 'margin' per transaction.
-}

-- | availableProfit :: fee -> margin -> asks -> bids -> (profit,volume,cost)
availableProfit :: Double -> Double -> [Quote a] -> [Quote b] -> (Double, Volume, Double)
availableProfit fee margin asks bids = let

                    -- Calculate initial estimate for profitable trading volume
                    -- at requested margin of return on investment.
                    profitableBids     = map (shave fee) bids
                    profitableAtMargin = map (shave margin) profitableBids
                    -- initial estimate (may be smaller)
                    vtap'              = findProfitableVolume asks profitableAtMargin

                    -- Calculate available volumes
                    aggCosts           = aggregateQuotes asks
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

                    in (tap , vtap, cost)

---------------------------------------
-- | Returns how much we have to pay or can get (i.e. the total cost or revenue) to buy/sell 1 Bitcoin
--   from/to the given quotes (fixed 1 BTC constant makes this cost equal to the average price)
--   This returns nothing if we can't get that much volume from the quotes
getBestPrice :: [Quote a] -> Maybe Price
getBestPrice qs = case totalValue 1 (aggregateQuotes qs) of
        Left   _           -> Nothing
        Right (cost, _vol) -> Just cost

-- | returns the best price on this side of the book independent of volume
-- (even if only 1 satoshi is available at this price, it's still the best price)
getBestPrice' :: [Quote a] -> Maybe Price
getBestPrice' = fmap price . safeHead
