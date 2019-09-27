module Main where

import Data.Maybe
import Test.HUnit

import Market.Types
import Market.Util
import Market.Coins

---------------------------------------
main :: IO Counts
main = runTestTT tests

tests :: Test
tests =
  TestList
    [ TestLabel "Orderbook equality works"     quoteBookEq
    , TestLabel "Aggregate and disaggregate"   (roundTripAggregation asksSample)
    , TestLabel "Return Total Value"           (getTotalValue asksSample)
    , TestLabel "Fee worsening of book works"  (testFeeBook book shavedBook)
    , TestLabel "Inverting orderbook works"    (invertOrderbook book invertedBook)
    , TestLabel "Merging orderbook works"      (mergeOrderbook firstBook otherBook mergedBook)
    , TestLabel "Numerically Unstable Book"    (unstableOrderbook bk1 bk2)
    ]

---------------------------- TESTS --------------------------------
-- FIX ME! I'm getting double precision rounding error problems in aggregation/disaggregation.
asksSample :: [(Price Double, Vol BTC)]
asksSample = [(256, 0.5),(512, 1),(576, 0.25)]

roundTripAggregation :: [(Price Double, Vol BTC)] -> Test
roundTripAggregation samples = TestCase $
  do
    assertEqual "Aggregation followed by disaggregation is modifying list"
        samples (disaggregate $ aggregate samples)

quoteBookEq :: Test
quoteBookEq = TestCase $ do
  assertBool  "Fragmenting quotes should still compare equal" (book == book')
  assertBool  "Different prices but books compare equal"      (book /= book2)
  assertBool  "Different volumes but books compare equal"     (book /= book3)

----------
book, book', book2, book3 :: QuoteBook USD BTC () ()
book = QuoteBook{ bids = [bid1, bid2]
                , asks = [ask1]
                , counter = ()}

book' = book { bids = [bid3]}
book2 = book { asks = [ask2]}
book3 = book { asks = [ask3]}

bid1, bid2, bid3, ask1, ask2, ask3 :: Quote USD BTC ()
bid1 = Quote { side = Bid, price = 600, volume = 0.7, qtail = ()}
bid2 = Quote { side = Bid, price = 600, volume = 0.3, qtail = ()}
bid3 = Quote { side = Bid, price = 600, volume = 1.0, qtail = ()}

ask1 = Quote { side = Ask, price = 1000, volume = 1,   qtail = ()}
ask2 = Quote { side = Ask, price = 1001, volume = 1,   qtail = ()}
ask3 = Quote { side = Ask, price = 1000, volume = 1.1, qtail = ()}

----------
shavedBook :: QuoteBook USD BTC () ()
shavedBook =
  QuoteBook
    { bids = [bid1', bid2']
    , asks = [ask1']
    , counter = ()}

bid1', bid2', ask1' :: Quote USD BTC ()
bid1' = Quote { side = Bid, price =  600 / 1.007, volume = 0.7, qtail = ()}
bid2' = Quote { side = Bid, price =  600 / 1.007, volume = 0.3, qtail = ()}
ask1' = Quote { side = Ask, price = 1000 * 1.007, volume = 1,   qtail = ()}

----------
invertedBook :: QuoteBook BTC USD () ()
invertedBook =
  QuoteBook
    { bids = [bid1'']
    , asks = [ask1'', ask2'']
    , counter = ()}

ask1'', ask2'', bid1'' :: Quote BTC USD ()
ask1'' = Quote { side = Ask, price =  1/600, volume = 0.7 *  600, qtail = ()}
ask2'' = Quote { side = Ask, price =  1/600, volume = 0.3 *  600, qtail = ()}
bid1'' = Quote { side = Bid, price = 1/1000, volume = 1   * 1000, qtail = ()}

----------
firstBook :: QuoteBook USD BTC () ()
firstBook = QuoteBook
                { bids = [fb1, fb2]
                , asks = [fa1]
                , counter = ()}

fb1, fb2, fa1 :: Quote USD BTC ()
fa1 = Quote { side = Ask, price = 1000, volume = 1,   qtail = ()}
fb1 = Quote { side = Bid, price =  500, volume = 0.7, qtail = ()}
fb2 = Quote { side = Bid, price =  500, volume = 0.3, qtail = ()}

otherBook :: QuoteBook USD LTC Int Int
otherBook =
  QuoteBook
    { bids = [b1, b2, b3]
    , asks = [a1, a2, a3]
    , counter = 555}

b1, b2, b3, a1, a2, a3 :: Quote USD LTC Int
b1 = Quote { side = Bid, price = 9, volume = 100, qtail = -1}
b2 = Quote { side = Bid, price = 8, volume =  20, qtail = -2}
b3 = Quote { side = Bid, price = 7, volume = 200, qtail = -3}

a1 = Quote { side = Ask, price = 10, volume = 40, qtail = -4}
a2 = Quote { side = Ask, price = 20, volume =  4, qtail = -5}
a3 = Quote { side = Ask, price = 30, volume =  1, qtail = -6}

----------
mergedBook :: QuoteBook BTC LTC () ()
mergedBook =
  QuoteBook
    { bids = [ Quote {side = Bid, price = Price 0.00900000, volume = Vol 100.00000000, qtail = ()}
             , Quote {side = Bid, price = Price 0.00800000, volume = Vol  12.50000000, qtail = ()}]

    , asks = [ Quote {side = Ask, price = Price 0.02000000, volume = Vol 35.00000000, qtail = ()}
             , Quote {side = Ask, price = Price 0.02000000, volume = Vol  5.00000000, qtail = ()}
             , Quote {side = Ask, price = Price 0.04000000, volume = Vol  4.00000000, qtail = ()}
             , Quote {side = Ask, price = Price 0.06000000, volume = Vol  0.66666667, qtail = ()}]

    , counter = ()}

----------
getTotalValue :: [(Price Double, Vol BTC)] -> Test
getTotalValue samples = TestCase $ do
    assertEqual "Returned wrong value for requested volume"
        (Right (256*0.5+512*1+576*0.1, 1.6)) (totalValue 1.6 $ aggregate samples)

testFeeBook :: (Coin p, Coin v, Eq q, Show q, Show c) => QuoteBook p v q c -> QuoteBook p v q c -> Test
testFeeBook sample result = TestCase $ do
    assertEqual "Applied fees mismatch expectation" result (feeBook 1.007 sample)

invertOrderbook :: (Coin p, Coin v, Eq q, Show q, Show c) => QuoteBook p v q c -> QuoteBook v p q c -> Test
invertOrderbook sample result = TestCase $ do
    assertEqual "Applied fees mismatch expectation" result (invert sample)

mergeOrderbook
    :: ( Coin p1, Coin v1, Eq q1, Show q1, Show c1
                , Coin v2, Eq q2, Show q2, Show c2)
    => QuoteBook p1 v1 q1 c1
    -> QuoteBook p1 v2 q2 c2
    -> QuoteBook v1 v2 () ()
    -> Test
mergeOrderbook part1 part2 result = TestCase $ do
    assertEqual "Merged orderbook does not match" result (part1 `merge` part2)

----------
unstableOrderbook
    :: ( Coin p1, Coin v1, Coin v2)
    => QuoteBook p1 v1 () ()
    -> QuoteBook v1 v2 () ()
    -> Test
unstableOrderbook quoteBook1 quoteBook2 = TestCase $ do
    let bk = feeBook (1.007 * 1.0025 * 1.007) $ merge (invert quoteBook1) quoteBook2
        a' = fromMaybe 99999 (getBestPrice' (asks bk))
    assertEqual "Merged orderbook ask does not match" 143.63699 a'

bk1 :: QuoteBook BRL BTC () ()
bk1 =
  QuoteBook
    { asks = [ Quote {side = Ask, price = Price 9039.90769, volume = Vol 0.04297070, qtail = ()}
             , Quote {side = Ask, price = Price 9039.9579 , volume = Vol 0.08888888, qtail = ()}
             , Quote {side = Ask, price = Price 9040.0000 , volume = Vol 0.09999999, qtail = ()}]

    , bids = [ Quote {side = Bid, price = Price 8905.00101, volume = Vol 0.59051716, qtail = ()}
             , Quote {side = Bid, price = Price 8905.001  , volume = Vol 0.11791000, qtail = ()}
             , Quote {side = Bid, price = Price 8891.0    , volume = Vol 0.02300000, qtail = ()}]

    , counter = ()}

bk2 :: QuoteBook BTC LTC () ()
bk2 =
  QuoteBook  -- first ask is a problem, has VERY small Volume at low price (cost < 1E-8)
    { asks = [ Quote {side = Ask, price = Price 0.01563000, volume = Vol 0.00000036, qtail = ()}
             , Quote {side = Ask, price = Price 0.01563000, volume = Vol 0.01514199, qtail = ()}
             , Quote {side = Ask, price = Price 0.01564000, volume = Vol 0.01803125, qtail = ()}]

    , bids = [ Quote {side = Bid, price = Price 0.01561000, volume = Vol  0.02098053, qtail = ()}
             , Quote {side = Bid, price = Price 0.01561000, volume = Vol 92.27239253, qtail = ()}
             , Quote {side = Bid, price = Price 0.01561000, volume = Vol 46.00000000, qtail = ()}]

    , counter = ()}
