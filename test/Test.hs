module Main where

import Test.HUnit

import Market.Types
import Market.Basics
import Razao.Util

---------------------------------------
main :: IO Counts
main = runTestTT tests

tests :: Test
tests =
  TestList
    [ TestLabel "Orderbook equality works"     quoteBookEq
    , TestLabel "Aggregate and disaggregate"   (roundTripAggregation asksSample)
    ]

---------------------------- TESTS --------------------------------
-- FIX ME! I'm getting double precision rounding error problems in aggregation/disaggregation.
asksSample :: [(Price, Volume)]
asksSample = [(256, 0.5),(512, 1),(576, 0.25)]

roundTripAggregation :: [(Price,Volume)] -> Test
roundTripAggregation samples = TestCase $
  do
    assertEqual "Aggregation followed by disaggregation is modifying list"
        samples (disaggregate $ aggregate samples)

quoteBookEq :: Test
quoteBookEq = TestCase $ do
  assertBool  "Fragmenting quotes should still compare equal" (book == book')
  assertBool  "Different prices but books compare equal"      (book /= book2)
  assertBool  "Different volumes but books compare equal"     (book /= book3)

book, book', book2, book3 :: QuoteBook () ()
book = QuoteBook{ bids = [bid1, bid2]
                , asks = [ask1]
                , counter = ()}

book' = book { bids = [bid3]}
book2 = book { asks = [ask2]}
book3 = book { asks = [ask3]}

bid1, bid2, bid3, ask1, ask2, ask3 :: Quote ()
bid1 = Quote { side = Bid, price = 600, volume = 0.7, qtail = ()}
bid2 = Quote { side = Bid, price = 600, volume = 0.3, qtail = ()}
bid3 = Quote { side = Bid, price = 600, volume = 1.0, qtail = ()}

ask1 = Quote { side = Ask, price = 1000, volume = 1,   qtail = ()}
ask2 = Quote { side = Ask, price = 1001, volume = 1,   qtail = ()}
ask3 = Quote { side = Ask, price = 1000, volume = 1.1, qtail = ()}


-- FIX ME! totalValue test missing
