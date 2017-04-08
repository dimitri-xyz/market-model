module Util where

import           Text.Printf (printf)
import           Data.Foldable

--------------------------
safeHead :: [a] -> Maybe a
safeHead  []    = Nothing
safeHead  (a:_) = Just a

defaultHead :: a -> [a] -> a
defaultHead  a []    = a
defaultHead  a (h:_) = h
--------------------------

-- Generalizations (to distinct types a and b) from Data.List that are needed --
elem' :: ( a -> b -> Bool) -> a -> [b] -> Bool
elem' _   _  []    = False
elem' iso x (y:ys) = x `iso` y || elem' iso x ys

-- O(N^2) but good enough for now
isSubsetOf :: ( a -> b -> Bool ) -> [a] -> [b] -> Bool
isSubsetOf _   []     _      = True
isSubsetOf iso (a:as) bs = elem' iso a bs && isSubsetOf iso as bs

isEquivTo :: ( a -> b -> Bool) -> [a] -> [b] -> Bool
isEquivTo iso as bs = isSubsetOf iso as bs && isSubsetOf (flip iso) bs as
--------------------------------------------------------------------------------

-- ensures prices and volumes are *non-negative* numbers
maybePositive :: (Eq a, Num a) =>  a -> Maybe a
maybePositive x = if abs x == x then Just x else Nothing

----------
floor2dp :: (Fractional a, RealFrac a) => a -> a
floor2dp x = fromInteger(floor (100 * x )) / 100

ceiling2dp :: (Fractional a, RealFrac a) => a -> a
ceiling2dp x = fromInteger(ceiling (100 * x )) / 100

round2dp :: (Fractional a, RealFrac a) => a -> a
round2dp x = fromInteger(round (100 * x )) / 100

round5dp :: (Fractional a, RealFrac a) => a -> a
round5dp x = fromInteger(round (100000 * x )) / 100000

round8dp :: (Fractional a, RealFrac a) => a -> a
round8dp x = fromInteger(round (100000000 * x )) / 100000000


format2dp :: Double -> String
format2dp x = printf "%.2f" x

format5dp :: Double -> String
format5dp x = printf "%.5f" x

format8dp :: Double -> String
format8dp x = printf "%.8f" x

-- | (on ANSI terminals) Backtracks cursor as many lines as the string has
backtrackCursor :: String -> String
backtrackCursor ss =
    let newLines = foldl' (\count char -> if char == '\n' then (count + 1) else count) (0::Int) ss
     in ss ++ "\ESC[" ++ show newLines ++ "A"


--------------------------------------------------------------------------------
