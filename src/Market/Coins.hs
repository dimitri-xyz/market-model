{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module Market.Coins where

import Data.Fixed
import Razao.Util
import Control.DeepSeq
import Data.Hashable

-------------------
-- Helpers from Data.Fixed
data E8
instance HasResolution E8 where
    resolution _ = 100000000

data E5
instance HasResolution E5 where
    resolution _ = 100000

-------------------
newtype BTC = BTC Double deriving (Num, Fractional, Real, RealFrac, NFData, Hashable)
instance Eq BTC where
  BTC x == BTC y = round8dp x == round8dp y
instance Ord BTC where
  BTC x `compare` BTC y = round8dp x `compare` round8dp y
instance Show BTC where
    show (BTC x) = show (realToFrac (round8dp x) :: Fixed E8)

-------------------
newtype LTC = LTC Double deriving (Num, Fractional, Real, RealFrac, NFData, Hashable)
instance Eq LTC where
  LTC x == LTC y = round8dp x == round8dp y
instance Ord LTC where
  LTC x `compare` LTC y = round8dp x `compare` round8dp y
instance Show LTC where
    show (LTC x) = show (realToFrac (round8dp x) :: Fixed E8)

-------------------
-- FIX ME! This will do for now, but ether has more precision. decimal-arithmetic package is an option.
newtype ETH = ETH Double deriving (Eq, Ord, Num, Fractional, Real, RealFrac, NFData, Hashable)
instance Show ETH where
  show (ETH vol) = show vol

-------------------
newtype USD = USD Double deriving (Num, Fractional, Real, RealFrac, NFData, Hashable)
instance Eq USD where
  USD x == USD y = round2dp x == round2dp y
instance Ord USD where
  USD x `compare` USD y = round2dp x `compare` round2dp y
instance Show USD where
    show (USD x) = show (realToFrac (round2dp x) :: Fixed E2)

-------------------
newtype BRL = BRL Double deriving (Num, Fractional, Real, RealFrac, NFData, Hashable)
instance Eq BRL where
  BRL x == BRL y = round5dp x == round5dp y
instance Ord BRL where
  BRL x `compare` BRL y = round5dp x `compare` round5dp y
instance Show BRL where
    show (BRL x) = show (realToFrac (round5dp x) :: Fixed E5)

-------------------
class (NFData coin, RealFrac coin, Show coin, Hashable coin) => Coin coin where
  coinSymbol :: coin -> String
  showBare   :: coin -> String
  readBare   :: String -> coin


instance Coin USD where
  coinSymbol _ = "USD"

instance Coin BRL where
  coinSymbol _ = "BRL"

instance Coin BTC where
  coinSymbol _ = "BTC"

instance Coin LTC where
  coinSymbol _ = "LTC"

instance Coin ETH where
  coinSymbol _ = "ETH"
