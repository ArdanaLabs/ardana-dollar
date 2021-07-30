{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module ArdanaDollar.Types (CollaterizationRatio (Zero, Finite, Infinity)) where

import PlutusTx.Prelude
import qualified Prelude as Haskell

-- | This type represents collateral/stablecoin ratio value of a vault
data CollaterizationRatio
  = -- | if collateral == 0 and stablecoin == 0 (unused vault)
    Zero
  | Finite Rational
  | -- | if only collateral == zero
    Infinity
  deriving stock (Haskell.Show, Haskell.Eq)

instance Eq CollaterizationRatio where
  Zero == Zero = True
  Finite x == Finite y = x == y
  Infinity == Infinity = True
  _ == _ = False

instance Ord CollaterizationRatio where
  -- Infinity > Zero > Finite x
  compare Infinity Infinity = EQ
  compare Infinity _ = GT
  compare Zero Infinity = LT
  compare Zero Zero = EQ
  compare Zero (Finite _) = GT
  compare (Finite x) (Finite y) = compare x y
  compare (Finite _) _ = LT
