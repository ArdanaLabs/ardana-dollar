{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module ArdanaDollar.Types (CollaterizationRatio (Zero, Finite, Infinity)) where

import PlutusTx.Ratio (Rational)
import Prelude qualified as Haskell

-- | This type represents collateral/stablecoin ratio value of a vault
data CollaterizationRatio
  = -- | if collateral == 0 and stablecoin == 0 (unused vault)
    Zero
  | Finite Rational
  | -- | if only collateral == zero
    Infinity
  deriving stock (Haskell.Show, Haskell.Eq)
