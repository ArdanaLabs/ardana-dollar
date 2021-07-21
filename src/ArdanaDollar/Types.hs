{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DerivingStrategies #-}

module ArdanaDollar.Types where

import qualified Prelude as Haskell
import qualified PlutusTx.Ratio as R
import PlutusTx.Ratio (Rational)


-- | This type represents collateral/stablecoin ratio value of a vault
data CollaterizationRatio
  = Zero             -- ^ if collateral == 0 and stablecoin == 0 (unused vault)
  | Finite Rational
  | Infinity         -- ^ if only collateral == zero
  deriving stock (Haskell.Show)
