{-# LANGUAGE NoImplicitPrelude #-}

module ArdanaDollar.Utils where

import qualified Prelude as Haskell
import PlutusTx.Prelude
import qualified PlutusTx.Ratio as R
import PlutusTx.Ratio (Rational)
import ArdanaDollar.Types -- TODO


{-# INLINABLE collaterizationRatio #-}
-- | Calculate current colllaterization ratio of a vault
-- which has `coll` amount of collateral inside and
-- `debt` amount of stablecoin debt
collaterizationRatio :: Rational -- ^ collateral/USD external exchange rate (>= 0)
                     -> Integer  -- ^ amount of collateral in the vault (>= 0)
                     -> Integer  -- ^ current total debt of the vault (>= 0)
                     -> CollaterizationRatio
collaterizationRatio exr coll debt
  | debt == 0, coll == 0 = Zero
  | debt == 0            = Infinity
  | otherwise            = Finite (R.fromInteger coll * exr * (1 R.% debt))

{-# INLINABLE maxDebtAllowed #-}
-- | For a given amount `coll` of collateral, calculate
-- the maximum debt allowed to be taken out
-- or the maximum debt before the vault is available for liquidation
maxDebtAllowed :: Rational -- ^ collateral/USD external exchange rate (>= 0)
               -> Rational -- ^ liquidation ratio (or the minimum collaterization ratio)
               -> Integer  -- ^ amount of collateral in the vault (>= 0)
               -> Integer
maxDebtAllowed exr liqRatio coll =
  R.truncate (R.fromInteger coll * exr * R.recip liqRatio)

{-# INLINABLE roundUp #-}
-- | Round a `Rational` away from zero (as opposed to
-- `PlutusTx.Ratio.truncate` which rounds towards zero)
roundUp :: Rational -> Integer
roundUp x =
  let (n, f) = R.properFraction x
      num = R.numerator f
      den = R.denominator f
  in if num == 0
     then n
     else n + (Haskell.signum num * Haskell.signum den)

{-# INLINABLE minCollateralRequired #-}
-- | For a given amount `debt` of stablecoin debt,
-- calculate the minimum collateral amount required to maintain
-- a collaterization ratio above the liquidation ratio
-- (or minimum collaterization ratio)
minCollateralRequired :: Rational -- ^ collateral/USD external exchange rate (>= 0)
                      -> Rational -- ^ liquidation ratio (or the minimum collaterization ratio)
                      -> Integer  -- ^ current total debt of the vault (>= 0)
                      -> Integer
minCollateralRequired exr liqRatio debt =
  roundUp (R.fromInteger debt * liqRatio * R.recip exr)

{-# INLINABLE liquidationPrice #-}
-- | Calculate current liquidation price (that is,
-- the price such that if collateral price falls below that,
-- the vault becomes unsafe and is available for liquidation)
-- of a vault
-- which has `coll` amount of collateral inside and
-- `debt` amount of stablecoin debt
liquidationPrice :: Rational -- ^ liquidation ratio (or the minimum collaterization ratio)
                 -> Integer  -- ^ amount of collateral in the vault (>= 0)
                 -> Integer  -- ^ current total debt of the vault (>= 0)
                 -> Rational
liquidationPrice liqRatio coll debt =
  R.fromInteger debt * liqRatio * (1 R.% coll)

{-# INLINABLE availableToGenerate #-}
-- | The max stablecoin amount that is additionaly available to mint
-- against the vault with these values
availableToGenerate :: Rational -- ^ collateral/USD external exchange rate (>= 0)
                    -> Rational -- ^ liquidation ratio (or the minimum collaterization ratio)
                    -> Integer  -- ^ amount of collateral in the vault (>= 0)
                    -> Integer  -- ^ current total debt of the vault (>= 0)
                    -> Integer
availableToGenerate exr liqRatio coll debt =
  maxDebtAllowed exr liqRatio coll - debt

{-# INLINABLE availableToWithdraw #-}
-- | The max collateral amount that can be withdrawn
-- against the vault with these values before putting the vault
-- at a risk of liquidation
availableToWithdraw :: Rational -- ^ collateral/USD external exchange rate (>= 0)
                    -> Rational -- ^ liquidation ratio (or the minimum collaterization ratio)
                    -> Integer  -- ^ amount of collateral in the vault (>= 0)
                    -> Integer  -- ^ current total debt of the vault (>= 0)
                    -> Integer
availableToWithdraw exr liqRatio coll debt = 
  coll - minCollateralRequired exr liqRatio debt
