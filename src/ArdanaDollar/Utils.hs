module ArdanaDollar.Utils (
  collaterizationRatio,
  maxDebtAllowed,
  roundUp,
  minCollateralRequired,
  liquidationPrice,
  availableToGenerate,
  availableToWithdraw,
) where

import ArdanaDollar.Types (CollaterizationRatio (Finite, Infinity, Zero))
import PlutusTx.Prelude
import PlutusTx.Ratio qualified as R
import Prelude qualified as Haskell

{-# INLINEABLE collaterizationRatio #-}

{- | Calculate current colllaterization ratio of a vault
 which has `coll` amount of collateral inside and
 `debt` amount of stablecoin debt
-}
collaterizationRatio ::
  -- | collateral/USD external exchange rate (>= 0)
  Rational ->
  -- | amount of collateral in the vault (>= 0)
  Integer ->
  -- | current total debt of the vault (>= 0)
  Integer ->
  CollaterizationRatio
collaterizationRatio exr coll debt
  | debt == 0, coll == 0 = Zero
  | debt == 0 = Infinity
  | otherwise = Finite (R.fromInteger coll * exr * (1 R.% debt))

{-# INLINEABLE maxDebtAllowed #-}

{- | For a given amount `coll` of collateral, calculate
 the maximum debt allowed to be taken out
 or the maximum debt before the vault is available for liquidation
-}
maxDebtAllowed ::
  -- | collateral/USD external exchange rate (>= 0)
  Rational ->
  -- | liquidation ratio (or the minimum collaterization ratio)
  Rational ->
  -- | amount of collateral in the vault (>= 0)
  Integer ->
  Integer
maxDebtAllowed exr liqRatio coll =
  R.truncate (R.fromInteger coll * exr * R.recip liqRatio)

{-# INLINEABLE roundUp #-}

{- | Round a `Rational` away from zero (as opposed to
 `PlutusTx.Ratio.truncate` which rounds towards zero)
-}
roundUp :: Rational -> Integer
roundUp x =
  let (n, f) = R.properFraction x
      num = R.numerator f
      den = R.denominator f
   in if num == 0
        then n
        else n + (Haskell.signum num * Haskell.signum den)

{-# INLINEABLE minCollateralRequired #-}

{- | For a given amount `debt` of stablecoin debt,
 calculate the minimum collateral amount required to maintain
 a collaterization ratio above the liquidation ratio
 (or minimum collaterization ratio)
-}
minCollateralRequired ::
  -- | collateral/USD external exchange rate (>= 0)
  Rational ->
  -- | liquidation ratio (or the minimum collaterization ratio)
  Rational ->
  -- | current total debt of the vault (>= 0)
  Integer ->
  Integer
minCollateralRequired exr liqRatio debt =
  roundUp (R.fromInteger debt * liqRatio * R.recip exr)

{-# INLINEABLE liquidationPrice #-}

{- | Calculate current liquidation price (that is,
 the price such that if collateral price falls below that,
 the vault becomes unsafe and is available for liquidation)
 of a vault
 which has `coll` amount of collateral inside and
 `debt` amount of stablecoin debt
-}
liquidationPrice ::
  -- | liquidation ratio (or the minimum collaterization ratio)
  Rational ->
  -- | amount of collateral in the vault (>= 0)
  Integer ->
  -- | current total debt of the vault (>= 0)
  Integer ->
  Maybe Rational
liquidationPrice liqRatio coll debt
  | coll == 0 = Nothing
  | otherwise = Just (R.fromInteger debt * liqRatio * (1 R.% coll))

{-# INLINEABLE availableToGenerate #-}

{- | The max stablecoin amount that is additionaly available to mint
 against the vault with these values
-}
availableToGenerate ::
  -- | collateral/USD external exchange rate (>= 0)
  Rational ->
  -- | liquidation ratio (or the minimum collaterization ratio)
  Rational ->
  -- | amount of collateral in the vault (>= 0)
  Integer ->
  -- | current total debt of the vault (>= 0)
  Integer ->
  Integer
availableToGenerate exr liqRatio coll debt =
  maxDebtAllowed exr liqRatio coll - debt

{-# INLINEABLE availableToWithdraw #-}

{- | The max collateral amount that can be withdrawn
 against the vault with these values before putting the vault
 at a risk of liquidation
-}
availableToWithdraw ::
  -- | collateral/USD external exchange rate (>= 0)
  Rational ->
  -- | liquidation ratio (or the minimum collaterization ratio)
  Rational ->
  -- | amount of collateral in the vault (>= 0)
  Integer ->
  -- | current total debt of the vault (>= 0)
  Integer ->
  Integer
availableToWithdraw exr liqRatio coll debt =
  coll - minCollateralRequired exr liqRatio debt
