module ArdanaDollar.Utils (
  collaterizationRatio,
  maxDebtAllowed,
  roundUp,
  minCollateralRequired,
  liquidationPrice,
  availableToGenerate,
  availableToWithdraw,
  scriptInputsAt,
  valueUnlockedBy,
  pubKeyInputsAt,
  valuePaidBy,
  datumForOffchain,
  datumForOnchain,
  validateDatumImmutable,
  safeDivide,
  safeRemainder,
  safeDivMod,
) where

import Data.Kind (Type)
import Data.Map qualified as Map

import ArdanaDollar.Types (CollaterizationRatio (Finite, Infinity, Zero))
import Ledger qualified
import Ledger.Contexts qualified as Contexts
import Ledger.Credential (Credential (PubKeyCredential, ScriptCredential))
import PlutusTx qualified
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

{-# INLINEABLE scriptInputsAt #-}

{- | The values of UTXO inputs being spent
 from the script address in the pending transaction
-}
scriptInputsAt :: Ledger.ValidatorHash -> Ledger.TxInfo -> [Ledger.Value]
scriptInputsAt h txInfo =
  [ value
  | Ledger.TxInInfo
      { Ledger.txInInfoResolved =
        Ledger.TxOut
          { Ledger.txOutAddress = Ledger.Address (ScriptCredential s) _
          , Ledger.txOutValue = value
          }
      } <-
      Ledger.txInfoInputs txInfo
  , s == h
  ]

{-# INLINEABLE valueUnlockedBy #-}

-- | The total value unlocked by the given validator in this transaction
valueUnlockedBy :: Ledger.TxInfo -> Ledger.ValidatorHash -> Ledger.Value
valueUnlockedBy txInfo h = mconcat (scriptInputsAt h txInfo)

{-# INLINEABLE pubKeyInputsAt #-}

{- | The values of UTXO inputs paid by a public key address
 in a pending transaction
-}
pubKeyInputsAt :: Ledger.PubKeyHash -> Ledger.TxInfo -> [Ledger.Value]
pubKeyInputsAt pk txInfo =
  [ value
  | Ledger.TxInInfo
      { Ledger.txInInfoResolved =
        Ledger.TxOut
          { Ledger.txOutAddress = Ledger.Address (PubKeyCredential pk') _
          , Ledger.txOutValue = value
          }
      } <-
      Ledger.txInfoInputs txInfo
  , pk == pk'
  ]

{-# INLINEABLE valuePaidBy #-}

{- | Get the total value paid by a public key address
 in a pending transaction
-}
valuePaidBy :: Ledger.TxInfo -> Ledger.PubKeyHash -> Ledger.Value
valuePaidBy txInfo pk = mconcat (pubKeyInputsAt pk txInfo)

{-# INLINEABLE datumFor #-}
datumFor :: forall (a :: Type). PlutusTx.FromData a => Ledger.TxOut -> (Ledger.DatumHash -> Maybe Ledger.Datum) -> Maybe a
datumFor txOut f = do
  dh <- Ledger.txOutDatum txOut
  Ledger.Datum d <- f dh
  PlutusTx.fromBuiltinData d

datumForOffchain :: forall (a :: Type). PlutusTx.FromData a => Ledger.TxOutTx -> Maybe a
datumForOffchain txOutTx = datumFor (Ledger.txOutTxOut txOutTx) $ \dh -> Map.lookup dh $ Ledger.txData $ Ledger.txOutTxTx txOutTx

{-# INLINEABLE datumForOnchain #-}
datumForOnchain :: forall (a :: Type). PlutusTx.FromData a => Ledger.TxInfo -> Ledger.TxOut -> Maybe a
datumForOnchain info txOut = datumFor txOut $ \dh -> Ledger.findDatum dh info

-- | On-chain helper function checking immutability of the validator's datum
{-# INLINEABLE validateDatumImmutable #-}
validateDatumImmutable ::
  forall (datum :: Type).
  (Eq datum, PlutusTx.FromData datum) =>
  datum ->
  Contexts.ScriptContext ->
  Bool
validateDatumImmutable td ctx =
  traceIfFalse "datum has changed" (Just td == outputDatum)
  where
    info :: Contexts.TxInfo
    info = Contexts.scriptContextTxInfo ctx

    outputDatum :: Maybe datum
    outputDatum = datumForOnchain info ownOutput

    ownOutput :: Ledger.TxOut
    ownOutput = case Contexts.getContinuingOutputs ctx of
      [o] -> o
      _ -> traceError "expected exactly one output"

-- Both `safeDiv` and `safeRem` works on Integers; Haskell's Integral instance
-- for Integer throws only on 0, hence we are allowed to ignore the HLint by
-- pattern-matching on the divisor
{- HLINT ignore safeDivide "Avoid restricted function" -}
{-# INLINEABLE safeDivide #-}
safeDivide :: Integer -> Integer -> Maybe Integer
a `safeDivide` b
  | b == 0 = Nothing
  | otherwise = Just $ a `divide` b

{- HLINT ignore safeRemainder "Avoid restricted function" -}
{-# INLINEABLE safeRemainder #-}
safeRemainder :: Integer -> Integer -> Maybe Integer
a `safeRemainder` b
  | b == 0 = Nothing
  | otherwise = Just $ a `remainder` b

safeDivMod :: Integer -> Integer -> Maybe (Integer, Integer)
a `safeDivMod` b = (,) <$> (a `safeDivide` b) <*> (a `safeRemainder` b)
