module ArdanaDollar.Treasury.OnChain (
  mkTreasuryValidator,
) where

--------------------------------------------------------------------------------

import Ledger qualified
import Ledger.Value qualified as Value
import Plutus.V1.Ledger.Contexts qualified as Contexts
import PlutusTx.Prelude

--------------------------------------------------------------------------------

import ArdanaDollar.Treasury.Types
import ArdanaDollar.Utils (validateDatumImmutable)

{-# INLINEABLE mkTreasuryValidator #-}
mkTreasuryValidator :: Treasury -> Value.AssetClass -> Value.AssetClass -> TreasuryDatum -> TreasuryAction -> Contexts.ScriptContext -> Bool
mkTreasuryValidator treasury _danaAC _dusdAsset td _redeemer ctx =
  traceIfFalse "treasury token missing from input" (hasToken ownInput)
    && traceIfFalse "treasury token missing from output" (hasToken ownOutput)
    && validateDatumImmutable td ctx
  where
    ownInput :: Ledger.TxOut
    ownInput = case Contexts.findOwnInput ctx of
      Nothing -> traceError "treasury input missing"
      Just i -> Contexts.txInInfoResolved i

    ownOutput :: Ledger.TxOut
    ownOutput = case Contexts.getContinuingOutputs ctx of
      [o] -> o
      _ -> traceError "expected exactly one treasury output"

    hasToken :: Ledger.TxOut -> Bool
    hasToken o = Value.assetClassValueOf (Ledger.txOutValue o) (tSymbol treasury) == 1
