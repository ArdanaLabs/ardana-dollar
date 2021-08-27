{-# LANGUAGE TemplateHaskell #-}

module ArdanaDollar.DanaStakePool.DanaCurrency (
  danaAsset,
) where

import PlutusTx.Prelude

import Ledger
import Ledger.Typed.Scripts
import Ledger.Value
import PlutusTx

{-# INLINEABLE danaTokenName #-}
danaTokenName :: TokenName
danaTokenName = TokenName "dana"

{-# INLINEABLE mkDanaMintingPolicy #-}
mkDanaMintingPolicy :: TokenName -> () -> Ledger.ScriptContext -> Bool
mkDanaMintingPolicy danaToken _ Ledger.ScriptContext {scriptContextTxInfo = txInfo} =
  PlutusTx.Prelude.map (\(_, tn, _) -> tn) (flattenValue (Ledger.txInfoForge txInfo)) == [danaToken]

{-# INLINEABLE danaMintingPolicy #-}
danaMintingPolicy :: Ledger.MintingPolicy
danaMintingPolicy =
  Ledger.mkMintingPolicyScript $
    $$(PlutusTx.compile [||wrapMintingPolicy . mkDanaMintingPolicy||])
      `PlutusTx.applyCode` PlutusTx.liftCode danaTokenName

{-# INLINEABLE danaCurrency #-}
danaCurrency :: CurrencySymbol
danaCurrency = Ledger.scriptCurrencySymbol danaMintingPolicy

{-# INLINEABLE danaAsset #-}
danaAsset :: AssetClass
danaAsset = AssetClass (danaCurrency, danaTokenName)
