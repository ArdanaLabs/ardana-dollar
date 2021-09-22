{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module ArdanaDollar.Map.ValidatorsTH (
  inst,
  validator,
  address,
  nodeValidPolicy,
  nodeValidPolicySymbol,
  ValidatorTypes,
) where

import Ledger qualified
import Ledger.Typed.Scripts qualified as Scripts
import Ledger.Value qualified as Value
import PlutusTx qualified
import PlutusTx.Prelude
import PlutusTx.TH qualified as TH

import ArdanaDollar.Map.Types
import ArdanaDollar.Map.Validators

{-# INLINEABLE nodeValidPolicy #-}
nodeValidPolicy :: MapInstance -> Scripts.MintingPolicy
nodeValidPolicy mapInstance =
  Ledger.mkMintingPolicyScript $
    $$(TH.compile [||Scripts.wrapMintingPolicy . mkNodeValidPolicy||])
      `PlutusTx.applyCode` PlutusTx.liftCode mapInstance

{-# INLINEABLE nodeValidPolicySymbol #-}
nodeValidPolicySymbol :: MapInstance -> Value.CurrencySymbol
nodeValidPolicySymbol = Ledger.scriptCurrencySymbol . nodeValidPolicy

data ValidatorTypes
instance Scripts.ValidatorTypes ValidatorTypes where
  type DatumType ValidatorTypes = Datum
  type RedeemerType ValidatorTypes = Redeemer

{-# INLINEABLE _inst #-}
_inst ::
  MapInstance ->
  PointerCS ->
  Scripts.TypedValidator ValidatorTypes
_inst mapInstance pointerCS =
  Scripts.mkTypedValidator @ValidatorTypes
    ( $$(PlutusTx.compile [||mkValidator||])
        `PlutusTx.applyCode` PlutusTx.liftCode mapInstance
        `PlutusTx.applyCode` PlutusTx.liftCode pointerCS
    )
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.wrapValidator @Datum @Redeemer

--
{-# INLINEABLE inst #-}
inst :: MapInstance -> Scripts.TypedValidator ValidatorTypes
inst mapInstance = _inst mapInstance (PointerCS $ nodeValidPolicySymbol mapInstance)

{-# INLINEABLE validator #-}
validator :: MapInstance -> Ledger.Validator
validator mapInstance = Scripts.validatorScript $ inst mapInstance

{-# INLINEABLE address #-}
address :: MapInstance -> Ledger.Address
address mapInstance = Ledger.scriptAddress $ validator mapInstance
