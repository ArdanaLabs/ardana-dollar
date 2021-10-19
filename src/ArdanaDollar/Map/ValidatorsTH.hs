{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module ArdanaDollar.Map.ValidatorsTH (
  inst,
  nodeValidPolicy,
  Integer2IntegerMap,
) where

import Ledger qualified
import Ledger.Typed.Scripts qualified as Scripts
import Ledger.Value qualified as Value
import PlutusTx qualified
import PlutusTx.Prelude (
  Integer,
  ($),
  (.),
 )
import PlutusTx.TH qualified as TH

import ArdanaDollar.Map.MapTerms
import ArdanaDollar.Map.Types
import ArdanaDollar.Map.Validators

{-# INLINEABLE nodeValidPolicy #-}
nodeValidPolicy :: MapInstance -> Scripts.MintingPolicy
nodeValidPolicy mapInstance =
  Ledger.mkMintingPolicyScript $
    $$(TH.compile [||Scripts.wrapMintingPolicy . mkNodeValidPolicy @Integer @Integer||])
      `PlutusTx.applyCode` PlutusTx.liftCode mapInstance

{-# INLINEABLE nodeValidPolicySymbol #-}
nodeValidPolicySymbol :: MapInstance -> Value.CurrencySymbol
nodeValidPolicySymbol = Ledger.scriptCurrencySymbol . nodeValidPolicy

data ValidatorTypes
instance Scripts.ValidatorTypes ValidatorTypes where
  type DatumType ValidatorTypes = Datum Integer Integer
  type RedeemerType ValidatorTypes = Redeemer

{-# INLINEABLE _inst #-}
_inst ::
  MapInstance ->
  PointerCS ->
  Scripts.TypedValidator ValidatorTypes
_inst mapInstance pointerCS =
  Scripts.mkTypedValidator @ValidatorTypes
    ( $$(PlutusTx.compile [||mkValidator @Integer @Integer||])
        `PlutusTx.applyCode` PlutusTx.liftCode mapInstance
        `PlutusTx.applyCode` PlutusTx.liftCode pointerCS
    )
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.wrapValidator @(Datum Integer Integer) @Redeemer

--
{-# INLINEABLE inst #-}
inst :: MapInstance -> Scripts.TypedValidator ValidatorTypes
inst mapInstance = _inst mapInstance (PointerCS $ nodeValidPolicySymbol mapInstance)

data Integer2IntegerMap

instance MapTerms' Integer2IntegerMap where
  type ValidatorTypes' Integer2IntegerMap = ValidatorTypes
  type K' Integer2IntegerMap = Integer
  type V' Integer2IntegerMap = Integer
  nodeValidPolicy' = nodeValidPolicy
  inst' = inst

instance MapTerms Integer2IntegerMap