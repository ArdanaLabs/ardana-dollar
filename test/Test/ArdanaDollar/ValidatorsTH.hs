{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.ArdanaDollar.ValidatorsTH (
  inst,
  nodeValidPolicy,
  snapshotPolicy,
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

import ArdanaDollar.Map.MapTerms qualified as T
import ArdanaDollar.Map.NodeValidPolicy qualified as P
import ArdanaDollar.Map.SnapshotPolicy qualified as S
import ArdanaDollar.Map.Types (
  Datum,
  MapInstance,
  PointerCS (PointerCS),
  Redeemer,
  SnapshotCS (SnapshotCS),
  UnlockCS (UnlockCS),
  UnlockPermCS (UnlockPermCS),
 )
import ArdanaDollar.Map.UnlockPermPolicy qualified as U
import ArdanaDollar.Map.UnlockPolicy qualified as U
import ArdanaDollar.Map.Validator qualified as V

{-# INLINEABLE nodeValidPolicy #-}
nodeValidPolicy :: MapInstance -> Scripts.MintingPolicy
nodeValidPolicy mapInstance =
  Ledger.mkMintingPolicyScript $
    $$(TH.compile [||Scripts.wrapMintingPolicy . P.mkNodeValidPolicy @Integer @Integer||])
      `PlutusTx.applyCode` PlutusTx.liftCode mapInstance

{-# INLINEABLE nodeValidPolicySymbol #-}
nodeValidPolicySymbol :: MapInstance -> Value.CurrencySymbol
nodeValidPolicySymbol = Ledger.scriptCurrencySymbol . nodeValidPolicy

{-# INLINEABLE snapshotPolicy #-}
snapshotPolicy :: MapInstance -> PointerCS -> Scripts.MintingPolicy
snapshotPolicy mapInstance pointerCS =
  Ledger.mkMintingPolicyScript $
    $$(TH.compile [||\m p -> Scripts.wrapMintingPolicy $ S.mkSnapshotPolicy @Integer @Integer m p||])
      `PlutusTx.applyCode` PlutusTx.liftCode mapInstance
      `PlutusTx.applyCode` PlutusTx.liftCode pointerCS

{-# INLINEABLE snapshotPolicySymbol #-}
snapshotPolicySymbol :: MapInstance -> PointerCS -> Value.CurrencySymbol
snapshotPolicySymbol mapInstance pointerCS =
  Ledger.scriptCurrencySymbol $ snapshotPolicy mapInstance pointerCS

{-# INLINEABLE unlockPermPolicy #-}
unlockPermPolicy :: MapInstance -> PointerCS -> SnapshotCS -> Scripts.MintingPolicy
unlockPermPolicy mapInstance pointerCS snapshotCS =
  Ledger.mkMintingPolicyScript $
    $$(TH.compile [||\m p u -> Scripts.wrapMintingPolicy $ U.mkUnlockPermPolicy @Integer @Integer m p u||])
      `PlutusTx.applyCode` PlutusTx.liftCode mapInstance
      `PlutusTx.applyCode` PlutusTx.liftCode pointerCS
      `PlutusTx.applyCode` PlutusTx.liftCode snapshotCS

{-# INLINEABLE unlockPermPolicySymbol #-}
unlockPermPolicySymbol :: MapInstance -> PointerCS -> SnapshotCS -> Value.CurrencySymbol
unlockPermPolicySymbol mapInstance pointerCS snapshotCS =
  Ledger.scriptCurrencySymbol $ unlockPermPolicy mapInstance pointerCS snapshotCS

{-# INLINEABLE unlockPolicy #-}
unlockPolicy :: MapInstance -> PointerCS -> Scripts.MintingPolicy
unlockPolicy mapInstance pointerCS =
  Ledger.mkMintingPolicyScript $
    $$(TH.compile [||\m p -> Scripts.wrapMintingPolicy $ U.mkUnlockPolicy @Integer @Integer m p||])
      `PlutusTx.applyCode` PlutusTx.liftCode mapInstance
      `PlutusTx.applyCode` PlutusTx.liftCode pointerCS

{-# INLINEABLE unlockPolicySymbol #-}
unlockPolicySymbol :: MapInstance -> PointerCS -> Value.CurrencySymbol
unlockPolicySymbol mapInstance pointerCS =
  Ledger.scriptCurrencySymbol $ unlockPolicy mapInstance pointerCS

data ValidatorTypes
instance Scripts.ValidatorTypes ValidatorTypes where
  type DatumType ValidatorTypes = Datum Integer Integer
  type RedeemerType ValidatorTypes = Redeemer

{-# INLINEABLE inst' #-}
inst' ::
  MapInstance ->
  PointerCS ->
  SnapshotCS ->
  UnlockPermCS ->
  UnlockCS ->
  Scripts.TypedValidator ValidatorTypes
inst' mapInstance pointerCS snapshotCS unlockPermCS unlockCS =
  Scripts.mkTypedValidator @ValidatorTypes
    ( $$(PlutusTx.compile [||V.mkValidator @Integer @Integer||])
        `PlutusTx.applyCode` PlutusTx.liftCode mapInstance
        `PlutusTx.applyCode` PlutusTx.liftCode pointerCS
        `PlutusTx.applyCode` PlutusTx.liftCode snapshotCS
        `PlutusTx.applyCode` PlutusTx.liftCode unlockPermCS
        `PlutusTx.applyCode` PlutusTx.liftCode unlockCS
    )
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.wrapValidator @(Datum Integer Integer) @Redeemer

{-# INLINEABLE inst #-}
inst :: MapInstance -> Scripts.TypedValidator ValidatorTypes
inst mapInstance =
  let s1 = PointerCS $ nodeValidPolicySymbol mapInstance
      s2 = SnapshotCS $ snapshotPolicySymbol mapInstance s1
      s3 = UnlockPermCS $ unlockPermPolicySymbol mapInstance s1 s2
      s4 = UnlockCS $ unlockPolicySymbol mapInstance s1
   in inst' mapInstance s1 s2 s3 s4

data Integer2IntegerMap

instance T.MapTerms' Integer2IntegerMap where
  type ValidatorTypes' Integer2IntegerMap = ValidatorTypes
  type K' Integer2IntegerMap = Integer
  type V' Integer2IntegerMap = Integer
  nodeValidPolicy' = nodeValidPolicy
  snapshotPolicy' mapInstance =
    snapshotPolicy
      mapInstance
      (PointerCS $ nodeValidPolicySymbol mapInstance)
  unlockPermPolicy' mapInstance =
    unlockPermPolicy
      mapInstance
      (PointerCS $ nodeValidPolicySymbol mapInstance)
      (SnapshotCS $ snapshotPolicySymbol mapInstance (PointerCS $ nodeValidPolicySymbol mapInstance))
  unlockPolicy' mapInstance =
    unlockPolicy
      mapInstance
      (PointerCS $ nodeValidPolicySymbol mapInstance)
  inst' = inst

instance T.MapTerms Integer2IntegerMap
