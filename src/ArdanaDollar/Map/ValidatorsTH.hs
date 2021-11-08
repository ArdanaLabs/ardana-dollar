{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module ArdanaDollar.Map.ValidatorsTH (
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
 )
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

{-# INLINEABLE unlockPolicy #-}
unlockPolicy :: MapInstance -> PointerCS -> SnapshotCS -> Scripts.MintingPolicy
unlockPolicy mapInstance pointerCS snapshotCS =
  Ledger.mkMintingPolicyScript $
    $$(TH.compile [||\m p u -> Scripts.wrapMintingPolicy $ U.mkUnlockPolicy m p u||])
      `PlutusTx.applyCode` PlutusTx.liftCode mapInstance
      `PlutusTx.applyCode` PlutusTx.liftCode pointerCS
      `PlutusTx.applyCode` PlutusTx.liftCode snapshotCS

{-# INLINEABLE unlockPolicySymbol #-}
unlockPolicySymbol :: MapInstance -> PointerCS -> SnapshotCS -> Value.CurrencySymbol
unlockPolicySymbol mapInstance pointerCS snapshotCS =
  Ledger.scriptCurrencySymbol $ unlockPolicy mapInstance pointerCS snapshotCS

data ValidatorTypes
instance Scripts.ValidatorTypes ValidatorTypes where
  type DatumType ValidatorTypes = Datum Integer Integer
  type RedeemerType ValidatorTypes = Redeemer

{-# INLINEABLE inst' #-}
inst' ::
  MapInstance ->
  PointerCS ->
  SnapshotCS ->
  UnlockCS ->
  Scripts.TypedValidator ValidatorTypes
inst' mapInstance pointerCS snapshotCS unlockCS =
  Scripts.mkTypedValidator @ValidatorTypes
    ( $$(PlutusTx.compile [||V.mkValidator @Integer @Integer||])
        `PlutusTx.applyCode` PlutusTx.liftCode mapInstance
        `PlutusTx.applyCode` PlutusTx.liftCode pointerCS
        `PlutusTx.applyCode` PlutusTx.liftCode snapshotCS
        `PlutusTx.applyCode` PlutusTx.liftCode unlockCS
    )
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.wrapValidator @(Datum Integer Integer) @Redeemer

{-# INLINEABLE inst #-}
inst :: MapInstance -> Scripts.TypedValidator ValidatorTypes
inst mapInstance =
  let !s1 = PointerCS $ nodeValidPolicySymbol mapInstance
      !s2 = SnapshotCS $ snapshotPolicySymbol mapInstance s1
      !s3 = UnlockCS $ unlockPolicySymbol mapInstance s1 s2
   in inst' mapInstance s1 s2 s3

data Integer2IntegerMap

instance T.MapTerms' Integer2IntegerMap where
  type ValidatorTypes' Integer2IntegerMap = ValidatorTypes
  type K' Integer2IntegerMap = Integer
  type V' Integer2IntegerMap = Integer
  nodeValidPolicy' = nodeValidPolicy
  inst' = inst

instance T.MapTerms Integer2IntegerMap
