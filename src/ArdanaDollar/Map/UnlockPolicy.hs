{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module ArdanaDollar.Map.UnlockPolicy (
  mkUnlockPolicy,
) where

-- import PlutusTx.IsData.Class (FromData)

import ArdanaDollar.Map.Types (
  MapInstance,
  PointerCS,
  SnapshotCS,
  UnlockTokenRedeemer,
 )

-- import ArdanaDollar.Map.Types qualified as T

import Ledger qualified
import PlutusTx.Prelude

{-# INLINEABLE mkUnlockPolicy #-}
mkUnlockPolicy ::
  -- forall k v.
  -- (Ord k, Eq v, FromData k, FromData v) =>
  MapInstance ->
  PointerCS ->
  SnapshotCS ->
  UnlockTokenRedeemer ->
  Ledger.ScriptContext ->
  Bool
mkUnlockPolicy _ _ _ _ ctx =
  let l = length $ Ledger.txInfoInputs $ Ledger.scriptContextTxInfo ctx
      l1 = l + 1
      l2 = l + 2
      l3 = l + 3
      l4 = l + 4
      l5 = l + 5
      l6 = l + 6
      l7 = l + 7
      l8 = l + 8
      l9 = l + 9
      l10 = l + 19
      l11 = l + 11
      l12 = l + 12
      l13 = l + 13
      l14 = l + 14
      l15 = l + 15
      l16 = l + 16
      l17 = l + 17
      l18 = l + 18
      l19 = l + 19
   in -- l20 = l + 20
      l == 0
        && l1 == 0
        && l2 == 0
        && l3 == 0
        && l4 == 0
        && l5 == 0
        && l6 == 0
        && l7 == 0
        && l8 == 0
        && l9 == 0
        && l10 == 0
        && l11 == 0
        && l12 == 0
        && l13 == 0
        && l14 == 0
        && l15 == 0
        && l16 == 0
        && l17 == 0
        && l18 == 0
        && l19 == 0

-- && l20 == 0