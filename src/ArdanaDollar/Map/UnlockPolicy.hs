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
mkUnlockPolicy _ _ _ _ _ = False