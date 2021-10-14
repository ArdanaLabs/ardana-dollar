{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module ArdanaDollar.Map.MapTerms (
  MapTerms' (..),
  MapTerms,
) where

import Data.Kind
import Data.Ord

import ArdanaDollar.Map.Types (Datum, MapInstance)

import Ledger.Typed.Scripts qualified as Scripts
import Plutus.V1.Ledger.Api qualified as Api

class MapTerms' t where
  type ValidatorTypes' t :: Type
  type K' t :: Type
  type V' t :: Type
  nodeValidPolicy' :: MapInstance -> Scripts.MintingPolicy
  inst' :: MapInstance -> Scripts.TypedValidator (ValidatorTypes' t)

class
  ( MapTerms' t
  , Scripts.ValidatorTypes (ValidatorTypes' t)
  , Ord (K' t)
  , Api.ToData (K' t)
  , Api.ToData (V' t)
  , Api.ToData
      (Scripts.DatumType (ValidatorTypes' t))
  , Api.ToData
      (Scripts.RedeemerType (ValidatorTypes' t))
  , Api.FromData
      (Scripts.DatumType (ValidatorTypes' t))
  , Scripts.DatumType (ValidatorTypes' t) ~ Datum (K' t) (V' t)
  ) =>
  MapTerms t