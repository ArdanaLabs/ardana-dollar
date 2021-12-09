{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module ArdanaDollar.Map.ContractUtils (
  TxOutEntry,
  address,
  MapLookup (..),
  mkMapLookup,
  findKey,
  lookups',
  fromJust',
) where

import Prelude

import Ledger qualified
import Ledger.Constraints qualified as Constraints

import Plutus.Contract (
  Contract,
  throwError,
  utxosAt,
 )

import Control.Lens ((^.))
import Data.Kind (Type)
import Data.List (sortOn)
import Data.Map qualified as M
import Data.Maybe (isJust, listToMaybe, mapMaybe, maybeToList)
import Data.Row (Row)
import Data.Text (Text)

import ArdanaDollar.Map.MapTerms (
  MapTerms,
  MapTerms' (K', V', inst', nodeValidPolicy'),
  ValidatorTypes',
 )
import ArdanaDollar.Map.Types (
  Datum (MapDatum, NodeDatum),
  Map,
  MapInstance,
  Node,
 )
import ArdanaDollar.Map.Types qualified as T
import ArdanaDollar.Map.Validator qualified as V
import ArdanaDollar.Utils qualified as Utils
import Ledger.Typed.Scripts qualified as Scripts

validator :: forall (t :: Type). MapTerms t => MapInstance -> Ledger.Validator
validator mapInstance = Scripts.validatorScript $ inst' @t mapInstance

address :: forall (t :: Type). MapTerms t => MapInstance -> Ledger.Address
address mapInstance = Ledger.scriptAddress $ validator @t mapInstance

type TxOutEntry = (Ledger.TxOutRef, Ledger.ChainIndexTxOut)

data MapLookup k v = MapLookup
  { mapLookup'map :: (TxOutEntry, Map)
  , mapLookup'nodes :: [(TxOutEntry, Node k v)]
  }

mkMapLookup' ::
  forall (t :: Type) (s :: Row Type) (w :: Type).
  MapTerms t =>
  MapInstance ->
  Contract w s Text (Maybe (MapLookup (K' t) (V' t)))
mkMapLookup' mapInstance = do
  utxos <- M.toList <$> utxosAt (address @t mapInstance)
  list <- concat <$> traverse own utxos
  let maps = mapMaybe mapF list
  let nodes = mapMaybe nodeF list
  let sortedNodes = sortOn (\(_, node) -> T.node'key node) nodes
  case maps of
    [head'] -> return $ Just $ MapLookup head' sortedNodes
    _ -> return Nothing
  where
    token :: Ledger.CurrencySymbol
    token = Ledger.scriptCurrencySymbol $ nodeValidPolicy' @t mapInstance

    own :: TxOutEntry -> Contract w s Text [(TxOutEntry, Datum (K' t) (V' t))]
    own t@(_, chainIndexTxOut) =
      (\m -> maybeToList $ (t,) <$> m) <$> Utils.datumForOffchain chainIndexTxOut

    mapF :: (TxOutEntry, Datum k v) -> Maybe (TxOutEntry, Map)
    mapF (tpl, datum) = case datum of
      MapDatum m | V.hasOne' (T.unMapInstance mapInstance) (snd tpl ^. Ledger.ciTxOutValue) -> Just (tpl, m)
      _ -> Nothing

    nodeF :: (TxOutEntry, Datum k v) -> Maybe (TxOutEntry, Node k v)
    nodeF (tpl, datum) = case datum of
      NodeDatum m | isJust (V.lookupToken' token (snd tpl ^. Ledger.ciTxOutValue)) -> Just (tpl, m)
      _ -> Nothing

mkMapLookup ::
  forall (t :: Type) (s :: Row Type) (w :: Type).
  MapTerms t =>
  MapInstance ->
  Contract w s Text (MapLookup (K' t) (V' t))
mkMapLookup mapInstance = do
  lkp <- mkMapLookup' @t mapInstance
  case lkp of
    Just r -> return r
    Nothing -> throwError "Cannot find map instance"

findKey ::
  forall t.
  MapTerms t =>
  MapLookup (K' t) (V' t) ->
  K' t ->
  Maybe (TxOutEntry, Node (K' t) (V' t))
findKey lkp key =
  let nodes = mapLookup'nodes lkp
      places = (\(_, node) -> T.node'key node == key) `filter` nodes
   in listToMaybe places

lookups' ::
  forall (t :: Type).
  MapTerms t =>
  MapInstance ->
  M.Map Ledger.TxOutRef Ledger.ChainIndexTxOut ->
  Constraints.ScriptLookups (ValidatorTypes' t)
lookups' mapInstance toSpend =
  Constraints.typedValidatorLookups (inst' @t mapInstance)
    <> Constraints.otherScript (validator @t mapInstance)
    <> Constraints.unspentOutputs toSpend

fromJust' :: forall (a :: Type) (s :: Row Type) (w :: Type). Text -> Maybe a -> Contract w s Text a
fromJust' message maybe' = case maybe' of
  Just v -> return v
  _ -> throwError message
