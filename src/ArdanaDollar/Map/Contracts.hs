{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module ArdanaDollar.Map.Contracts (
  create,
  createTest,
  insert,
  remove,
  use,
  mkMapLookup,
  MapLookup (..),
  address,
) where

import Prelude

import Ledger qualified
import Ledger.Constraints qualified as Constraints
import Ledger.Typed.Scripts qualified as Scripts

import Plutus.Contract (
  Contract,
  awaitTxConfirmed,
  logInfo,
  mapError,
  ownPubKey,
  submitTxConstraintsWith,
  tell,
  throwError,
  utxosAt,
 )
import Plutus.Contracts.Currency qualified as Currency
import Plutus.V1.Ledger.Api qualified as Ledger
import Plutus.V1.Ledger.Value qualified as Value
import PlutusTx.Prelude qualified

import Control.Lens ((^.))
import Control.Monad (join, void)
import Data.Kind (Type)
import Data.List (sortOn)
import Data.Map qualified as M
import Data.Maybe (fromJust, isJust, listToMaybe, maybeToList)
import Data.Monoid (Last (Last))
import Data.Row (Row)
import Data.Text (Text, pack)

import ArdanaDollar.Map.MapTerms
import ArdanaDollar.Map.Types
import ArdanaDollar.Map.Validators
import ArdanaDollar.Utils

type Tpl = (Ledger.TxOutRef, Ledger.ChainIndexTxOut)

data MapLookup k v = MapLookup
  { mapLookup'map :: (Tpl, Map)
  , mapLookup'nodes :: [(Tpl, Node k v)]
  }

nodeValidPolicySymbol :: forall (t :: Type). MapTerms t => MapInstance -> Value.CurrencySymbol
nodeValidPolicySymbol = Ledger.scriptCurrencySymbol . nodeValidPolicy' @t

validator :: forall (t :: Type). MapTerms t => MapInstance -> Ledger.Validator
validator mapInstance = Scripts.validatorScript $ inst' @t mapInstance

address :: forall (t :: Type). MapTerms t => MapInstance -> Ledger.Address
address mapInstance = Ledger.scriptAddress $ validator @t mapInstance

mkMapLookup' ::
  forall (t :: Type) (s :: Row Type) (w :: Type).
  MapTerms t =>
  MapInstance ->
  Contract w s Text (Maybe (MapLookup (K' t) (V' t)))
mkMapLookup' mapInstance = do
  utxos <- M.toList <$> utxosAt (address @t mapInstance)
  list <- join <$> sequence (own <$> utxos)
  let maps = list >>= (maybeToList . mapF)
  let nodes = list >>= (maybeToList . nodeF)
  let sortedNodes = sortOn (\(_, node) -> node'key node) nodes
  case maps of
    [head'] -> return $ Just $ MapLookup head' sortedNodes
    _ -> return Nothing
  where
    token = Ledger.scriptCurrencySymbol $ nodeValidPolicy' @t mapInstance

    own t@(_, chainIndexTxOut) =
      (\m -> maybeToList $ (t,) <$> m) <$> datumForOffchain @(Datum (K' t) (V' t)) chainIndexTxOut

    mapF (tpl, datum) = case datum of
      MapDatum m | hasOne' (unMapInstance mapInstance) (snd tpl ^. Ledger.ciTxOutValue) -> Just (tpl, m)
      _ -> Nothing

    nodeF (tpl, datum) = case datum of
      NodeDatum m | isJust (hasToken' token (snd tpl ^. Ledger.ciTxOutValue)) -> Just (tpl, m)
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

mintNFT ::
  forall (s :: Row Type) (w :: Type).
  Contract w s Text Ledger.AssetClass
mintNFT = do
  self <- Ledger.pubKeyHash <$> ownPubKey
  let nftTokenName = Value.TokenName PlutusTx.Prelude.emptyByteString
  x <-
    mapError
      (pack . show @Currency.CurrencyError)
      (Currency.mintContract self [(nftTokenName, 1)])
  return $ Value.assetClass (Currency.currencySymbol x) nftTokenName

create ::
  forall (t :: Type) (s :: Row Type).
  (MapTerms t) =>
  Contract (Last MapInstance) s Text ()
create = do
  nftAC <- mintNFT
  create' @t nftAC

createTest ::
  forall (t :: Type) (s :: Row Type).
  (MapTerms t) =>
  Value.AssetClass ->
  Contract (Last MapInstance) s Text ()
createTest ac = do
  create' @t ac

create' ::
  forall (t :: Type) (s :: Row Type).
  (MapTerms t) =>
  Value.AssetClass ->
  Contract (Last MapInstance) s Text ()
create' ac = do
  let nftValue = Value.assetClassValue ac 1
      mapInstance = MapInstance ac

      lookups =
        Constraints.typedValidatorLookups (inst' @t mapInstance)
          <> Constraints.otherScript (Scripts.validatorScript $ inst' @t mapInstance)
      tx = Constraints.mustPayToTheScript (MapDatum $ Map Nothing) nftValue

  ledgerTx <- submitTxConstraintsWith @(ValidatorTypes' t) lookups tx
  void $ awaitTxConfirmed $ Ledger.txId ledgerTx

  tell $ Last $ Just mapInstance

insert ::
  forall (t :: Type) (s :: Row Type) (w :: Type).
  MapTerms t =>
  MapInstance ->
  (K' t, V' t) ->
  Contract w s Text ()
insert mapInstance pair@(key, _) = do
  lkp <- mkMapLookup @t mapInstance
  let nodes = mapLookup'nodes lkp
      goesBeforeFirst = key < node'key (snd $ head nodes)
      goesAfterLast = key > node'key (snd $ last nodes)
  if
      | null nodes -> addToEmptyMap @t mapInstance (mapLookup'map lkp) pair
      | goesBeforeFirst -> addSmallest @t mapInstance (mapLookup'map lkp) (head nodes) pair
      | goesAfterLast -> addGreatest @t mapInstance (last nodes) pair
      | otherwise -> case findPlaceInTheMiddle @t lkp key of
        Just neighbours -> addInTheMiddle @t mapInstance neighbours pair
        Nothing -> throwError "Key already in the map"

remove ::
  forall (t :: Type) (s :: Row Type) (w :: Type).
  MapTerms t =>
  MapInstance ->
  K' t ->
  Contract w s Text ()
remove mapInstance key = do
  lkp <- mkMapLookup @t mapInstance
  let nodes = mapLookup'nodes lkp
      atFirstNode = key == node'key (snd $ head nodes)
      atLastNode = key == node'key (snd $ last nodes)
  if
      | null nodes -> throwError "Removing from empty map"
      | length nodes == 1 && atFirstNode -> removeFromOneElementMap @t mapInstance (mapLookup'map lkp) (head nodes)
      | length nodes > 1 && atFirstNode -> removeSmallest @t mapInstance (mapLookup'map lkp) (head nodes) (head $ tail nodes)
      | length nodes > 1 && atLastNode -> removeGreatest @t mapInstance (last $ init nodes) (last nodes)
      | otherwise -> case findKeyInTheMiddle @t lkp key of
        Just triple -> removeInTheMiddle @t mapInstance triple
        Nothing -> throwError "Key not in the map"

use ::
  forall (t :: Type) (s :: Row Type) (w :: Type).
  MapTerms t =>
  MapInstance ->
  K' t ->
  (V' t -> V' t) ->
  Contract w s Text ()
use mapInstance key update = do
  lkp <- mkMapLookup @t mapInstance
  case findKey @t lkp key of
    Just (tpl, node) ->
      do
        let toSpend = M.fromList [tpl]
            lookups = lookups' @t mapInstance toSpend
            updated = node {node'value = update (node'value node)}
            tx =
              Constraints.mustPayToTheScript (NodeDatum updated) (snd tpl ^. Ledger.ciTxOutValue)
                <> Constraints.mustSpendScriptOutput
                  (fst tpl)
                  (Ledger.Redeemer $ Ledger.toBuiltinData Use)

        logInfo @String $ "Map: use entry"

        ledgerTx <- submitTxConstraintsWith @(ValidatorTypes' t) lookups tx
        void $ awaitTxConfirmed $ Ledger.txId ledgerTx
    _ -> return ()

lookups' ::
  forall (t :: Type).
  MapTerms t =>
  MapInstance ->
  M.Map Ledger.TxOutRef Ledger.ChainIndexTxOut ->
  Constraints.ScriptLookups (ValidatorTypes' t)
lookups' mapInstance toSpend =
  Constraints.typedValidatorLookups (inst' @t mapInstance)
    <> Constraints.otherScript (validator @t mapInstance)
    <> Constraints.mintingPolicy (nodeValidPolicy' @t mapInstance)
    <> Constraints.unspentOutputs toSpend

removeFromOneElementMap ::
  forall (t :: Type) (s :: Row Type) (w :: Type).
  MapTerms t =>
  MapInstance ->
  (Tpl, Map) ->
  (Tpl, Node (K' t) (V' t)) ->
  Contract w s Text ()
removeFromOneElementMap mapInstance map' node' =
  do
    let tokenRedeemer = RemoveFromOneElementMap (fst $ fst node')

        tokenAC = unPointer $ fromJust $ map'head $ snd map'
        tokenValue = Value.assetClassValue tokenAC (-1)
        nftValue = Value.assetClassValue (unMapInstance mapInstance) 1

        toSpend = M.fromList [fst map', fst node']

        lookups = lookups' @t mapInstance toSpend
        tx =
          Constraints.mustMintValueWithRedeemer (Ledger.Redeemer $ Ledger.toBuiltinData @(TokenRedeemer (K' t)) tokenRedeemer) tokenValue
            <> Constraints.mustPayToTheScript (MapDatum $ Map Nothing) nftValue
            <> Constraints.mustSpendScriptOutput
              (fst $ fst map')
              (Ledger.Redeemer $ Ledger.toBuiltinData ListOp)
            <> Constraints.mustSpendScriptOutput
              (fst $ fst node')
              (Ledger.Redeemer $ Ledger.toBuiltinData ListOp)

    logInfo @String $ "Map: remove from one element map"

    ledgerTx <- submitTxConstraintsWith @(ValidatorTypes' t) lookups tx
    void $ awaitTxConfirmed $ Ledger.txId ledgerTx

removeSmallest ::
  forall (t :: Type) (s :: Row Type) (w :: Type).
  MapTerms t =>
  MapInstance ->
  (Tpl, Map) ->
  (Tpl, Node (K' t) (V' t)) ->
  (Tpl, Node (K' t) (V' t)) ->
  Contract w s Text ()
removeSmallest mapInstance map' node' next' =
  do
    let tokenRedeemer = RemoveSmallest (fst $ fst node') (fst $ fst next')

        tokenAC = unPointer $ fromJust $ map'head $ snd map'
        tokenValue = Value.assetClassValue tokenAC (-1)
        nftValue = Value.assetClassValue (unMapInstance mapInstance) 1

        toSpend = M.fromList [fst map', fst node', fst next']

        lookups = lookups' @t mapInstance toSpend
        tx =
          Constraints.mustMintValueWithRedeemer (Ledger.Redeemer $ Ledger.toBuiltinData @(TokenRedeemer (K' t)) tokenRedeemer) tokenValue
            <> Constraints.mustPayToTheScript (MapDatum $ Map (node'next $ snd node')) nftValue
            <> Constraints.mustPayToTheScript (NodeDatum $ snd next') (snd (fst next') ^. Ledger.ciTxOutValue)
            <> Constraints.mustSpendScriptOutput
              (fst $ fst map')
              (Ledger.Redeemer $ Ledger.toBuiltinData ListOp)
            <> Constraints.mustSpendScriptOutput
              (fst $ fst node')
              (Ledger.Redeemer $ Ledger.toBuiltinData ListOp)
            <> Constraints.mustSpendScriptOutput
              (fst $ fst next')
              (Ledger.Redeemer $ Ledger.toBuiltinData ListOp)

    logInfo @String $ "Map: remove smallest"

    ledgerTx <- submitTxConstraintsWith @(ValidatorTypes' t) lookups tx
    void $ awaitTxConfirmed $ Ledger.txId ledgerTx

removeGreatest ::
  forall (t :: Type) (s :: Row Type) (w :: Type).
  MapTerms t =>
  MapInstance ->
  (Tpl, Node (K' t) (V' t)) ->
  (Tpl, Node (K' t) (V' t)) ->
  Contract w s Text ()
removeGreatest mapInstance node' next' =
  do
    let tokenRedeemer = RemoveGreatest (fst $ fst node') (fst $ fst next')

        tokenAC = unPointer $ fromJust $ node'next $ snd node'
        tokenValue = Value.assetClassValue tokenAC (-1)

        toSpend = M.fromList [fst node', fst next']

        lookups = lookups' @t mapInstance toSpend
        tx =
          Constraints.mustMintValueWithRedeemer (Ledger.Redeemer $ Ledger.toBuiltinData @(TokenRedeemer (K' t)) tokenRedeemer) tokenValue
            <> Constraints.mustPayToTheScript (NodeDatum (snd node'){node'next = Nothing}) (snd (fst node') ^. Ledger.ciTxOutValue)
            <> Constraints.mustSpendScriptOutput
              (fst $ fst node')
              (Ledger.Redeemer $ Ledger.toBuiltinData ListOp)
            <> Constraints.mustSpendScriptOutput
              (fst $ fst next')
              (Ledger.Redeemer $ Ledger.toBuiltinData ListOp)

    logInfo @String $ "Map: remove greatest"

    ledgerTx <- submitTxConstraintsWith @(ValidatorTypes' t) lookups tx
    void $ awaitTxConfirmed $ Ledger.txId ledgerTx

removeInTheMiddle ::
  forall (t :: Type) (s :: Row Type) (w :: Type).
  MapTerms t =>
  MapInstance ->
  ((Tpl, Node (K' t) (V' t)), (Tpl, Node (K' t) (V' t)), (Tpl, Node (K' t) (V' t))) ->
  Contract w s Text ()
removeInTheMiddle mapInstance (prev, mid, next) =
  do
    let tokenRedeemer = RemoveInTheMiddle (fst $ fst prev) (fst $ fst mid) (fst $ fst next)

        tokenAC = unPointer $ fromJust $ node'next $ snd prev
        tokenValue = Value.assetClassValue tokenAC (-1)

        toSpend = M.fromList [fst prev, fst mid, fst next]

        lookups = lookups' @t mapInstance toSpend
        tx =
          Constraints.mustMintValueWithRedeemer (Ledger.Redeemer $ Ledger.toBuiltinData @(TokenRedeemer (K' t)) tokenRedeemer) tokenValue
            <> Constraints.mustPayToTheScript (NodeDatum (snd prev){node'next = node'next (snd mid)}) (snd (fst prev) ^. Ledger.ciTxOutValue)
            <> Constraints.mustPayToTheScript (NodeDatum (snd next)) (snd (fst next) ^. Ledger.ciTxOutValue)
            <> Constraints.mustSpendScriptOutput
              (fst $ fst prev)
              (Ledger.Redeemer $ Ledger.toBuiltinData ListOp)
            <> Constraints.mustSpendScriptOutput
              (fst $ fst mid)
              (Ledger.Redeemer $ Ledger.toBuiltinData ListOp)
            <> Constraints.mustSpendScriptOutput
              (fst $ fst next)
              (Ledger.Redeemer $ Ledger.toBuiltinData ListOp)

    logInfo @String $ "Map: remove in the middle"

    ledgerTx <- submitTxConstraintsWith @(ValidatorTypes' t) lookups tx
    void $ awaitTxConfirmed $ Ledger.txId ledgerTx

addToEmptyMap ::
  forall (t :: Type) (s :: Row Type) (w :: Type).
  MapTerms t =>
  MapInstance ->
  (Tpl, Map) ->
  (K' t, V' t) ->
  Contract w s Text ()
addToEmptyMap mapInstance map' (key, value) =
  do
    let tokenRedeemer = AddToEmptyMap key

        tokenAC = Value.assetClass (nodeValidPolicySymbol @t mapInstance) (tokenName (fst $ fst map'))
        tokenValue = Value.assetClassValue tokenAC 1
        nftValue = Value.assetClassValue (unMapInstance mapInstance) 1

        toSpend = M.fromList [fst map']

        lookups = lookups' @t mapInstance toSpend
        tx =
          Constraints.mustMintValueWithRedeemer (Ledger.Redeemer $ Ledger.toBuiltinData @(TokenRedeemer (K' t)) tokenRedeemer) tokenValue
            <> Constraints.mustPayToTheScript (MapDatum $ Map $ Just $ Pointer tokenAC) nftValue
            <> Constraints.mustPayToTheScript (NodeDatum $ Node key value Nothing) tokenValue
            <> Constraints.mustSpendScriptOutput
              (fst $ fst map')
              (Ledger.Redeemer $ Ledger.toBuiltinData ListOp)

    logInfo @String $ "Map: add to empty map"

    ledgerTx <- submitTxConstraintsWith @(ValidatorTypes' t) lookups tx
    void $ awaitTxConfirmed $ Ledger.txId ledgerTx

addSmallest ::
  forall (t :: Type) (s :: Row Type) (w :: Type).
  MapTerms t =>
  MapInstance ->
  (Tpl, Map) ->
  (Tpl, Node (K' t) (V' t)) ->
  (K' t, V' t) ->
  Contract w s Text ()
addSmallest mapInstance map' node' (key, value) =
  do
    let tokenRedeemer = AddSmallest key (fst $ fst node')

        tokenAC = Value.assetClass (nodeValidPolicySymbol @t mapInstance) (tokenName (fst $ fst map'))
        tokenValue = Value.assetClassValue tokenAC 1
        nftValue = Value.assetClassValue (unMapInstance mapInstance) 1

        nodePointer = map'head $ snd map'

        toSpend = M.fromList [fst map', fst node']

        lookups = lookups' @t mapInstance toSpend
        tx =
          Constraints.mustMintValueWithRedeemer (Ledger.Redeemer $ Ledger.toBuiltinData @(TokenRedeemer (K' t)) tokenRedeemer) tokenValue
            <> Constraints.mustPayToTheScript (MapDatum $ Map $ Just $ Pointer tokenAC) nftValue
            <> Constraints.mustPayToTheScript (NodeDatum $ Node key value nodePointer) tokenValue
            <> Constraints.mustPayToTheScript (NodeDatum $ Node (node'key $ snd node') (node'value $ snd node') Nothing) (snd (fst node') ^. Ledger.ciTxOutValue)
            <> Constraints.mustSpendScriptOutput
              (fst $ fst map')
              (Ledger.Redeemer $ Ledger.toBuiltinData ListOp)
            <> Constraints.mustSpendScriptOutput
              (fst $ fst node')
              (Ledger.Redeemer $ Ledger.toBuiltinData ListOp)

    logInfo @String $ "Map: add smallest"

    ledgerTx <- submitTxConstraintsWith @(ValidatorTypes' t) lookups tx
    void $ awaitTxConfirmed $ Ledger.txId ledgerTx

addGreatest ::
  forall (t :: Type) (s :: Row Type) (w :: Type).
  MapTerms t =>
  MapInstance ->
  (Tpl, Node (K' t) (V' t)) ->
  (K' t, V' t) ->
  Contract w s Text ()
addGreatest mapInstance node' (key, value) =
  do
    let tokenRedeemer = AddGreatest key (fst $ fst node')

        tokenAC = Value.assetClass (nodeValidPolicySymbol @t mapInstance) (tokenName (fst $ fst node'))
        tokenValue = Value.assetClassValue tokenAC 1

        toSpend = M.fromList [fst node']

        lookups = lookups' @t mapInstance toSpend
        tx =
          Constraints.mustMintValueWithRedeemer (Ledger.Redeemer $ Ledger.toBuiltinData @(TokenRedeemer (K' t)) tokenRedeemer) tokenValue
            <> Constraints.mustPayToTheScript (NodeDatum $ Node key value Nothing) tokenValue
            <> Constraints.mustPayToTheScript (NodeDatum $ Node (node'key $ snd node') (node'value $ snd node') (Just $ Pointer tokenAC)) (snd (fst node') ^. Ledger.ciTxOutValue)
            <> Constraints.mustSpendScriptOutput
              (fst $ fst node')
              (Ledger.Redeemer $ Ledger.toBuiltinData ListOp)

    logInfo @String $ "Map: add greatest"

    ledgerTx <- submitTxConstraintsWith @(ValidatorTypes' t) lookups tx
    void $ awaitTxConfirmed $ Ledger.txId ledgerTx

addInTheMiddle ::
  forall (t :: Type) (s :: Row Type) (w :: Type).
  MapTerms t =>
  MapInstance ->
  ((Tpl, Node (K' t) (V' t)), (Tpl, Node (K' t) (V' t))) ->
  (K' t, V' t) ->
  Contract w s Text ()
addInTheMiddle mapInstance (before, after) (key, value) =
  do
    let tokenRedeemer = AddInTheMiddle key (fst $ fst before) (fst $ fst after)

        tokenAC = Value.assetClass (nodeValidPolicySymbol @t mapInstance) (tokenName (fst $ fst before))
        tokenValue = Value.assetClassValue tokenAC 1

        toSpend = M.fromList [fst before, fst after]

        lookups = lookups' @t mapInstance toSpend
        tx =
          Constraints.mustMintValueWithRedeemer (Ledger.Redeemer $ Ledger.toBuiltinData @(TokenRedeemer (K' t)) tokenRedeemer) tokenValue
            <> Constraints.mustPayToTheScript (NodeDatum $ Node key value (node'next $ snd before)) tokenValue
            <> Constraints.mustPayToTheScript (NodeDatum $ Node (node'key $ snd before) (node'value $ snd before) (Just $ Pointer tokenAC)) (snd (fst before) ^. Ledger.ciTxOutValue)
            <> Constraints.mustPayToTheScript (NodeDatum $ Node (node'key $ snd after) (node'value $ snd after) (node'next $ snd after)) (snd (fst after) ^. Ledger.ciTxOutValue)
            <> Constraints.mustSpendScriptOutput
              (fst $ fst before)
              (Ledger.Redeemer $ Ledger.toBuiltinData ListOp)
            <> Constraints.mustSpendScriptOutput
              (fst $ fst after)
              (Ledger.Redeemer $ Ledger.toBuiltinData ListOp)

    logInfo @String $ "Map: add in the middle"

    ledgerTx <- submitTxConstraintsWith @(ValidatorTypes' t) lookups tx
    void $ awaitTxConfirmed $ Ledger.txId ledgerTx

findPlaceInTheMiddle ::
  forall t.
  MapTerms t =>
  MapLookup (K' t) (V' t) ->
  K' t ->
  Maybe ((Tpl, Node (K' t) (V' t)), (Tpl, Node (K' t) (V' t)))
findPlaceInTheMiddle lkp key =
  let nodes = mapLookup'nodes lkp
      zipped = zip nodes (tail nodes)
      places = (\((_, node1), (_, node2)) -> node'key node1 < key && key < node'key node2) `filter` zipped
   in case nodes of
        (_ : _) -> listToMaybe places
        _ -> Nothing

findKeyInTheMiddle ::
  forall t.
  MapTerms t =>
  MapLookup (K' t) (V' t) ->
  K' t ->
  Maybe ((Tpl, Node (K' t) (V' t)), (Tpl, Node (K' t) (V' t)), (Tpl, Node (K' t) (V' t)))
findKeyInTheMiddle lkp key =
  let nodes = mapLookup'nodes lkp
      zipped = zip3 nodes (tail nodes) (tail (tail nodes))
      places = (\((_, _), (_, node), (_, _)) -> node'key node == key) `filter` zipped
   in case nodes of
        (_ : _ : _) -> listToMaybe places
        _ -> Nothing

findKey ::
  forall t.
  MapTerms t =>
  MapLookup (K' t) (V' t) ->
  K' t ->
  Maybe (Tpl, Node (K' t) (V' t))
findKey lkp key =
  let nodes = mapLookup'nodes lkp
      places = (\(_, node) -> node'key node == key) `filter` nodes
   in if not (null nodes) && not (null places)
        then return $ head places
        else Nothing
