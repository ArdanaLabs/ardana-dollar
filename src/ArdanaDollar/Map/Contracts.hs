{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module ArdanaDollar.Map.Contracts (
  create,
  createTest,
  insert,
  remove,
  mkMapLookup,
  MapLookup (..),
) where

import Prelude

import Ledger qualified
import Ledger.Constraints qualified as Constraints
import Plutus.Contract
import Plutus.Contracts.Currency qualified as Currency
import Plutus.V1.Ledger.Api qualified as Api
import Plutus.V1.Ledger.Value qualified as Value
import PlutusTx.Prelude qualified

import Control.Lens ((^.))
import Control.Monad
import Data.Kind
import Data.List (sortOn)
import Data.Map qualified as Map
import Data.Maybe
import Data.Monoid (Last (Last))
import Data.Row
import Data.Text (Text, pack)

import ArdanaDollar.Map.Types
import ArdanaDollar.Map.Validators
import ArdanaDollar.Map.ValidatorsTH
import ArdanaDollar.Utils

type Tpl = (Ledger.TxOutRef, Ledger.ChainIndexTxOut)

data MapLookup = MapLookup
  { mapLookup'map :: (Tpl, Map)
  , mapLookup'nodes :: [(Tpl, Node)]
  }

mkMapLookup :: forall (s :: Row Type) (w :: Type). MapInstance -> Contract w s Text (Maybe MapLookup)
mkMapLookup mapInstance = do
  utxos <- Map.toList <$> utxosAt (address mapInstance)
  list <- join <$> sequence (own <$> utxos)
  let maps = list >>= (maybeToList . mapF)
  let nodes = list >>= (maybeToList . nodeF)
  let sortedNodes = sortOn (\(_, node) -> node'key node) nodes
  if length maps == 1
    then return $ Just $ MapLookup (head maps) sortedNodes
    else return Nothing
  where
    token = nodeValidPolicySymbol mapInstance

    own t@(_, chainIndexTxOut) =
      (\m -> maybeToList $ (t,) <$> m) <$> datumForOffchain @Datum chainIndexTxOut

    mapF (tpl, datum) = case datum of
      MapDatum m | hasOne' (unMapInstance mapInstance) (snd tpl ^. Ledger.ciTxOutValue) -> Just (tpl, m)
      _ -> Nothing

    nodeF (tpl, datum) = case datum of
      NodeDatum m | isJust (hasToken' token (snd tpl ^. Ledger.ciTxOutValue)) -> Just (tpl, m)
      _ -> Nothing

mintNFT :: forall (s :: Row Type) (w :: Type). Contract w s Text Ledger.AssetClass
mintNFT = do
  self <- Ledger.pubKeyHash <$> ownPubKey
  let nftTokenName = Value.TokenName PlutusTx.Prelude.emptyByteString
  x <-
    mapError
      (pack . show @Currency.CurrencyError)
      (Currency.mintContract self [(nftTokenName, 1)])
  return $ Value.assetClass (Currency.currencySymbol x) nftTokenName

create :: forall (s :: Row Type). Contract (Last MapInstance) s Text ()
create = do
  nftAC <- mintNFT
  _create nftAC

createTest :: forall (s :: Row Type). Value.AssetClass -> Contract (Last MapInstance) s Text ()
createTest ac = do
  _create ac

_create :: forall (s :: Row Type). Value.AssetClass -> Contract (Last MapInstance) s Text ()
_create ac = do
  let nftValue = Value.assetClassValue ac 1
      mapInstance = MapInstance ac

      lookups =
        Constraints.typedValidatorLookups (inst mapInstance)
          <> Constraints.otherScript (validator mapInstance)
      tx = Constraints.mustPayToTheScript (MapDatum $ Map Nothing) nftValue

  ledgerTx <- submitTxConstraintsWith @ValidatorTypes lookups tx
  void $ awaitTxConfirmed $ Ledger.txId ledgerTx

  tell $ Last $ Just mapInstance

insert :: forall (s :: Row Type) (w :: Type). MapInstance -> (Key, Value) -> Contract w s Text ()
insert mapInstance (key, value) = do
  lkp <- fromJust <$> mkMapLookup mapInstance
  let placeInTheMiddle = findPlaceInTheMiddle lkp key
  if
      | null (mapLookup'nodes lkp) -> addToEmptyMap mapInstance lkp (key, value)
      | key < node'key (snd $ head $ mapLookup'nodes lkp) -> addSmallest mapInstance lkp (key, value)
      | key > node'key (snd $ last $ mapLookup'nodes lkp) -> addGreatest mapInstance lkp (key, value)
      | isJust placeInTheMiddle -> addInTheMiddle mapInstance (fromJust placeInTheMiddle) (key, value)
      | otherwise -> throwError "Key already in the map"

remove :: forall (s :: Row Type) (w :: Type). MapInstance -> Key -> Contract w s Text ()
remove mapInstance key = do
  lkp <- fromJust <$> mkMapLookup mapInstance
  let atFirstNode = key == node'key (snd $ head $ mapLookup'nodes lkp)
  let atLastNode = key == node'key (snd $ last $ mapLookup'nodes lkp)
  let triple = findKey lkp key
  if
      | length (mapLookup'nodes lkp) == 1 && atFirstNode -> removeFromOneElementMap mapInstance lkp
      | atFirstNode -> removeSmallest mapInstance lkp
      | atLastNode -> removeGreatest mapInstance lkp
      | length (mapLookup'nodes lkp) >= 3 && isJust triple -> removeInTheMiddle mapInstance (fromJust triple)
      | otherwise -> throwError "Key not in the map"

lookups' :: MapInstance -> Map.Map Api.TxOutRef Ledger.ChainIndexTxOut -> Constraints.ScriptLookups ValidatorTypes
lookups' mapInstance toSpend =
  Constraints.typedValidatorLookups (inst mapInstance)
    <> Constraints.otherScript (validator mapInstance)
    <> Constraints.mintingPolicy (nodeValidPolicy mapInstance)
    <> Constraints.unspentOutputs toSpend

removeFromOneElementMap :: forall (s :: Row Type) (w :: Type). MapInstance -> MapLookup -> Contract w s Text ()
removeFromOneElementMap mapInstance lkp =
  do
    let _map = mapLookup'map lkp
        _node = head $ mapLookup'nodes lkp
        tokenRedeemer = RemoveFromOneElementMap (fst $ fst _node)

        tokenAC = unPointer $ fromJust $ map'head $ snd _map
        tokenValue = Value.assetClassValue tokenAC (-1)
        nftValue = Value.assetClassValue (unMapInstance mapInstance) 1

        toSpend = Map.fromList [fst _map, fst _node]

        lookups = lookups' mapInstance toSpend
        tx =
          Constraints.mustMintValueWithRedeemer (Ledger.Redeemer $ Api.toBuiltinData tokenRedeemer) tokenValue
            <> Constraints.mustPayToTheScript (MapDatum $ Map Nothing) nftValue
            <> Constraints.mustSpendScriptOutput
              (fst $ fst _map)
              (Ledger.Redeemer $ Api.toBuiltinData ListOp)
            <> Constraints.mustSpendScriptOutput
              (fst $ fst _node)
              (Ledger.Redeemer $ Api.toBuiltinData ListOp)

    logInfo @String $ "Map: remove from one element map"

    ledgerTx <- submitTxConstraintsWith @ValidatorTypes lookups tx
    void $ awaitTxConfirmed $ Ledger.txId ledgerTx

removeSmallest :: forall (s :: Row Type) (w :: Type). MapInstance -> MapLookup -> Contract w s Text ()
removeSmallest mapInstance lkp =
  do
    let _map = mapLookup'map lkp
        _node = head $ mapLookup'nodes lkp
        _next = head $ tail $ mapLookup'nodes lkp
        tokenRedeemer = RemoveSmallest (fst $ fst _node) (fst $ fst _next)

        tokenAC = unPointer $ fromJust $ map'head $ snd _map
        tokenValue = Value.assetClassValue tokenAC (-1)
        nftValue = Value.assetClassValue (unMapInstance mapInstance) 1

        toSpend = Map.fromList [fst _map, fst _node, fst _next]

        lookups = lookups' mapInstance toSpend
        tx =
          Constraints.mustMintValueWithRedeemer (Ledger.Redeemer $ Api.toBuiltinData tokenRedeemer) tokenValue
            <> Constraints.mustPayToTheScript (MapDatum $ Map (node'next $ snd _node)) nftValue
            <> Constraints.mustPayToTheScript (NodeDatum $ snd _next) (snd (fst _next) ^. Ledger.ciTxOutValue)
            <> Constraints.mustSpendScriptOutput
              (fst $ fst _map)
              (Ledger.Redeemer $ Api.toBuiltinData ListOp)
            <> Constraints.mustSpendScriptOutput
              (fst $ fst _node)
              (Ledger.Redeemer $ Api.toBuiltinData ListOp)
            <> Constraints.mustSpendScriptOutput
              (fst $ fst _next)
              (Ledger.Redeemer $ Api.toBuiltinData ListOp)

    logInfo @String $ "Map: remove smallest"

    ledgerTx <- submitTxConstraintsWith @ValidatorTypes lookups tx
    void $ awaitTxConfirmed $ Ledger.txId ledgerTx

removeGreatest :: forall (s :: Row Type) (w :: Type). MapInstance -> MapLookup -> Contract w s Text ()
removeGreatest mapInstance lkp =
  do
    let _node = last $ init $ mapLookup'nodes lkp
        _next = last $ mapLookup'nodes lkp
        tokenRedeemer = RemoveGreatest (fst $ fst _node) (fst $ fst _next)

        tokenAC = unPointer $ fromJust $ node'next $ snd _node
        tokenValue = Value.assetClassValue tokenAC (-1)

        toSpend = Map.fromList [fst _node, fst _next]

        lookups = lookups' mapInstance toSpend
        tx =
          Constraints.mustMintValueWithRedeemer (Ledger.Redeemer $ Api.toBuiltinData tokenRedeemer) tokenValue
            <> Constraints.mustPayToTheScript (NodeDatum (snd _node){node'next = Nothing}) (snd (fst _node) ^. Ledger.ciTxOutValue)
            <> Constraints.mustSpendScriptOutput
              (fst $ fst _node)
              (Ledger.Redeemer $ Api.toBuiltinData ListOp)
            <> Constraints.mustSpendScriptOutput
              (fst $ fst _next)
              (Ledger.Redeemer $ Api.toBuiltinData ListOp)

    logInfo @String $ "Map: remove greatest"

    ledgerTx <- submitTxConstraintsWith @ValidatorTypes lookups tx
    void $ awaitTxConfirmed $ Ledger.txId ledgerTx

removeInTheMiddle :: forall (s :: Row Type) (w :: Type). MapInstance -> ((Tpl, Node), (Tpl, Node), (Tpl, Node)) -> Contract w s Text ()
removeInTheMiddle mapInstance (prev, mid, next) =
  do
    let tokenRedeemer = RemoveInTheMiddle (fst $ fst prev) (fst $ fst mid) (fst $ fst next)

        tokenAC = unPointer $ fromJust $ node'next $ snd prev
        tokenValue = Value.assetClassValue tokenAC (-1)

        toSpend = Map.fromList [fst prev, fst mid, fst next]

        lookups = lookups' mapInstance toSpend
        tx =
          Constraints.mustMintValueWithRedeemer (Ledger.Redeemer $ Api.toBuiltinData tokenRedeemer) tokenValue
            <> Constraints.mustPayToTheScript (NodeDatum (snd prev){node'next = node'next (snd mid)}) (snd (fst prev) ^. Ledger.ciTxOutValue)
            <> Constraints.mustPayToTheScript (NodeDatum (snd next)) (snd (fst next) ^. Ledger.ciTxOutValue)
            <> Constraints.mustSpendScriptOutput
              (fst $ fst prev)
              (Ledger.Redeemer $ Api.toBuiltinData ListOp)
            <> Constraints.mustSpendScriptOutput
              (fst $ fst mid)
              (Ledger.Redeemer $ Api.toBuiltinData ListOp)
            <> Constraints.mustSpendScriptOutput
              (fst $ fst next)
              (Ledger.Redeemer $ Api.toBuiltinData ListOp)

    logInfo @String $ "Map: remove in the middle"

    ledgerTx <- submitTxConstraintsWith @ValidatorTypes lookups tx
    void $ awaitTxConfirmed $ Ledger.txId ledgerTx

addToEmptyMap :: forall (s :: Row Type) (w :: Type). MapInstance -> MapLookup -> (Key, Value) -> Contract w s Text ()
addToEmptyMap mapInstance lkp (key, value) =
  do
    let tokenRedeemer = AddToEmptyMap key
        _map = mapLookup'map lkp

        tokenAC = Value.assetClass (nodeValidPolicySymbol mapInstance) (tokenName (fst $ fst _map))
        tokenValue = Value.assetClassValue tokenAC 1
        nftValue = Value.assetClassValue (unMapInstance mapInstance) 1

        toSpend = Map.fromList [fst _map]

        lookups = lookups' mapInstance toSpend
        tx =
          Constraints.mustMintValueWithRedeemer (Ledger.Redeemer $ Api.toBuiltinData tokenRedeemer) tokenValue
            <> Constraints.mustPayToTheScript (MapDatum $ Map $ Just $ Pointer tokenAC) nftValue
            <> Constraints.mustPayToTheScript (NodeDatum $ Node key value Nothing) tokenValue
            <> Constraints.mustSpendScriptOutput
              (fst $ fst _map)
              (Ledger.Redeemer $ Api.toBuiltinData ListOp)

    logInfo @String $ "Map: add to empty map"

    ledgerTx <- submitTxConstraintsWith @ValidatorTypes lookups tx
    void $ awaitTxConfirmed $ Ledger.txId ledgerTx

addSmallest :: forall (s :: Row Type) (w :: Type). MapInstance -> MapLookup -> (Key, Value) -> Contract w s Text ()
addSmallest mapInstance lkp (key, value) =
  do
    let _map = mapLookup'map lkp
        _node = head $ mapLookup'nodes lkp
        tokenRedeemer = AddSmallest key (fst $ fst _node)

        tokenAC = Value.assetClass (nodeValidPolicySymbol mapInstance) (tokenName (fst $ fst _map))
        tokenValue = Value.assetClassValue tokenAC 1
        nftValue = Value.assetClassValue (unMapInstance mapInstance) 1

        nodePointer = map'head $ snd _map

        toSpend = Map.fromList [fst _map, fst _node]

        lookups = lookups' mapInstance toSpend
        tx =
          Constraints.mustMintValueWithRedeemer (Ledger.Redeemer $ Api.toBuiltinData tokenRedeemer) tokenValue
            <> Constraints.mustPayToTheScript (MapDatum $ Map $ Just $ Pointer tokenAC) nftValue
            <> Constraints.mustPayToTheScript (NodeDatum $ Node key value nodePointer) tokenValue
            <> Constraints.mustPayToTheScript (NodeDatum $ Node (node'key $ snd _node) (node'value $ snd _node) Nothing) (snd (fst _node) ^. Ledger.ciTxOutValue)
            <> Constraints.mustSpendScriptOutput
              (fst $ fst _map)
              (Ledger.Redeemer $ Api.toBuiltinData ListOp)
            <> Constraints.mustSpendScriptOutput
              (fst $ fst _node)
              (Ledger.Redeemer $ Api.toBuiltinData ListOp)

    logInfo @String $ "Map: add smallest"

    ledgerTx <- submitTxConstraintsWith @ValidatorTypes lookups tx
    void $ awaitTxConfirmed $ Ledger.txId ledgerTx

addGreatest :: forall (s :: Row Type) (w :: Type). MapInstance -> MapLookup -> (Key, Value) -> Contract w s Text ()
addGreatest mapInstance lkp (key, value) =
  do
    let _node = last $ mapLookup'nodes lkp
        tokenRedeemer = AddGreatest key (fst $ fst _node)

        tokenAC = Value.assetClass (nodeValidPolicySymbol mapInstance) (tokenName (fst $ fst _node))
        tokenValue = Value.assetClassValue tokenAC 1

        toSpend = Map.fromList [fst _node]

        lookups = lookups' mapInstance toSpend
        tx =
          Constraints.mustMintValueWithRedeemer (Ledger.Redeemer $ Api.toBuiltinData tokenRedeemer) tokenValue
            <> Constraints.mustPayToTheScript (NodeDatum $ Node key value Nothing) tokenValue
            <> Constraints.mustPayToTheScript (NodeDatum $ Node (node'key $ snd _node) (node'value $ snd _node) (Just $ Pointer tokenAC)) (snd (fst _node) ^. Ledger.ciTxOutValue)
            <> Constraints.mustSpendScriptOutput
              (fst $ fst _node)
              (Ledger.Redeemer $ Api.toBuiltinData ListOp)

    logInfo @String $ "Map: add greatest"

    ledgerTx <- submitTxConstraintsWith @ValidatorTypes lookups tx
    void $ awaitTxConfirmed $ Ledger.txId ledgerTx

addInTheMiddle :: forall (s :: Row Type) (w :: Type). MapInstance -> ((Tpl, Node), (Tpl, Node)) -> (Key, Value) -> Contract w s Text ()
addInTheMiddle mapInstance (before, after) (key, value) =
  do
    let tokenRedeemer = AddInTheMiddle key (fst $ fst before) (fst $ fst after)

        tokenAC = Value.assetClass (nodeValidPolicySymbol mapInstance) (tokenName (fst $ fst before))
        tokenValue = Value.assetClassValue tokenAC 1

        toSpend = Map.fromList [fst before, fst after]

        lookups = lookups' mapInstance toSpend
        tx =
          Constraints.mustMintValueWithRedeemer (Ledger.Redeemer $ Api.toBuiltinData tokenRedeemer) tokenValue
            <> Constraints.mustPayToTheScript (NodeDatum $ Node key value (node'next $ snd before)) tokenValue
            <> Constraints.mustPayToTheScript (NodeDatum $ Node (node'key $ snd before) (node'value $ snd before) (Just $ Pointer tokenAC)) (snd (fst before) ^. Ledger.ciTxOutValue)
            <> Constraints.mustPayToTheScript (NodeDatum $ Node (node'key $ snd after) (node'value $ snd after) (node'next $ snd after)) (snd (fst after) ^. Ledger.ciTxOutValue)
            <> Constraints.mustSpendScriptOutput
              (fst $ fst before)
              (Ledger.Redeemer $ Api.toBuiltinData ListOp)
            <> Constraints.mustSpendScriptOutput
              (fst $ fst after)
              (Ledger.Redeemer $ Api.toBuiltinData ListOp)

    logInfo @String $ "Map: add in the middle"

    ledgerTx <- submitTxConstraintsWith @ValidatorTypes lookups tx
    void $ awaitTxConfirmed $ Ledger.txId ledgerTx

findPlaceInTheMiddle :: MapLookup -> Key -> Maybe ((Tpl, Node), (Tpl, Node))
findPlaceInTheMiddle lkp key =
  let nodes = mapLookup'nodes lkp
      zipped = zip nodes (tail nodes)
      places = (\((_, node1), (_, node2)) -> node'key node1 < key && key < node'key node2) `filter` zipped
   in if not (null nodes) && not (null places)
        then return $ head places
        else Nothing

findKey :: MapLookup -> Key -> Maybe ((Tpl, Node), (Tpl, Node), (Tpl, Node))
findKey lkp key =
  let nodes = mapLookup'nodes lkp
      zipped = zip3 nodes (tail nodes) (tail (tail nodes))
      places = (\((_, _), (_, node), (_, _)) -> node'key node == key) `filter` zipped
   in if not (null nodes) && not (null places)
        then return $ head places
        else Nothing
