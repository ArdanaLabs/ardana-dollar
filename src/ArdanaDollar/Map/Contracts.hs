{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module ArdanaDollar.Map.Contracts (
  create,
  createTest,
  insert,
  remove,
  use,
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
import Data.Maybe (isJust, listToMaybe, maybeToList)
import Data.Monoid (Last (Last))
import Data.Row (Row)
import Data.Text (Text, pack)

import ArdanaDollar.Map.MapTerms (
  MapTerms,
  MapTerms' (K', V', ValidatorTypes', inst', nodeValidPolicy'),
 )
import ArdanaDollar.Map.Types (
  Datum (MapDatum, NodeDatum),
  Map (Map),
  MapInstance (MapInstance),
  Node (Node),
  NodeValidTokenRedeemer (..),
  Pointer (Pointer),
  Redeemer (ListOp, Use),
  SnapshotVersion (SnapshotVersion),
 )
import ArdanaDollar.Map.Types qualified as T
import ArdanaDollar.Map.Validator qualified as V
import ArdanaDollar.Utils qualified as Utils

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
  let sortedNodes = sortOn (\(_, node) -> T.node'key node) nodes
  case maps of
    [head'] -> return $ Just $ MapLookup head' sortedNodes
    _ -> return Nothing
  where
    token :: Ledger.CurrencySymbol
    token = Ledger.scriptCurrencySymbol $ nodeValidPolicy' @t mapInstance

    own :: Tpl -> Contract w s Text [(Tpl, Datum (K' t) (V' t))]
    own t@(_, chainIndexTxOut) =
      (\m -> maybeToList $ (t,) <$> m) <$> Utils.datumForOffchain chainIndexTxOut

    mapF :: (Tpl, Datum k v) -> Maybe (Tpl, Map)
    mapF (tpl, datum) = case datum of
      MapDatum m | V.hasOne' (T.unMapInstance mapInstance) (snd tpl ^. Ledger.ciTxOutValue) -> Just (tpl, m)
      _ -> Nothing

    nodeF :: (Tpl, Datum k v) -> Maybe (Tpl, Node k v)
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
      tx = Constraints.mustPayToTheScript (MapDatum $ Map Nothing False (SnapshotVersion 0)) nftValue

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
      zipped = zip nodes (reverse nodes)
      map' = mapLookup'map lkp

  case zipped of
    [] -> addToEmptyMap @t mapInstance map' pair
    (head', last') : _
      | goesBefore (snd head') -> addSmallest @t mapInstance map' head' pair
      | goesAfter (snd last') -> addGreatest @t mapInstance map' last' pair
      | otherwise -> case findPlaceInTheMiddle @t lkp key of
        Just neighbours -> addInTheMiddle @t mapInstance map' neighbours pair
        Nothing -> throwError "Key already in the map"
  where
    goesBefore :: Node (K' t) (V' t) -> Bool
    goesBefore node = key < T.node'key node

    goesAfter :: Node (K' t) (V' t) -> Bool
    goesAfter node = key > T.node'key node

remove ::
  forall (t :: Type) (s :: Row Type) (w :: Type).
  MapTerms t =>
  MapInstance ->
  K' t ->
  Contract w s Text ()
remove mapInstance key = do
  lkp <- mkMapLookup @t mapInstance

  let nodes = mapLookup'nodes lkp
      zipped = zip nodes (reverse nodes)
      map' = mapLookup'map lkp

  case zipped of
    [(head', _)] | at (snd head') -> removeFromOneElementMap @t mapInstance map' head'
    (head', last') : (oneAfterHead', oneBeforeLast') : _
      | at (snd head') -> removeSmallest @t mapInstance map' head' oneAfterHead'
      | at (snd last') -> removeGreatest @t mapInstance map' oneBeforeLast' last'
      | otherwise -> case findKeyInTheMiddle @t lkp key of
        Just triple -> removeInTheMiddle @t mapInstance map' triple
        Nothing -> throwError "Key not in the map"
    _ -> throwError "Key not in the map"
  where
    at :: Node (K' t) (V' t) -> Bool
    at node = key == T.node'key node

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
            updated = node {T.node'value = update (T.node'value node)}
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

fromJust' :: forall (a :: Type) (s :: Row Type) (w :: Type). Maybe a -> Contract w s Text a
fromJust' maybe' = case maybe' of
  Just v -> return v
  _ -> throwError "Empty maybe"

removeFromOneElementMap ::
  forall (t :: Type) (s :: Row Type) (w :: Type).
  MapTerms t =>
  MapInstance ->
  (Tpl, Map) ->
  (Tpl, Node (K' t) (V' t)) ->
  Contract w s Text ()
removeFromOneElementMap mapInstance map' node' =
  do
    tokenAC <- T.unPointer <$> fromJust' (T.map'head $ snd map')
    let tokenRedeemer = RemoveFromOneElementMap

        tokenValue = Value.assetClassValue tokenAC (-1)
        nftValue = Value.assetClassValue (T.unMapInstance mapInstance) 1

        toSpend = M.fromList [fst map', fst node']

        lookups = lookups' @t mapInstance toSpend
        tx =
          Constraints.mustMintValueWithRedeemer
            (Ledger.Redeemer $ Ledger.toBuiltinData @NodeValidTokenRedeemer tokenRedeemer)
            tokenValue
            <> Constraints.mustPayToTheScript
              (MapDatum $ Map Nothing False (T.map'nextVersion $ snd map'))
              nftValue
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
    tokenAC <- T.unPointer <$> fromJust' (T.map'head $ snd map')
    let tokenRedeemer = RemoveSmallest

        tokenValue = Value.assetClassValue tokenAC (-1)
        nftValue = Value.assetClassValue (T.unMapInstance mapInstance) 1

        toSpend = M.fromList [fst map', fst node', fst next']

        lookups = lookups' @t mapInstance toSpend
        tx =
          Constraints.mustMintValueWithRedeemer
            (Ledger.Redeemer $ Ledger.toBuiltinData @NodeValidTokenRedeemer tokenRedeemer)
            tokenValue
            <> Constraints.mustPayToTheScript
              (MapDatum $ Map (T.node'next $ snd node') False (T.map'nextVersion $ snd map'))
              nftValue
            <> Constraints.mustPayToTheScript
              (NodeDatum $ snd next')
              (snd (fst next') ^. Ledger.ciTxOutValue)
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
  (Tpl, Map) ->
  (Tpl, Node (K' t) (V' t)) ->
  (Tpl, Node (K' t) (V' t)) ->
  Contract w s Text ()
removeGreatest mapInstance map' node' next' =
  do
    tokenAC <- T.unPointer <$> fromJust' (T.node'next $ snd node')
    let tokenRedeemer = RemoveGreatest (fst $ fst node')

        tokenValue = Value.assetClassValue tokenAC (-1)
        nftValue = Value.assetClassValue (T.unMapInstance mapInstance) 1

        toSpend = M.fromList [fst map', fst node', fst next']

        lookups = lookups' @t mapInstance toSpend
        tx =
          Constraints.mustMintValueWithRedeemer
            (Ledger.Redeemer $ Ledger.toBuiltinData @NodeValidTokenRedeemer tokenRedeemer)
            tokenValue
            <> Constraints.mustPayToTheScript
              (NodeDatum (snd node'){node'next = Nothing})
              (snd (fst node') ^. Ledger.ciTxOutValue)
            <> Constraints.mustPayToTheScript
              (MapDatum $ snd map')
              nftValue
            <> Constraints.mustSpendScriptOutput
              (fst $ fst map')
              (Ledger.Redeemer $ Ledger.toBuiltinData ListOp)
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
  (Tpl, Map) ->
  ((Tpl, Node (K' t) (V' t)), (Tpl, Node (K' t) (V' t)), (Tpl, Node (K' t) (V' t))) ->
  Contract w s Text ()
removeInTheMiddle mapInstance map' (prev, mid, next) =
  do
    tokenAC <- T.unPointer <$> fromJust' (T.node'next $ snd prev)
    let tokenRedeemer = RemoveInTheMiddle (fst $ fst prev)

        tokenValue = Value.assetClassValue tokenAC (-1)
        nftValue = Value.assetClassValue (T.unMapInstance mapInstance) 1

        toSpend = M.fromList [fst map', fst prev, fst mid, fst next]

        lookups = lookups' @t mapInstance toSpend
        tx =
          Constraints.mustMintValueWithRedeemer
            (Ledger.Redeemer $ Ledger.toBuiltinData @NodeValidTokenRedeemer tokenRedeemer)
            tokenValue
            <> Constraints.mustPayToTheScript
              (NodeDatum (snd prev){node'next = T.node'next (snd mid)})
              (snd (fst prev) ^. Ledger.ciTxOutValue)
            <> Constraints.mustPayToTheScript
              (NodeDatum (snd next))
              (snd (fst next) ^. Ledger.ciTxOutValue)
            <> Constraints.mustPayToTheScript
              (MapDatum $ snd map')
              nftValue
            <> Constraints.mustSpendScriptOutput
              (fst $ fst map')
              (Ledger.Redeemer $ Ledger.toBuiltinData ListOp)
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
    let tokenRedeemer = AddToEmptyMap

        tokenAC = Value.assetClass (nodeValidPolicySymbol @t mapInstance) (V.tokenName (fst $ fst map'))
        tokenValue = Value.assetClassValue tokenAC 1
        nftValue = Value.assetClassValue (T.unMapInstance mapInstance) 1

        toSpend = M.fromList [fst map']

        lookups = lookups' @t mapInstance toSpend
        tx =
          Constraints.mustMintValueWithRedeemer
            (Ledger.Redeemer $ Ledger.toBuiltinData @NodeValidTokenRedeemer tokenRedeemer)
            tokenValue
            <> Constraints.mustPayToTheScript
              (MapDatum $ Map (Just $ Pointer tokenAC) False (T.map'nextVersion $ snd map'))
              nftValue
            <> Constraints.mustPayToTheScript
              (NodeDatum $ Node key value Nothing False)
              tokenValue
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
    let tokenRedeemer = AddSmallest

        tokenAC = Value.assetClass (nodeValidPolicySymbol @t mapInstance) (V.tokenName (fst $ fst map'))
        tokenValue = Value.assetClassValue tokenAC 1
        nftValue = Value.assetClassValue (T.unMapInstance mapInstance) 1

        nodePointer = T.map'head $ snd map'

        toSpend = M.fromList [fst map', fst node']

        lookups = lookups' @t mapInstance toSpend
        tx =
          Constraints.mustMintValueWithRedeemer
            (Ledger.Redeemer $ Ledger.toBuiltinData @NodeValidTokenRedeemer tokenRedeemer)
            tokenValue
            <> Constraints.mustPayToTheScript
              (MapDatum $ Map (Just $ Pointer tokenAC) False (T.map'nextVersion $ snd map'))
              nftValue
            <> Constraints.mustPayToTheScript
              (NodeDatum $ Node key value nodePointer False)
              tokenValue
            <> Constraints.mustPayToTheScript
              (NodeDatum $ Node (T.node'key $ snd node') (T.node'value $ snd node') Nothing False)
              (snd (fst node') ^. Ledger.ciTxOutValue)
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
  (Tpl, Map) ->
  (Tpl, Node (K' t) (V' t)) ->
  (K' t, V' t) ->
  Contract w s Text ()
addGreatest mapInstance map' node' (key, value) =
  do
    let tokenRedeemer = AddGreatest (fst $ fst node')

        tokenAC = Value.assetClass (nodeValidPolicySymbol @t mapInstance) (V.tokenName (fst $ fst node'))
        tokenValue = Value.assetClassValue tokenAC 1
        nftValue = Value.assetClassValue (T.unMapInstance mapInstance) 1

        toSpend = M.fromList [fst map', fst node']

        lookups = lookups' @t mapInstance toSpend
        tx =
          Constraints.mustMintValueWithRedeemer
            (Ledger.Redeemer $ Ledger.toBuiltinData @NodeValidTokenRedeemer tokenRedeemer)
            tokenValue
            <> Constraints.mustPayToTheScript
              (NodeDatum $ Node key value Nothing False)
              tokenValue
            <> Constraints.mustPayToTheScript
              (NodeDatum $ Node (T.node'key $ snd node') (T.node'value $ snd node') (Just $ Pointer tokenAC) False)
              (snd (fst node') ^. Ledger.ciTxOutValue)
            <> Constraints.mustPayToTheScript
              (MapDatum $ snd map')
              nftValue
            <> Constraints.mustSpendScriptOutput
              (fst $ fst map')
              (Ledger.Redeemer $ Ledger.toBuiltinData ListOp)
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
  (Tpl, Map) ->
  ((Tpl, Node (K' t) (V' t)), (Tpl, Node (K' t) (V' t))) ->
  (K' t, V' t) ->
  Contract w s Text ()
addInTheMiddle mapInstance map' (before, after) (key, value) =
  do
    let tokenRedeemer = AddInTheMiddle (fst $ fst before)

        tokenAC = Value.assetClass (nodeValidPolicySymbol @t mapInstance) (V.tokenName (fst $ fst before))
        tokenValue = Value.assetClassValue tokenAC 1
        nftValue = Value.assetClassValue (T.unMapInstance mapInstance) 1

        toSpend = M.fromList [fst map', fst before, fst after]

        lookups = lookups' @t mapInstance toSpend
        tx =
          Constraints.mustMintValueWithRedeemer
            (Ledger.Redeemer $ Ledger.toBuiltinData @NodeValidTokenRedeemer tokenRedeemer)
            tokenValue
            <> Constraints.mustPayToTheScript
              (NodeDatum $ Node key value (T.node'next $ snd before) False)
              tokenValue
            <> Constraints.mustPayToTheScript
              (NodeDatum $ Node (T.node'key $ snd before) (T.node'value $ snd before) (Just $ Pointer tokenAC) False)
              (snd (fst before) ^. Ledger.ciTxOutValue)
            <> Constraints.mustPayToTheScript
              (NodeDatum $ Node (T.node'key $ snd after) (T.node'value $ snd after) (T.node'next $ snd after) False)
              (snd (fst after) ^. Ledger.ciTxOutValue)
            <> Constraints.mustPayToTheScript
              (MapDatum $ snd map')
              nftValue
            <> Constraints.mustSpendScriptOutput
              (fst $ fst map')
              (Ledger.Redeemer $ Ledger.toBuiltinData ListOp)
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
  case nodes of
    _ : tail' ->
      let zipped = zip nodes tail'
          places = (\((_, node1), (_, node2)) -> T.node'key node1 < key && key < T.node'key node2) `filter` zipped
       in listToMaybe places
    _ -> Nothing
  where
    nodes = mapLookup'nodes lkp

findKeyInTheMiddle ::
  forall t.
  MapTerms t =>
  MapLookup (K' t) (V' t) ->
  K' t ->
  Maybe ((Tpl, Node (K' t) (V' t)), (Tpl, Node (K' t) (V' t)), (Tpl, Node (K' t) (V' t)))
findKeyInTheMiddle lkp key =
  case nodes of
    _ : tail'@(_ : tail'') ->
      let zipped = zip3 nodes tail' tail''
          places = (\((_, _), (_, node), (_, _)) -> T.node'key node == key) `filter` zipped
       in listToMaybe places
    _ -> Nothing
  where
    nodes = mapLookup'nodes lkp

findKey ::
  forall t.
  MapTerms t =>
  MapLookup (K' t) (V' t) ->
  K' t ->
  Maybe (Tpl, Node (K' t) (V' t))
findKey lkp key =
  let nodes = mapLookup'nodes lkp
      places = (\(_, node) -> T.node'key node == key) `filter` nodes
   in listToMaybe places
