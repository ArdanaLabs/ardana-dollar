{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module ArdanaDollar.Map.ListOpContracts (
  create,
  createTest,
  insert,
  remove,
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
  ownPubKeyHash,
  submitTxConstraintsWith,
  tell,
  throwError,
 )
import Plutus.Contracts.Currency qualified as Currency
import Plutus.V1.Ledger.Api qualified as Ledger
import Plutus.V1.Ledger.Value qualified as Value
import PlutusTx.Prelude qualified

import Control.Lens ((^.))
import Control.Monad (void)
import Data.Kind (Type)
import Data.Map qualified as M
import Data.Maybe (listToMaybe)
import Data.Monoid (Last (Last))
import Data.Row (Row)
import Data.Text (Text, pack)

import ArdanaDollar.Map.ContractUtils (
  MapLookup (mapLookup'map, mapLookup'nodes),
  Tpl,
  address,
  fromJust',
  lookups',
  mkMapLookup,
  nodeValidPolicySymbol,
 )
import ArdanaDollar.Map.MapTerms (
  MapTerms,
  MapTerms' (K', V', ValidatorTypes', inst', nodeValidPolicy'),
 )
import ArdanaDollar.Map.Types (
  Datum (MapDatum, NodeDatum),
  LockState (Unlocked),
  Map (Map),
  MapInstance (MapInstance),
  Node (Node),
  NodeValidTokenRedeemer (..),
  Pointer (Pointer),
  Redeemer (ListOp),
  SnapshotVersion (SnapshotVersion),
 )
import ArdanaDollar.Map.Types qualified as T
import ArdanaDollar.Map.Validator qualified as V

mintNFT ::
  forall (s :: Row Type) (w :: Type).
  Contract w s Text Ledger.AssetClass
mintNFT = do
  self <- ownPubKeyHash
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
      tx = Constraints.mustPayToTheScript (MapDatum $ Map Nothing Unlocked (SnapshotVersion 0)) nftValue

  ledgerTx <- submitTxConstraintsWith @(ValidatorTypes' t) lookups tx
  void $ awaitTxConfirmed $ Ledger.getCardanoTxId ledgerTx

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
      | goesAfter (snd last') -> addGreatest @t mapInstance last' pair
      | otherwise -> case findPlaceInTheMiddle @t lkp key of
        Just neighbours -> addInTheMiddle @t mapInstance neighbours pair
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
      | at (snd last') -> removeGreatest @t mapInstance oneBeforeLast' last'
      | otherwise -> case findKeyInTheMiddle @t lkp key of
        Just triple -> removeInTheMiddle @t mapInstance triple
        Nothing -> throwError "Key not in the map"
    _ -> throwError "Key not in the map"
  where
    at :: Node (K' t) (V' t) -> Bool
    at node = key == T.node'key node

removeFromOneElementMap ::
  forall (t :: Type) (s :: Row Type) (w :: Type).
  MapTerms t =>
  MapInstance ->
  (Tpl, Map) ->
  (Tpl, Node (K' t) (V' t)) ->
  Contract w s Text ()
removeFromOneElementMap mapInstance map' node' =
  do
    tokenAC <- T.unPointer <$> fromJust' "Empty" (T.map'head $ snd map')
    let tokenRedeemer = RemoveFromOneElementMap

        tokenValue = Value.assetClassValue tokenAC (-1)
        nftValue = Value.assetClassValue (T.unMapInstance mapInstance) 1

        toSpend = M.fromList [fst map', fst node']

        lookups =
          lookups' @t mapInstance toSpend
            <> Constraints.mintingPolicy (nodeValidPolicy' @t mapInstance)

        tx =
          Constraints.mustMintValueWithRedeemer
            (Ledger.Redeemer $ Ledger.toBuiltinData @NodeValidTokenRedeemer tokenRedeemer)
            tokenValue
            <> Constraints.mustPayToTheScript
              (MapDatum $ Map Nothing Unlocked (T.map'nextVersion $ snd map'))
              nftValue
            <> Constraints.mustSpendScriptOutput
              (fst $ fst map')
              (Ledger.Redeemer $ Ledger.toBuiltinData ListOp)
            <> Constraints.mustSpendScriptOutput
              (fst $ fst node')
              (Ledger.Redeemer $ Ledger.toBuiltinData ListOp)

    logInfo @String $ "Map: remove from one element map"

    ledgerTx <- submitTxConstraintsWith @(ValidatorTypes' t) lookups tx
    void $ awaitTxConfirmed $ Ledger.getCardanoTxId ledgerTx

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
    tokenAC <- T.unPointer <$> fromJust' "Empty" (T.map'head $ snd map')
    let tokenRedeemer = RemoveSmallest

        tokenValue = Value.assetClassValue tokenAC (-1)
        nftValue = Value.assetClassValue (T.unMapInstance mapInstance) 1

        toSpend = M.fromList [fst map', fst node', fst next']

        lookups =
          lookups' @t mapInstance toSpend
            <> Constraints.mintingPolicy (nodeValidPolicy' @t mapInstance)

        tx =
          Constraints.mustMintValueWithRedeemer
            (Ledger.Redeemer $ Ledger.toBuiltinData @NodeValidTokenRedeemer tokenRedeemer)
            tokenValue
            <> Constraints.mustPayToTheScript
              (MapDatum $ Map (T.node'next $ snd node') Unlocked (T.map'nextVersion $ snd map'))
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
    void $ awaitTxConfirmed $ Ledger.getCardanoTxId ledgerTx

removeGreatest ::
  forall (t :: Type) (s :: Row Type) (w :: Type).
  MapTerms t =>
  MapInstance ->
  (Tpl, Node (K' t) (V' t)) ->
  (Tpl, Node (K' t) (V' t)) ->
  Contract w s Text ()
removeGreatest mapInstance node' next' =
  do
    tokenAC <- T.unPointer <$> fromJust' "Empty" (T.node'next $ snd node')
    let tokenRedeemer = RemoveGreatest (fst $ fst node')

        tokenValue = Value.assetClassValue tokenAC (-1)

        toSpend = M.fromList [fst node', fst next']

        lookups =
          lookups' @t mapInstance toSpend
            <> Constraints.mintingPolicy (nodeValidPolicy' @t mapInstance)

        tx =
          Constraints.mustMintValueWithRedeemer
            (Ledger.Redeemer $ Ledger.toBuiltinData @NodeValidTokenRedeemer tokenRedeemer)
            tokenValue
            <> Constraints.mustPayToTheScript
              (NodeDatum (snd node'){node'next = Nothing})
              (snd (fst node') ^. Ledger.ciTxOutValue)
            <> Constraints.mustSpendScriptOutput
              (fst $ fst node')
              (Ledger.Redeemer $ Ledger.toBuiltinData ListOp)
            <> Constraints.mustSpendScriptOutput
              (fst $ fst next')
              (Ledger.Redeemer $ Ledger.toBuiltinData ListOp)

    logInfo @String $ "Map: remove greatest"

    ledgerTx <- submitTxConstraintsWith @(ValidatorTypes' t) lookups tx
    void $ awaitTxConfirmed $ Ledger.getCardanoTxId ledgerTx

removeInTheMiddle ::
  forall (t :: Type) (s :: Row Type) (w :: Type).
  MapTerms t =>
  MapInstance ->
  ((Tpl, Node (K' t) (V' t)), (Tpl, Node (K' t) (V' t)), (Tpl, Node (K' t) (V' t))) ->
  Contract w s Text ()
removeInTheMiddle mapInstance (prev, mid, next) =
  do
    tokenAC <- T.unPointer <$> fromJust' "Empty" (T.node'next $ snd prev)
    let tokenRedeemer = RemoveInTheMiddle (fst $ fst prev)

        tokenValue = Value.assetClassValue tokenAC (-1)

        toSpend = M.fromList [fst prev, fst mid, fst next]

        lookups =
          lookups' @t mapInstance toSpend
            <> Constraints.mintingPolicy (nodeValidPolicy' @t mapInstance)

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
    void $ awaitTxConfirmed $ Ledger.getCardanoTxId ledgerTx

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

        lookups =
          lookups' @t mapInstance toSpend
            <> Constraints.mintingPolicy (nodeValidPolicy' @t mapInstance)

        tx =
          Constraints.mustMintValueWithRedeemer
            (Ledger.Redeemer $ Ledger.toBuiltinData @NodeValidTokenRedeemer tokenRedeemer)
            tokenValue
            <> Constraints.mustPayToTheScript
              (MapDatum $ Map (Just $ Pointer tokenAC) Unlocked (T.map'nextVersion $ snd map'))
              nftValue
            <> Constraints.mustPayToTheScript
              (NodeDatum $ Node key value Nothing Unlocked)
              tokenValue
            <> Constraints.mustSpendScriptOutput
              (fst $ fst map')
              (Ledger.Redeemer $ Ledger.toBuiltinData ListOp)

    logInfo @String $ "Map: add to empty map"

    ledgerTx <- submitTxConstraintsWith @(ValidatorTypes' t) lookups tx
    void $ awaitTxConfirmed $ Ledger.getCardanoTxId ledgerTx

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

        lookups =
          lookups' @t mapInstance toSpend
            <> Constraints.mintingPolicy (nodeValidPolicy' @t mapInstance)

        tx =
          Constraints.mustMintValueWithRedeemer
            (Ledger.Redeemer $ Ledger.toBuiltinData @NodeValidTokenRedeemer tokenRedeemer)
            tokenValue
            <> Constraints.mustPayToTheScript
              (MapDatum $ Map (Just $ Pointer tokenAC) Unlocked (T.map'nextVersion $ snd map'))
              nftValue
            <> Constraints.mustPayToTheScript
              (NodeDatum $ Node key value nodePointer Unlocked)
              tokenValue
            <> Constraints.mustPayToTheScript
              (NodeDatum $ Node (T.node'key $ snd node') (T.node'value $ snd node') (T.node'next $ snd node') Unlocked)
              (snd (fst node') ^. Ledger.ciTxOutValue)
            <> Constraints.mustSpendScriptOutput
              (fst $ fst map')
              (Ledger.Redeemer $ Ledger.toBuiltinData ListOp)
            <> Constraints.mustSpendScriptOutput
              (fst $ fst node')
              (Ledger.Redeemer $ Ledger.toBuiltinData ListOp)

    logInfo @String $ "Map: add smallest"

    ledgerTx <- submitTxConstraintsWith @(ValidatorTypes' t) lookups tx
    void $ awaitTxConfirmed $ Ledger.getCardanoTxId ledgerTx

addGreatest ::
  forall (t :: Type) (s :: Row Type) (w :: Type).
  MapTerms t =>
  MapInstance ->
  (Tpl, Node (K' t) (V' t)) ->
  (K' t, V' t) ->
  Contract w s Text ()
addGreatest mapInstance node' (key, value) =
  do
    let tokenRedeemer = AddGreatest (fst $ fst node')

        tokenAC = Value.assetClass (nodeValidPolicySymbol @t mapInstance) (V.tokenName (fst $ fst node'))
        tokenValue = Value.assetClassValue tokenAC 1

        toSpend = M.fromList [fst node']

        lookups =
          lookups' @t mapInstance toSpend
            <> Constraints.mintingPolicy (nodeValidPolicy' @t mapInstance)

        tx =
          Constraints.mustMintValueWithRedeemer
            (Ledger.Redeemer $ Ledger.toBuiltinData @NodeValidTokenRedeemer tokenRedeemer)
            tokenValue
            <> Constraints.mustPayToTheScript
              (NodeDatum $ Node key value Nothing Unlocked)
              tokenValue
            <> Constraints.mustPayToTheScript
              (NodeDatum $ Node (T.node'key $ snd node') (T.node'value $ snd node') (Just $ Pointer tokenAC) Unlocked)
              (snd (fst node') ^. Ledger.ciTxOutValue)
            <> Constraints.mustSpendScriptOutput
              (fst $ fst node')
              (Ledger.Redeemer $ Ledger.toBuiltinData ListOp)

    logInfo @String $ "Map: add greatest"

    ledgerTx <- submitTxConstraintsWith @(ValidatorTypes' t) lookups tx
    void $ awaitTxConfirmed $ Ledger.getCardanoTxId ledgerTx

addInTheMiddle ::
  forall (t :: Type) (s :: Row Type) (w :: Type).
  MapTerms t =>
  MapInstance ->
  ((Tpl, Node (K' t) (V' t)), (Tpl, Node (K' t) (V' t))) ->
  (K' t, V' t) ->
  Contract w s Text ()
addInTheMiddle mapInstance (before, after) (key, value) =
  do
    let tokenRedeemer = AddInTheMiddle (fst $ fst before)

        tokenAC = Value.assetClass (nodeValidPolicySymbol @t mapInstance) (V.tokenName (fst $ fst before))
        tokenValue = Value.assetClassValue tokenAC 1

        toSpend = M.fromList [fst before, fst after]

        lookups =
          lookups' @t mapInstance toSpend
            <> Constraints.mintingPolicy (nodeValidPolicy' @t mapInstance)

        tx =
          Constraints.mustMintValueWithRedeemer
            (Ledger.Redeemer $ Ledger.toBuiltinData @NodeValidTokenRedeemer tokenRedeemer)
            tokenValue
            <> Constraints.mustPayToTheScript
              (NodeDatum $ Node key value (T.node'next $ snd before) Unlocked)
              tokenValue
            <> Constraints.mustPayToTheScript
              (NodeDatum $ Node (T.node'key $ snd before) (T.node'value $ snd before) (Just $ Pointer tokenAC) Unlocked)
              (snd (fst before) ^. Ledger.ciTxOutValue)
            <> Constraints.mustPayToTheScript
              (NodeDatum $ Node (T.node'key $ snd after) (T.node'value $ snd after) (T.node'next $ snd after) Unlocked)
              (snd (fst after) ^. Ledger.ciTxOutValue)
            <> Constraints.mustSpendScriptOutput
              (fst $ fst before)
              (Ledger.Redeemer $ Ledger.toBuiltinData ListOp)
            <> Constraints.mustSpendScriptOutput
              (fst $ fst after)
              (Ledger.Redeemer $ Ledger.toBuiltinData ListOp)

    logInfo @String $ "Map: add in the middle"

    ledgerTx <- submitTxConstraintsWith @(ValidatorTypes' t) lookups tx
    void $ awaitTxConfirmed $ Ledger.getCardanoTxId ledgerTx

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
