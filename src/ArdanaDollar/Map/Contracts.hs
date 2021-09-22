{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module ArdanaDollar.Map.Contracts (
  create,
  insert,
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

  let nftValue = Value.assetClassValue nftAC 1
      mapInstance = MapInstance nftAC

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
      | key > node'key (snd $ last $ mapLookup'nodes lkp) -> return () -- TODO
      | isJust placeInTheMiddle -> return () -- TODO
      | otherwise -> return ()

addToEmptyMap :: forall (s :: Row Type) (w :: Type). MapInstance -> MapLookup -> (Key, Value) -> Contract w s Text ()
addToEmptyMap mapInstance lkp (key, value) =
  do
    let tokenRedeemer = AddToEmptyMap key
        _map = mapLookup'map lkp

        tokenAC = Value.assetClass (nodeValidPolicySymbol mapInstance) (tokenName (fst $ fst _map))
        tokenValue = Value.assetClassValue tokenAC 1
        nftValue = Value.assetClassValue (unMapInstance mapInstance) 1

        toSpend = Map.fromList [fst _map]

        lookups =
          Constraints.typedValidatorLookups (inst mapInstance)
            <> Constraints.otherScript (validator mapInstance)
            <> Constraints.mintingPolicy (nodeValidPolicy mapInstance)
            <> Constraints.unspentOutputs toSpend
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

        lookups =
          Constraints.typedValidatorLookups (inst mapInstance)
            <> Constraints.otherScript (validator mapInstance)
            <> Constraints.mintingPolicy (nodeValidPolicy mapInstance)
            <> Constraints.unspentOutputs toSpend
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

findPlaceInTheMiddle :: MapLookup -> Key -> Maybe (Tpl, Tpl)
findPlaceInTheMiddle lkp key =
  let nodes = mapLookup'nodes lkp
      zipped = zip nodes (tail nodes)
      places = (\((_, node1), (_, node2)) -> node'key node1 < key && key < node'key node2) `filter` zipped
   in if not (null nodes) && not (null places)
        then
          let ((ref1, _), (ref2, _)) = head places
           in return (ref1, ref2)
        else Nothing
