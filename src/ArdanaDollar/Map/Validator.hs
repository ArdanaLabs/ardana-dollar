{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-specialise #-}

module ArdanaDollar.Map.Validator (
  mkValidator,
  tokenName,
  hasOne,
  hasOne',
  lookupToken',
) where

import Ledger qualified
import Ledger.Value qualified as Value

import PlutusTx.IsData.Class (FromData)
import PlutusTx.Prelude

import ArdanaDollar.Map.TxUtils (
  hasOne,
  hasOne',
  isUnlocked,
  lookupToken,
  lookupToken',
  mapOutput',
  mapSnapshotOutput',
  nodeByKey',
  nodeSnapshotOutput',
  tokenName,
 )
import ArdanaDollar.Map.Types (
  Datum (MapDatum, MapSnapshotDatum, NodeDatum, NodeSnapshotDatum),
  Map,
  MapInstance,
  MapSnapshot,
  Node (Node),
  NodeSnapshot,
  PointerCS,
  Redeemer (ListOp, SnapshotOp, UnlockOp, UnlockPermOp, Use),
  SnapshotCS (..),
  UnlockCS (..),
  UnlockPermCS,
 )
import ArdanaDollar.Map.Types qualified as T

{-# INLINEABLE mkValidator #-}
mkValidator ::
  forall k v.
  (Ord k, FromData k, FromData v, Eq v) =>
  MapInstance ->
  PointerCS ->
  SnapshotCS ->
  UnlockPermCS ->
  UnlockCS ->
  Datum k v ->
  Redeemer ->
  Ledger.ScriptContext ->
  Bool
mkValidator inst pointerCS snapshotCS unlockPermCS unlockCS datum redeemer ctx =
  case redeemer of
    Use -> validateUseRedeemer
    ListOp -> nodePolicyFires
    SnapshotOp -> snapshotPolicyFires
    UnlockPermOp -> unlockPermPolicyFires
    UnlockOp -> unlockPolicyFires
  where
    info :: Ledger.TxInfo
    info = Ledger.scriptContextTxInfo ctx

    mapOutput :: Maybe (Ledger.TxOut, Map)
    mapOutput = mapOutput' @k @v info inst (Ledger.getContinuingOutputs ctx)

    nodeOutput :: k -> Maybe (Ledger.TxOut, Node k v)
    nodeOutput = nodeByKey' info pointerCS (Ledger.getContinuingOutputs ctx)

    mapSnapshotAC :: Ledger.AssetClass
    mapSnapshotAC = Value.AssetClass (unSnapshotCS snapshotCS, Value.TokenName emptyByteString)

    mapSnapshotOutput :: MapSnapshot -> Maybe Ledger.TxOut
    mapSnapshotOutput mapSnapshot = mapSnapshotOutput' @k @v info mapSnapshotAC (Ledger.getContinuingOutputs ctx) mapSnapshot

    nodeSnapshotOutput :: NodeSnapshot k v -> Maybe Ledger.TxOut
    nodeSnapshotOutput nodeSnapshot = nodeSnapshotOutput' @k @v info (Ledger.getContinuingOutputs ctx) snapshotCS nodeSnapshot

    hasNFT :: Ledger.TxOut -> Bool
    hasNFT = hasOne (T.unMapInstance inst)

    inputHasNFT :: Bool
    inputHasNFT = ownInputSatisfies hasNFT

    outputHasNFT :: Bool
    outputHasNFT = maybe False hasNFT (fst <$> mapOutput)

    inputPointerToken :: Maybe Ledger.AssetClass
    inputPointerToken = Ledger.findOwnInput ctx >>= lookupToken (T.unPointerCS pointerCS) . Ledger.txInInfoResolved

    inputSnapshotToken :: Maybe Ledger.AssetClass
    inputSnapshotToken = Ledger.findOwnInput ctx >>= lookupToken (T.unSnapshotCS snapshotCS) . Ledger.txInInfoResolved

    outputPointerToken :: k -> Maybe Ledger.AssetClass
    outputPointerToken key = nodeOutput key >>= lookupToken (T.unPointerCS pointerCS) . fst

    ownInputSatisfies :: (Ledger.TxOut -> Bool) -> Bool
    ownInputSatisfies f = maybe False (f . Ledger.txInInfoResolved) (Ledger.findOwnInput ctx)

    validateUseRedeemer :: Bool
    validateUseRedeemer = case datum of
      MapDatum inputMap ->
        fromMaybe False $ do
          (_, outputMap) <- mapOutput
          return
            ( inputHasNFT
                && outputHasNFT
                && inputMap == outputMap
            )
      NodeDatum inputNode@(Node key _ _ lock) -> fromMaybe False $ do
        i <- inputPointerToken
        o <- outputPointerToken key
        (outputNode', outputNode) <- nodeOutput key
        return $
          if isUnlocked lock
            then
              inputNode{node'value = T.node'value outputNode} == outputNode
                && i == o
            else
              inputNode == outputNode
                && Just (Ledger.txOutValue outputNode')
                  == (Ledger.txOutValue . Ledger.txInInfoResolved <$> Ledger.findOwnInput ctx)
      MapSnapshotDatum inputMapSnapshot -> fromMaybe False $ do
        _ <- inputSnapshotToken
        o <- mapSnapshotOutput inputMapSnapshot
        return $
          Just (Ledger.txOutValue o)
            == (Ledger.txOutValue . Ledger.txInInfoResolved <$> Ledger.findOwnInput ctx)
      NodeSnapshotDatum inputNodeSnapshot -> fromMaybe False $ do
        _ <- inputSnapshotToken
        o <- nodeSnapshotOutput inputNodeSnapshot
        return $
          Just (Ledger.txOutValue o)
            == (Ledger.txOutValue . Ledger.txInInfoResolved <$> Ledger.findOwnInput ctx)
      _ -> False

    snapshotPolicyFires :: Bool
    snapshotPolicyFires = mintingPolicyFires (T.unSnapshotCS snapshotCS)

    unlockPermPolicyFires :: Bool
    unlockPermPolicyFires = mintingPolicyFires (T.unUnlockPermCS unlockPermCS)

    unlockPolicyFires :: Bool
    unlockPolicyFires = mintingPolicyFires (T.unUnlockCS unlockCS)

    nodePolicyFires :: Bool
    nodePolicyFires = mintingPolicyFires (T.unPointerCS pointerCS)

    mintingPolicyFires :: Value.CurrencySymbol -> Bool
    mintingPolicyFires expected =
      expected `elem` Value.symbols (Ledger.txInfoMint info)
