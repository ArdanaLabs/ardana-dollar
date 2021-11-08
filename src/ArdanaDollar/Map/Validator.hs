{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

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
  isLocked,
  lookupToken,
  lookupToken',
  mapOutput',
  nodeByKey',
  tokenName,
 )
import ArdanaDollar.Map.Types (
  Datum (MapDatum, NodeDatum),
  Map,
  MapInstance,
  Node (Node),
  PointerCS,
  Redeemer (ListOp, SnapshotOp, Use),
  SnapshotCS,
 )
import ArdanaDollar.Map.Types qualified as T

{-# INLINEABLE mkValidator #-}
mkValidator ::
  forall k v.
  (Ord k, FromData k, FromData v, Eq v) =>
  MapInstance ->
  PointerCS ->
  SnapshotCS ->
  Datum k v ->
  Redeemer ->
  Ledger.ScriptContext ->
  Bool
mkValidator inst pointerCS snapshotCS datum redeemer ctx =
  case redeemer of
    Use -> validateUseRedeemer
    ListOp -> nodePolicyFires
    SnapshotOp -> snapshotPolicyFires
  where
    info :: Ledger.TxInfo
    info = Ledger.scriptContextTxInfo ctx

    mapOutput :: Maybe (Ledger.TxOut, Map)
    mapOutput = mapOutput' @k @v info inst (Ledger.getContinuingOutputs ctx)

    nodeOutput :: k -> Maybe (Ledger.TxOut, Node k v)
    nodeOutput = nodeByKey' info pointerCS (Ledger.getContinuingOutputs ctx)

    hasNFT :: Ledger.TxOut -> Bool
    hasNFT = hasOne (T.unMapInstance inst)

    inputHasNFT :: Bool
    inputHasNFT = ownInputSatisfies hasNFT

    outputHasNFT :: Bool
    outputHasNFT = maybe False hasNFT (fst <$> mapOutput)

    inputToken :: Maybe Ledger.AssetClass
    inputToken = Ledger.findOwnInput ctx >>= lookupToken (T.unPointerCS pointerCS) . Ledger.txInInfoResolved

    outputToken :: k -> Maybe Ledger.AssetClass
    outputToken key = nodeOutput key >>= lookupToken (T.unPointerCS pointerCS) . fst

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
        i <- inputToken
        o <- outputToken key
        (outputNode', outputNode) <- nodeOutput key
        return $
          if isLocked lock
            then
              inputNode == outputNode
                && Just (Ledger.txOutValue outputNode')
                  == (Ledger.txOutValue . Ledger.txInInfoResolved <$> Ledger.findOwnInput ctx)
            else
              inputNode{node'value = T.node'value outputNode} == outputNode
                && i == o
      _ -> False

    snapshotPolicyFires :: Bool
    snapshotPolicyFires = mintingPolicyFires (T.unSnapshotCS snapshotCS)

    nodePolicyFires :: Bool
    nodePolicyFires = mintingPolicyFires (T.unPointerCS pointerCS)

    mintingPolicyFires :: Value.CurrencySymbol -> Bool
    mintingPolicyFires expected =
      let flattened = Value.flattenValue (Ledger.txInfoMint info)
       in case (\(cs, _, _) -> cs == expected) `filter` flattened of
            _ : _ -> True
            [] -> False
