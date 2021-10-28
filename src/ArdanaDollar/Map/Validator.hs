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
  Redeemer (ListOp, Use),
 )
import ArdanaDollar.Map.Types qualified as T

{-# INLINEABLE mkValidator #-}
mkValidator ::
  forall k v.
  (Ord k, FromData k, FromData v, Eq v) =>
  MapInstance ->
  PointerCS ->
  Datum k v ->
  Redeemer ->
  Ledger.ScriptContext ->
  Bool
mkValidator inst pointerCS datum redeemer ctx =
  case redeemer of
    Use -> validateUseRedeemer
    ListOp -> burnsXorMintsOneToken
  where
    info :: Ledger.TxInfo
    info = Ledger.scriptContextTxInfo ctx

    mapOutput :: Maybe (Ledger.TxOut, Map)
    mapOutput = mapOutput' @k @v info (Ledger.getContinuingOutputs ctx)

    nodeOutput :: k -> Maybe (Ledger.TxOut, Node k v)
    nodeOutput = nodeByKey' info (Ledger.getContinuingOutputs ctx)

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
      NodeDatum inputNode@(Node key _ _) -> fromMaybe False $ do
        i <- inputToken
        o <- outputToken key
        (_, outputNode) <- nodeOutput key
        return (i == o && inputNode{node'value = T.node'value outputNode} == outputNode)

    burnsXorMintsOneToken :: Bool
    burnsXorMintsOneToken =
      let flattened = Value.flattenValue (Ledger.txInfoMint info)
          f = (\(cs, _, _) -> cs == T.unPointerCS pointerCS) `filter` flattened
       in case f of
            [(_, _, amt)] -> amt == 1 || amt == -1
            _ -> False
