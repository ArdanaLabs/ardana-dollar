{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module ArdanaDollar.Map.TxUtils (
  hasOne,
  hasOne',
  lookupToken,
  lookupToken',
  inputsAt',
  outputsAt',
  mapInput',
  mapOutput',
  mapSnapshotOutput',
  nodeByKey',
  nodeByPointer',
  nodeInputRef',
  snapshotPermOutput',
  snapshotPermInputByRef',
  nodeSnapshotOutput',
  tokenName,
) where

import Ledger qualified
import Ledger.Value qualified as Value

import PlutusTx.IsData.Class (FromData)
import PlutusTx.Prelude

import ArdanaDollar.Map.Types (
  Datum (MapDatum, MapSnapshotDatum, NodeDatum, NodeSnapshotDatum, SnapshotPermDatum),
  Map,
  MapInstance (..),
  MapSnapshot (..),
  Node,
  NodeSnapshot (..),
  Pointer,
  PointerCS (..),
  SnapshotPerm (..),
 )
import ArdanaDollar.Map.Types qualified as T
import ArdanaDollar.Utils (datumForOnchain)

{-# INLINEABLE maybeToList #-}
maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just !x) = [x]

{-# INLINEABLE tokenName #-}
tokenName :: Ledger.TxOutRef -> Ledger.TokenName
tokenName !ref =
  Value.TokenName $ consByteString (Ledger.txOutRefIdx ref) $ Ledger.getTxId (Ledger.txOutRefId ref)

{-# INLINEABLE mapDatum #-}
mapDatum :: forall k v. (FromData k, FromData v) => Ledger.TxInfo -> Ledger.TxOut -> Maybe Map
mapDatum info txOut = case datumForOnchain @(Datum k v) info txOut of
  Just (MapDatum !dat) -> Just dat
  _ -> Nothing

{-# INLINEABLE mapSnapshotDatum #-}
mapSnapshotDatum :: forall k v. (FromData k, FromData v) => Ledger.TxInfo -> Ledger.TxOut -> Maybe MapSnapshot
mapSnapshotDatum info txOut = case datumForOnchain @(Datum k v) info txOut of
  Just (MapSnapshotDatum !dat) -> Just dat
  _ -> Nothing

{-# INLINEABLE snapshotPermDatum #-}
snapshotPermDatum :: forall k v. (FromData k, FromData v) => Ledger.TxInfo -> Ledger.TxOut -> Maybe SnapshotPerm
snapshotPermDatum info txOut = case datumForOnchain @(Datum k v) info txOut of
  Just (SnapshotPermDatum !dat) -> Just dat
  _ -> Nothing

{-# INLINEABLE nodeSnapshotDatum #-}
nodeSnapshotDatum :: forall k v. (FromData k, FromData v) => Ledger.TxInfo -> Ledger.TxOut -> Maybe (NodeSnapshot k v)
nodeSnapshotDatum info txOut = case datumForOnchain @(Datum k v) info txOut of
  Just (NodeSnapshotDatum !dat) -> Just dat
  _ -> Nothing

{-# INLINEABLE mapNode #-}
mapNode :: forall k v. (FromData k, FromData v) => Ledger.TxInfo -> Ledger.TxOut -> Maybe (Node k v)
mapNode info txOut = case datumForOnchain @(Datum k v) info txOut of
  Just (NodeDatum !dat) -> Just dat
  _ -> Nothing

{-# INLINEABLE hasOne' #-}
hasOne' :: Ledger.AssetClass -> Value.Value -> Bool
hasOne' ac value = Value.assetClassValueOf value ac == 1

{-# INLINEABLE hasOne #-}
hasOne :: Ledger.AssetClass -> Ledger.TxOut -> Bool
hasOne ac txOut = hasOne' ac (Ledger.txOutValue txOut)

{-# INLINEABLE lookupToken' #-}
lookupToken' :: Ledger.CurrencySymbol -> Value.Value -> Maybe Ledger.AssetClass
lookupToken' expected value =
  let flattened = Value.flattenValue value
      f = (\(cs, _, _) -> cs == expected) `filter` flattened
   in case f of
        [(cs, tn, amt)] | amt == 1 -> Just $ Value.assetClass cs tn
        _ -> Nothing

{-# INLINEABLE lookupToken #-}
lookupToken :: Ledger.CurrencySymbol -> Ledger.TxOut -> Maybe Ledger.AssetClass
lookupToken expected txOut = lookupToken' expected (Ledger.txOutValue txOut)

{-# INLINEABLE mapInput' #-}
mapInput' ::
  forall k v.
  (FromData k, FromData v) =>
  Ledger.TxInfo ->
  MapInstance ->
  [Ledger.TxInInfo] ->
  Maybe (Ledger.TxInInfo, Map)
mapInput' info mapInstance inputs =
  case (hasOne (unMapInstance mapInstance) . Ledger.txInInfoResolved) `filter` inputs of
    fst' : _ -> (fst',) <$> mapDatum @k @v info (Ledger.txInInfoResolved fst')
    _ -> Nothing

{-# INLINEABLE mapOutput' #-}
mapOutput' ::
  forall k v.
  (FromData k, FromData v) =>
  Ledger.TxInfo ->
  MapInstance ->
  [Ledger.TxOut] ->
  Maybe (Ledger.TxOut, Map)
mapOutput' info mapInstance outputs =
  case hasOne (unMapInstance mapInstance) `filter` outputs of
    fst' : _ -> (fst',) <$> mapDatum @k @v info fst'
    _ -> Nothing

{-# INLINEABLE mapSnapshotOutput' #-}
mapSnapshotOutput' ::
  forall k v.
  (FromData k, FromData v) =>
  Ledger.TxInfo ->
  Value.AssetClass ->
  [Ledger.TxOut] ->
  Maybe (Ledger.TxOut, MapSnapshot)
mapSnapshotOutput' info snapshotAC outputs =
  case hasOne snapshotAC `filter` outputs of
    fst' : _ -> (fst',) <$> mapSnapshotDatum @k @v info fst'
    _ -> Nothing

{-# INLINEABLE snapshotPermOutput' #-}
snapshotPermOutput' ::
  forall k v.
  (FromData k, FromData v) =>
  Ledger.TxInfo ->
  Value.AssetClass ->
  [Ledger.TxOut] ->
  SnapshotPerm ->
  Maybe Ledger.TxOut
snapshotPermOutput' info snapshotAC outputs snapshotPerm =
  case (\txOut -> hasOne snapshotAC txOut && snapshotPermDatum @k @v info txOut == Just snapshotPerm) `filter` outputs of
    fst' : _ -> Just fst'
    [] -> Nothing

{-# INLINEABLE nodeSnapshotOutput' #-}
nodeSnapshotOutput' ::
  forall k v.
  (Eq k, Eq v, FromData k, FromData v) =>
  Ledger.TxInfo ->
  [Ledger.TxOut] ->
  Value.AssetClass ->
  NodeSnapshot k v ->
  Maybe Ledger.TxOut
nodeSnapshotOutput' info outputs snapshotAC nodeSnapshot =
  case (\txOut -> hasOne snapshotAC txOut && nodeSnapshotDatum @k @v info txOut == Just nodeSnapshot) `filter` outputs of
    fst' : _ -> Just fst'
    [] -> Nothing

{-# INLINEABLE nodeBy' #-}
nodeBy' ::
  forall k v.
  (FromData k, FromData v) =>
  Ledger.TxInfo ->
  [Ledger.TxOut] ->
  ((Ledger.TxOut, Node k v) -> Bool) ->
  Maybe (Ledger.TxOut, Node k v)
nodeBy' info lookupSet pred' =
  let l = lookupSet >>= \txOut -> maybeToList ((txOut,) <$> mapNode info txOut)
      ll = pred' `filter` l
   in listToMaybe ll

{-# INLINEABLE nodeByKey' #-}
nodeByKey' ::
  forall k v.
  (FromData k, Ord k, FromData v) =>
  Ledger.TxInfo ->
  PointerCS ->
  [Ledger.TxOut] ->
  k ->
  Maybe (Ledger.TxOut, Node k v)
nodeByKey' info pointerCS lookupSet key =
  let withPointer = (isJust . lookupToken (unPointerCS pointerCS)) `filter` lookupSet
   in nodeBy' info withPointer (matchesKey . snd)
  where
    matchesKey :: Node k v -> Bool
    matchesKey node = T.node'key node == key

{-# INLINEABLE nodeByPointer' #-}
nodeByPointer' ::
  forall k v.
  (FromData k, FromData v) =>
  Ledger.TxInfo ->
  PointerCS ->
  [Ledger.TxOut] ->
  Pointer ->
  Maybe (Ledger.TxOut, Node k v)
nodeByPointer' info pointerCS lookupSet pointer =
  let withPointer = (isJust . lookupToken (unPointerCS pointerCS)) `filter` lookupSet
   in nodeBy' info withPointer (matchesPointer . fst)
  where
    matchesPointer :: Ledger.TxOut -> Bool
    matchesPointer txOut = Value.assetClassValueOf (Ledger.txOutValue txOut) (T.unPointer pointer) == 1

{-# INLINEABLE nodeInputRef' #-}
nodeInputRef' ::
  forall k v.
  (FromData k, FromData v) =>
  Ledger.TxInfo ->
  PointerCS ->
  [Ledger.TxInInfo] ->
  Ledger.TxOutRef ->
  Maybe (Ledger.TxInInfo, Node k v)
nodeInputRef' info pointerCS inputs ref =
  case (\txOutRef -> (isJust . lookupToken (unPointerCS pointerCS) . Ledger.txInInfoResolved) txOutRef && matchesRef txOutRef) `filter` inputs of
    txInInfo : _ -> (txInInfo,) <$> mapNode info (Ledger.txInInfoResolved txInInfo)
    _ -> Nothing
  where
    matchesRef :: Ledger.TxInInfo -> Bool
    matchesRef txInInfo = Ledger.txInInfoOutRef txInInfo == ref

{-# INLINEABLE snapshotPermInputByRef' #-}
snapshotPermInputByRef' ::
  forall k v.
  (FromData k, FromData v) =>
  Ledger.TxInfo ->
  Value.AssetClass ->
  [Ledger.TxInInfo] ->
  Ledger.TxOutRef ->
  Maybe (Ledger.TxInInfo, SnapshotPerm)
snapshotPermInputByRef' info snapshotAC inputs ref =
  case (\txOutRef -> (hasOne snapshotAC . Ledger.txInInfoResolved) txOutRef && matchesRef txOutRef) `filter` inputs of
    txInInfo : _ -> (txInInfo,) <$> snapshotPermDatum @k @v info (Ledger.txInInfoResolved txInInfo)
    _ -> Nothing
  where
    matchesRef :: Ledger.TxInInfo -> Bool
    matchesRef txInInfo = Ledger.txInInfoOutRef txInInfo == ref

inputsAt' :: Ledger.TxInfo -> Ledger.Address -> Integer
inputsAt' !info !address =
  length $
    ( \txInInfo ->
        Ledger.txOutAddress
          (Ledger.txInInfoResolved txInInfo)
          == address
    )
      `filter` Ledger.txInfoInputs info

outputsAt' :: Ledger.TxInfo -> Ledger.Address -> Integer
outputsAt' !info !address =
  length $
    (\txOut -> Ledger.txOutAddress txOut == address) `filter` Ledger.txInfoOutputs info
