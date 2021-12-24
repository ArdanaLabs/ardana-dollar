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
  nodeSnapshotInputByRef',
  mapSnapshotInputByRef',
  unlockPermOutput',
  unlockPermInputByRef',
  unlockInputByRef',
  unlockOutput',
  tokenName,
  unlockOutputs',
  isUnlocked,
  isLockedFor,
  isSnapshotDone,
  incrementSV,
) where

import Ledger qualified
import Ledger.Value qualified as Value

import PlutusTx.IsData.Class (FromData)
import PlutusTx.Prelude

import ArdanaDollar.Map.Types (
  Datum (
    MapDatum,
    MapSnapshotDatum,
    NodeDatum,
    NodeSnapshotDatum,
    SnapshotPermDatum,
    UnlockDatum,
    UnlockPermDatum
  ),
  LockState (..),
  Map,
  MapInstance (..),
  MapSnapshot (..),
  Node,
  NodeSnapshot (..),
  Pointer,
  PointerCS (..),
  SnapshotCS (..),
  SnapshotPerm (..),
  SnapshotVersion (..),
  Unlock (..),
  UnlockCS (..),
  UnlockPerm (..),
  UnlockPermCS (..),
 )
import ArdanaDollar.Map.Types qualified as T
import ArdanaDollar.Utils (datumForOnchain)

{-# INLINEABLE maybeToList #-}
maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

{-# INLINEABLE tokenName #-}
tokenName :: Ledger.TxOutRef -> Ledger.TokenName
tokenName ref =
  Value.TokenName $ consByteString (Ledger.txOutRefIdx ref) $ Ledger.getTxId (Ledger.txOutRefId ref)

{-# INLINEABLE mapDatum #-}
mapDatum :: forall k v. (FromData k, FromData v) => Ledger.TxInfo -> Ledger.TxOut -> Maybe Map
mapDatum info txOut = case datumForOnchain @(Datum k v) info txOut of
  Just (MapDatum dat) -> Just dat
  _ -> Nothing

{-# INLINEABLE mapSnapshotDatum #-}
mapSnapshotDatum :: forall k v. (FromData k, FromData v) => Ledger.TxInfo -> Ledger.TxOut -> Maybe MapSnapshot
mapSnapshotDatum info txOut = case datumForOnchain @(Datum k v) info txOut of
  Just (MapSnapshotDatum dat) -> Just dat
  _ -> Nothing

{-# INLINEABLE snapshotPermDatum #-}
snapshotPermDatum :: forall k v. (FromData k, FromData v) => Ledger.TxInfo -> Ledger.TxOut -> Maybe SnapshotPerm
snapshotPermDatum info txOut = case datumForOnchain @(Datum k v) info txOut of
  Just (SnapshotPermDatum dat) -> Just dat
  _ -> Nothing

{-# INLINEABLE nodeSnapshotDatum #-}
nodeSnapshotDatum :: forall k v. (FromData k, FromData v) => Ledger.TxInfo -> Ledger.TxOut -> Maybe (NodeSnapshot k v)
nodeSnapshotDatum info txOut = case datumForOnchain @(Datum k v) info txOut of
  Just (NodeSnapshotDatum dat) -> Just dat
  _ -> Nothing

{-# INLINEABLE mapNode #-}
mapNode :: forall k v. (FromData k, FromData v) => Ledger.TxInfo -> Ledger.TxOut -> Maybe (Node k v)
mapNode info txOut = case datumForOnchain @(Datum k v) info txOut of
  Just (NodeDatum dat) -> Just dat
  _ -> Nothing

{-# INLINEABLE unlockPermDatum #-}
unlockPermDatum :: forall k v. (FromData k, FromData v) => Ledger.TxInfo -> Ledger.TxOut -> Maybe UnlockPerm
unlockPermDatum info txOut = case datumForOnchain @(Datum k v) info txOut of
  Just (UnlockPermDatum dat) -> Just dat
  _ -> Nothing

{-# INLINEABLE unlockDatum #-}
unlockDatum :: forall k v. (FromData k, FromData v) => Ledger.TxInfo -> Ledger.TxOut -> Maybe Unlock
unlockDatum info txOut = case datumForOnchain @(Datum k v) info txOut of
  Just (UnlockDatum dat) -> Just dat
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
  let filtered = (hasOne (unMapInstance mapInstance) . Ledger.txInInfoResolved) `filter` inputs
   in listToMaybe filtered >>= (\fst' -> (fst',) <$> mapDatum @k @v info (Ledger.txInInfoResolved fst'))

{-# INLINEABLE mapOutput' #-}
mapOutput' ::
  forall k v.
  (FromData k, FromData v) =>
  Ledger.TxInfo ->
  MapInstance ->
  [Ledger.TxOut] ->
  Maybe (Ledger.TxOut, Map)
mapOutput' info mapInstance outputs =
  let filtered = hasOne (unMapInstance mapInstance) `filter` outputs
   in listToMaybe filtered >>= (\fst' -> (fst',) <$> mapDatum @k @v info fst')

{-# INLINEABLE mapSnapshotOutput' #-}
mapSnapshotOutput' ::
  forall k v.
  (FromData k, FromData v) =>
  Ledger.TxInfo ->
  Value.AssetClass ->
  [Ledger.TxOut] ->
  MapSnapshot ->
  Maybe Ledger.TxOut
mapSnapshotOutput' info snapshotAC outputs mapSnapshot =
  let filtered = (\txOut -> hasOne snapshotAC txOut && mapSnapshotDatum @k @v info txOut == Just mapSnapshot) `filter` outputs
   in listToMaybe filtered

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
  let filtered = (\txOut -> hasOne snapshotAC txOut && snapshotPermDatum @k @v info txOut == Just snapshotPerm) `filter` outputs
   in listToMaybe filtered

{-# INLINEABLE nodeSnapshotOutput' #-}
nodeSnapshotOutput' ::
  forall k v.
  (Eq k, Eq v, FromData k, FromData v) =>
  Ledger.TxInfo ->
  [Ledger.TxOut] ->
  SnapshotCS ->
  NodeSnapshot k v ->
  Maybe Ledger.TxOut
nodeSnapshotOutput' info outputs snapshotCS nodeSnapshot =
  let filtered = (\txOut -> (isJust . lookupToken (unSnapshotCS snapshotCS)) txOut && nodeSnapshotDatum @k @v info txOut == Just nodeSnapshot) `filter` outputs
   in listToMaybe filtered

{-# INLINEABLE unlockPermOutput' #-}
unlockPermOutput' ::
  forall k v.
  (FromData k, FromData v) =>
  Ledger.TxInfo ->
  [Ledger.TxOut] ->
  UnlockPermCS ->
  UnlockPerm ->
  Maybe Ledger.TxOut
unlockPermOutput' info outputs unlockPermCS unlockPerm =
  let filtered = (\txOut -> (isJust . lookupToken (unUnlockPermCS unlockPermCS)) txOut && unlockPermDatum @k @v info txOut == Just unlockPerm) `filter` outputs
   in listToMaybe filtered

{-# INLINEABLE unlockOutputs' #-}
unlockOutputs' ::
  forall k v.
  (FromData k, FromData v) =>
  Ledger.TxInfo ->
  [Ledger.TxOut] ->
  UnlockCS ->
  Unlock ->
  [Ledger.TxOut]
unlockOutputs' info outputs unlockCS unlock =
  (\txOut -> (isJust . lookupToken (unUnlockCS unlockCS)) txOut && unlockDatum @k @v info txOut == Just unlock) `filter` outputs

{-# INLINEABLE unlockOutput' #-}
unlockOutput' ::
  forall k v.
  (FromData k, FromData v) =>
  Ledger.TxInfo ->
  [Ledger.TxOut] ->
  UnlockCS ->
  Unlock ->
  Maybe Ledger.TxOut
unlockOutput' info outputs unlockCS unlock =
  let filtered = (\txOut -> (isJust . lookupToken (unUnlockCS unlockCS)) txOut && unlockDatum @k @v info txOut == Just unlock) `filter` outputs
   in listToMaybe filtered

{-# INLINEABLE nodeBy' #-}
nodeBy' ::
  forall k v.
  (FromData k, FromData v) =>
  Ledger.TxInfo ->
  [Ledger.TxOut] ->
  ((Ledger.TxOut, Node k v) -> Bool) ->
  Maybe (Ledger.TxOut, Node k v)
nodeBy' info lookupSet pred' =
  listToMaybe
    [ x
    | txOut <- lookupSet
    , x <- maybeToList $ (txOut,) <$> mapNode @k @v info txOut
    , pred' x
    ]

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
  let filtered = (\txOutRef -> (isJust . lookupToken (unPointerCS pointerCS) . Ledger.txInInfoResolved) txOutRef && matchesRef txOutRef) `filter` inputs
   in listToMaybe filtered >>= (\txInInfo -> (txInInfo,) <$> mapNode info (Ledger.txInInfoResolved txInInfo))
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
  let filtered = (\txOutRef -> (hasOne snapshotAC . Ledger.txInInfoResolved) txOutRef && matchesRef txOutRef) `filter` inputs
   in listToMaybe filtered >>= (\txInInfo -> (txInInfo,) <$> snapshotPermDatum @k @v info (Ledger.txInInfoResolved txInInfo))
  where
    matchesRef :: Ledger.TxInInfo -> Bool
    matchesRef txInInfo = Ledger.txInInfoOutRef txInInfo == ref

{-# INLINEABLE nodeSnapshotInputByRef' #-}
nodeSnapshotInputByRef' ::
  forall k v.
  (FromData k, FromData v) =>
  Ledger.TxInfo ->
  SnapshotCS ->
  [Ledger.TxInInfo] ->
  Ledger.TxOutRef ->
  Maybe (Ledger.TxInInfo, NodeSnapshot k v)
nodeSnapshotInputByRef' info snapshotCS inputs ref =
  let filtered = (\txOutRef -> (isJust . lookupToken (unSnapshotCS snapshotCS) . Ledger.txInInfoResolved) txOutRef && matchesRef txOutRef) `filter` inputs
   in listToMaybe filtered >>= (\txInInfo -> (txInInfo,) <$> nodeSnapshotDatum @k @v info (Ledger.txInInfoResolved txInInfo))
  where
    matchesRef :: Ledger.TxInInfo -> Bool
    matchesRef txInInfo = Ledger.txInInfoOutRef txInInfo == ref

{-# INLINEABLE unlockInputByRef' #-}
unlockInputByRef' ::
  forall k v.
  (FromData k, FromData v) =>
  Ledger.TxInfo ->
  UnlockCS ->
  [Ledger.TxInInfo] ->
  Ledger.TxOutRef ->
  Maybe (Ledger.TxInInfo, Unlock)
unlockInputByRef' info unlockCS inputs ref =
  let filtered = (\txOutRef -> (isJust . lookupToken (unUnlockCS unlockCS) . Ledger.txInInfoResolved) txOutRef && matchesRef txOutRef) `filter` inputs
   in listToMaybe filtered >>= (\txInInfo -> (txInInfo,) <$> unlockDatum @k @v info (Ledger.txInInfoResolved txInInfo))
  where
    matchesRef :: Ledger.TxInInfo -> Bool
    matchesRef txInInfo = Ledger.txInInfoOutRef txInInfo == ref

{-# INLINEABLE unlockPermInputByRef' #-}
unlockPermInputByRef' ::
  forall k v.
  (FromData k, FromData v) =>
  Ledger.TxInfo ->
  UnlockPermCS ->
  [Ledger.TxInInfo] ->
  Ledger.TxOutRef ->
  Maybe (Ledger.TxInInfo, UnlockPerm)
unlockPermInputByRef' info unlockPermCS inputs ref =
  let filtered = (\txOutRef -> (isJust . lookupToken (unUnlockPermCS unlockPermCS) . Ledger.txInInfoResolved) txOutRef && matchesRef txOutRef) `filter` inputs
   in listToMaybe filtered >>= (\txInInfo -> (txInInfo,) <$> unlockPermDatum @k @v info (Ledger.txInInfoResolved txInInfo))
  where
    matchesRef :: Ledger.TxInInfo -> Bool
    matchesRef txInInfo = Ledger.txInInfoOutRef txInInfo == ref

{-# INLINEABLE mapSnapshotInputByRef' #-}
mapSnapshotInputByRef' ::
  forall k v.
  (FromData k, FromData v) =>
  Ledger.TxInfo ->
  SnapshotCS ->
  [Ledger.TxInInfo] ->
  Ledger.TxOutRef ->
  Maybe (Ledger.TxInInfo, MapSnapshot)
mapSnapshotInputByRef' info snapshotCS inputs ref =
  let filtered = (\txOutRef -> (isJust . lookupToken (unSnapshotCS snapshotCS) . Ledger.txInInfoResolved) txOutRef && matchesRef txOutRef) `filter` inputs
   in listToMaybe filtered >>= (\txInInfo -> (txInInfo,) <$> mapSnapshotDatum @k @v info (Ledger.txInInfoResolved txInInfo))
  where
    matchesRef :: Ledger.TxInInfo -> Bool
    matchesRef txInInfo = Ledger.txInInfoOutRef txInInfo == ref

inputsAt' :: Ledger.TxInfo -> Ledger.Address -> Integer
inputsAt' info address =
  length $
    ( \txInInfo ->
        Ledger.txOutAddress
          (Ledger.txInInfoResolved txInInfo)
          == address
    )
      `filter` Ledger.txInfoInputs info

outputsAt' :: Ledger.TxInfo -> Ledger.Address -> Integer
outputsAt' info address =
  length $
    (\txOut -> Ledger.txOutAddress txOut == address) `filter` Ledger.txInfoOutputs info

{-# INLINEABLE isUnlocked #-}
isUnlocked :: LockState -> Bool
isUnlocked lockState = case lockState of
  Unlocked -> True
  _ -> False

{-# INLINEABLE isLockedFor #-}
isLockedFor :: LockState -> SnapshotVersion -> Bool
isLockedFor lockState snapshotVersion = case lockState of
  LockedFor v -> v == snapshotVersion
  _ -> False

{-# INLINEABLE isSnapshotDone #-}
isSnapshotDone :: LockState -> SnapshotVersion -> Bool
isSnapshotDone lockState snapshotVersion = case lockState of
  SnapshotDone v -> v == snapshotVersion
  _ -> False

{-# INLINEABLE incrementSV #-}
incrementSV :: T.SnapshotVersion -> T.SnapshotVersion
incrementSV (T.SnapshotVersion v) = T.SnapshotVersion (v + 1)
