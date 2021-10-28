{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module ArdanaDollar.Map.TxUtils (
  hasOne,
  hasOne',
  lookupToken,
  lookupToken',
  mapOutput',
  nodeByKey',
  tokenName,
  mapInput',
  nodeByPointer',
  nodeInputRef',
) where

import Ledger qualified
import Ledger.Value qualified as Value

import PlutusTx.IsData.Class (FromData)
import PlutusTx.Prelude

import ArdanaDollar.Map.Types (
  Datum (MapDatum, NodeDatum),
  Map,
  Node,
  Pointer,
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

{-# INLINEABLE singleList #-}
singleList :: [a] -> Maybe a
singleList l = case l of
  [a] -> Just a
  _ -> Nothing

{-# INLINEABLE mapDatum #-}
mapDatum :: forall k v. (FromData k, FromData v) => Ledger.TxInfo -> Ledger.TxOut -> Maybe Map
mapDatum info txOut = case datumForOnchain @(Datum k v) info txOut of
  Just (MapDatum dat) -> Just dat
  _ -> Nothing

{-# INLINEABLE mapNode #-}
mapNode :: forall k v. (FromData k, FromData v) => Ledger.TxInfo -> Ledger.TxOut -> Maybe (Node k v)
mapNode info txOut = case datumForOnchain @(Datum k v) info txOut of
  Just (NodeDatum dat) -> Just dat
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
mapInput' :: forall k v. (FromData k, FromData v) => Ledger.TxInfo -> [Ledger.TxInInfo] -> Maybe (Ledger.TxInInfo, Map)
mapInput' info inputs =
  let l = inputs >>= \txOut -> maybeToList ((txOut,) <$> mapDatum @k @v info (Ledger.txInInfoResolved txOut))
   in singleList l

{-# INLINEABLE mapOutput' #-}
mapOutput' :: forall k v. (FromData k, FromData v) => Ledger.TxInfo -> [Ledger.TxOut] -> Maybe (Ledger.TxOut, Map)
mapOutput' info outputs =
  let l = outputs >>= \txOut -> maybeToList ((txOut,) <$> mapDatum @k @v info txOut)
   in singleList l

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
   in singleList ll

{-# INLINEABLE nodeByKey' #-}
nodeByKey' ::
  forall k v.
  (FromData k, Ord k, FromData v) =>
  Ledger.TxInfo ->
  [Ledger.TxOut] ->
  k ->
  Maybe (Ledger.TxOut, Node k v)
nodeByKey' info lookupSet key =
  nodeBy' info lookupSet (matchesKey . snd)
  where
    matchesKey :: Node k v -> Bool
    matchesKey node = T.node'key node == key

{-# INLINEABLE nodeByPointer' #-}
nodeByPointer' ::
  forall k v.
  (FromData k, FromData v) =>
  Ledger.TxInfo ->
  [Ledger.TxOut] ->
  Pointer ->
  Maybe (Ledger.TxOut, Node k v)
nodeByPointer' info lookupSet pointer =
  nodeBy' info lookupSet (matchesPointer . fst)
  where
    matchesPointer :: Ledger.TxOut -> Bool
    matchesPointer txOut = Value.assetClassValueOf (Ledger.txOutValue txOut) (T.unPointer pointer) == 1

{-# INLINEABLE nodeInputRef' #-}
nodeInputRef' ::
  forall k v.
  (FromData k, FromData v) =>
  Ledger.TxInfo ->
  [Ledger.TxInInfo] ->
  Ledger.TxOutRef ->
  Maybe (Ledger.TxInInfo, Node k v)
nodeInputRef' info inputs ref =
  let l = inputs >>= \txInInfo -> maybeToList ((txInInfo,) <$> mapNode info (Ledger.txInInfoResolved txInInfo))
      ll = (matchesRef . fst) `filter` l
   in singleList ll
  where
    matchesRef :: Ledger.TxInInfo -> Bool
    matchesRef txInInfo = Ledger.txInInfoOutRef txInInfo == ref
