{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module ArdanaDollar.Map.Validators (
  mkValidator,
  mkNodeValidPolicy,
  tokenName,
  hasOne,
  hasOne',
  hasToken,
  hasToken',
) where

import Ledger qualified
import Ledger.Value qualified as Value
import Plutus.V1.Ledger.Value qualified as Ledger
import PlutusTx.Prelude

import Data.Maybe (maybeToList)

import ArdanaDollar.Map.Types
import ArdanaDollar.Utils (datumForOnchain)

{-# INLINEABLE tokenName #-}
tokenName :: Ledger.TxOutRef -> Ledger.TokenName
tokenName ref =
  Ledger.TokenName $ consByteString (Ledger.txOutRefIdx ref) $ Ledger.getTxId (Ledger.txOutRefId ref)

{-# INLINEABLE singleList #-}
singleList :: [a] -> Maybe a
singleList l = case l of
  [a] -> Just a
  _ -> Nothing

{-# INLINEABLE mapDatum #-}
mapDatum :: Ledger.TxInfo -> Ledger.TxOut -> Maybe Map
mapDatum info txOut = case datumForOnchain @Datum info txOut of
  Just (MapDatum dat) -> Just dat
  _ -> Nothing

{-# INLINEABLE mapNode #-}
mapNode :: Ledger.TxInfo -> Ledger.TxOut -> Maybe Node
mapNode info txOut = case datumForOnchain @Datum info txOut of
  Just (NodeDatum dat) -> Just dat
  _ -> Nothing

{-# INLINEABLE hasOne' #-}
hasOne' :: Ledger.AssetClass -> Value.Value -> Bool
hasOne' ac value = Value.assetClassValueOf value ac == 1

{-# INLINEABLE hasOne #-}
hasOne :: Ledger.AssetClass -> Ledger.TxOut -> Bool
hasOne ac txOut = hasOne' ac (Ledger.txOutValue txOut)

{-# INLINEABLE hasToken' #-}
hasToken' :: Ledger.CurrencySymbol -> Value.Value -> Maybe Ledger.AssetClass
hasToken' expected value =
  let flattened = Value.flattenValue value
      f = (\(cs, _, _) -> cs == expected) `filter` flattened
   in case f of
        [(cs, tn, amt)] | amt == 1 -> Just $ Value.assetClass cs tn
        _ -> Nothing

{-# INLINEABLE hasToken #-}
hasToken :: Ledger.CurrencySymbol -> Ledger.TxOut -> Maybe Ledger.AssetClass
hasToken expected txOut = hasToken' expected (Ledger.txOutValue txOut)

{-# INLINEABLE mapInput' #-}
mapInput' :: Ledger.TxInfo -> [Ledger.TxInInfo] -> Maybe (Ledger.TxInInfo, Map)
mapInput' info inputs =
  let l = inputs >>= \txOut -> maybeToList ((txOut,) <$> mapDatum info (Ledger.txInInfoResolved txOut))
   in singleList l

{-# INLINEABLE mapOutput' #-}
mapOutput' :: Ledger.TxInfo -> [Ledger.TxOut] -> Maybe (Ledger.TxOut, Map)
mapOutput' info outputs =
  let l = outputs >>= \txOut -> maybeToList ((txOut,) <$> mapDatum info txOut)
   in singleList l

{-# INLINEABLE nodeOutput' #-}
nodeOutput' :: Ledger.TxInfo -> [Ledger.TxOut] -> Key -> Maybe (Ledger.TxOut, Node)
nodeOutput' info outputs key =
  let l = outputs >>= \txOut -> maybeToList ((txOut,) <$> mapNode info txOut)
      ll = (matchesKey . snd) `filter` l
   in singleList ll
  where
    matchesKey :: Node -> Bool
    matchesKey node = node'key node == key

{-# INLINEABLE nodeInputRef' #-}
nodeInputRef' :: Ledger.TxInfo -> [Ledger.TxInInfo] -> Ledger.TxOutRef -> Maybe (Ledger.TxInInfo, Node)
nodeInputRef' info inputs ref =
  let l = inputs >>= \txInInfo -> maybeToList ((txInInfo,) <$> mapNode info (Ledger.txInInfoResolved txInInfo))
      ll = (matchesRef . fst) `filter` l
   in singleList ll
  where
    matchesRef :: Ledger.TxInInfo -> Bool
    matchesRef txInInfo = Ledger.txInInfoOutRef txInInfo == ref

{-# INLINEABLE mkValidator #-}
mkValidator ::
  MapInstance ->
  PointerCS ->
  Datum ->
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
    mapOutput = mapOutput' info (Ledger.getContinuingOutputs ctx)

    nodeOutput :: Key -> Maybe (Ledger.TxOut, Node)
    nodeOutput = nodeOutput' info (Ledger.getContinuingOutputs ctx)

    hasNFT :: Ledger.TxOut -> Bool
    hasNFT txOut = hasOne (unMapInstance inst) txOut

    inputHasNFT :: Bool
    inputHasNFT = ownInputSatisfies hasNFT

    outputHasNFT :: Bool
    outputHasNFT = maybe False hasNFT (fst <$> mapOutput)

    inputToken :: Maybe Ledger.AssetClass
    inputToken = Ledger.findOwnInput ctx >>= hasToken (unPointerCS pointerCS) . Ledger.txInInfoResolved

    outputToken :: Key -> Maybe Ledger.AssetClass
    outputToken key = nodeOutput key >>= hasToken (unPointerCS pointerCS) . fst

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
        return (i == o && inputNode{node'value = node'value outputNode} == outputNode)

    burnsXorMintsOneToken :: Bool
    burnsXorMintsOneToken =
      let flattened = Value.flattenValue (Ledger.txInfoMint info)
          f = (\(cs, _, _) -> cs == unPointerCS pointerCS) `filter` flattened
       in case f of
            [(_, _, amt)] -> amt == 1 || amt == -1
            _ -> False

{-# INLINEABLE mkNodeValidPolicy #-}
mkNodeValidPolicy :: MapInstance -> TokenRedeemer -> Ledger.ScriptContext -> Bool
mkNodeValidPolicy inst redeemer ctx =
  case redeemer of
    ----
    AddToEmptyMap key ->
      fromMaybe False $ do
        (inputMap', inputMap) <- mapInput
        (outputMap', outputMap) <- mapOutput
        (outputNode', outputNode) <- nodeOutput key
        let expectedTokenAC = tokenAC $ Ledger.txInInfoOutRef inputMap'
        let expectedAddress = Ledger.txOutAddress . Ledger.txInInfoResolved $ inputMap'
        return
          ( hasNFT (Ledger.txInInfoResolved inputMap')
              && hasNFT outputMap'
              && Ledger.txOutAddress outputMap' == expectedAddress
              && Ledger.txOutAddress outputNode' == expectedAddress
              && isNothing (map'head inputMap)
              && map'head outputMap == Just (Pointer expectedTokenAC)
              && outputMap `mapPointsTo` outputNode'
              && isNothing (node'next outputNode)
              && inputsAtAddress expectedAddress == 1
              && outputsAtAddress expectedAddress == 2
              && checkMintedAmount expectedTokenAC 1
          )
    ----
    AddSmallest key smallest ->
      fromMaybe False $ do
        (inputMap', inputMap) <- mapInput
        (outputMap', outputMap) <- mapOutput
        (outputNode', outputNode) <- nodeOutput key
        (oldNode', oldNode) <- nodeInputRef smallest
        (propagatedNode', propagatedNode) <- nodeOutput (node'key oldNode)
        let expectedTokenAC = tokenAC $ Ledger.txInInfoOutRef inputMap'
        let expectedAddress = Ledger.txOutAddress . Ledger.txInInfoResolved $ inputMap'
        return
          ( hasNFT (Ledger.txInInfoResolved inputMap')
              && hasNFT outputMap'
              && Ledger.txOutAddress outputMap' == expectedAddress
              && Ledger.txOutAddress outputNode' == expectedAddress
              && Ledger.txOutAddress propagatedNode' == expectedAddress
              && oldNode == propagatedNode
              && Ledger.txOutValue (Ledger.txInInfoResolved oldNode') == Ledger.txOutValue propagatedNode'
              && map'head outputMap == Just (Pointer expectedTokenAC)
              && inputMap `mapPointsTo` Ledger.txInInfoResolved oldNode'
              && outputMap `mapPointsTo` outputNode'
              && outputNode `nodePointsTo` propagatedNode'
              && node'key outputNode < node'key oldNode
              && inputsAtAddress expectedAddress == 2
              && outputsAtAddress expectedAddress == 3
              && checkMintedAmount expectedTokenAC 1
          )
    ----
    AddInTheMiddle key prev next ->
      fromMaybe False $ do
        (prevInput', prevInput) <- nodeInputRef prev
        (nextInput', nextInput) <- nodeInputRef next
        (prevOutput', prevOutput) <- nodeOutput (node'key prevInput)
        (nextOutput', nextOutput) <- nodeOutput (node'key nextInput)
        (newOutput', newOutput) <- nodeOutput key
        let expectedTokenAC = tokenAC $ Ledger.txInInfoOutRef prevInput'
        let expectedAddress = Ledger.txOutAddress . Ledger.txInInfoResolved $ prevInput'
        return
          ( hasUs (Ledger.txInInfoResolved prevInput')
              && hasUs (Ledger.txInInfoResolved nextInput')
              && prevInput `nodePointsTo` Ledger.txInInfoResolved nextInput'
              && Ledger.txOutAddress prevOutput' == expectedAddress
              && Ledger.txOutAddress nextOutput' == expectedAddress
              && Ledger.txOutAddress newOutput' == expectedAddress
              && nextInput == nextOutput
              && Ledger.txOutValue (Ledger.txInInfoResolved nextInput') == Ledger.txOutValue nextOutput'
              && prevInput{node'next = node'next prevOutput} == prevOutput
              && Ledger.txOutValue (Ledger.txInInfoResolved prevInput') == Ledger.txOutValue prevOutput'
              && prevOutput `nodePointsTo` newOutput'
              && newOutput `nodePointsTo` nextOutput'
              && node'next prevOutput == Just (Pointer expectedTokenAC)
              && node'key prevOutput < node'key newOutput
              && node'key newOutput < node'key nextOutput
              && inputsAtAddress expectedAddress == 2
              && outputsAtAddress expectedAddress == 3
              && checkMintedAmount expectedTokenAC 1
          )
    ----
    AddGreatest key prev ->
      fromMaybe False $ do
        (prevInput', prevInput) <- nodeInputRef prev
        (prevOutput', prevOutput) <- nodeOutput (node'key prevInput)
        (newOutput', newOutput) <- nodeOutput key
        let expectedTokenAC = tokenAC $ Ledger.txInInfoOutRef prevInput'
        let expectedAddress = Ledger.txOutAddress . Ledger.txInInfoResolved $ prevInput'
        return
          ( hasUs (Ledger.txInInfoResolved prevInput')
              && Ledger.txOutAddress prevOutput' == expectedAddress
              && Ledger.txOutAddress newOutput' == expectedAddress
              && prevInput{node'next = node'next prevOutput} == prevOutput
              && Ledger.txOutValue (Ledger.txInInfoResolved prevInput') == Ledger.txOutValue prevOutput'
              && prevOutput `nodePointsTo` newOutput'
              && node'next prevOutput == Just (Pointer expectedTokenAC)
              && node'key prevOutput < node'key newOutput
              && inputsAtAddress expectedAddress == 1
              && outputsAtAddress expectedAddress == 2
              && checkMintedAmount expectedTokenAC 1
          )
    ----
    RemoveFromOneElementMap smallest ->
      fromMaybe False $ do
        (inputMap', inputMap) <- mapInput
        (outputMap', outputMap) <- mapOutput
        (oldNode', oldNode) <- nodeInputRef smallest
        burntPointer <- map'head inputMap
        let expectedAddress = Ledger.txOutAddress . Ledger.txInInfoResolved $ inputMap'
        return
          ( hasNFT (Ledger.txInInfoResolved inputMap')
              && hasNFT outputMap'
              && Ledger.txOutAddress outputMap' == expectedAddress
              && inputMap `mapPointsTo` Ledger.txInInfoResolved oldNode'
              && isNothing (map'head outputMap)
              && isNothing (node'next oldNode)
              && inputsAtAddress expectedAddress == 2
              && outputsAtAddress expectedAddress == 1
              && checkMintedAmount (unPointer burntPointer) (-1)
          )
    ----
    RemoveSmallest prev next ->
      fromMaybe False $ do
        (inputMap', inputMap) <- mapInput
        (outputMap', outputMap) <- mapOutput
        (prevInput', prevInput) <- nodeInputRef prev
        (nextInput', nextInput) <- nodeInputRef next
        (nextOutput', nextOutput) <- nodeOutput (node'key nextInput)
        burntPointer <- map'head inputMap
        let expectedAddress = Ledger.txOutAddress . Ledger.txInInfoResolved $ inputMap'
        return
          ( hasNFT (Ledger.txInInfoResolved inputMap')
              && hasNFT outputMap'
              && hasUs (Ledger.txInInfoResolved prevInput')
              && hasUs (Ledger.txInInfoResolved nextInput')
              && Ledger.txOutAddress outputMap' == expectedAddress
              && Ledger.txOutAddress nextOutput' == expectedAddress
              && nextInput == nextOutput
              && Ledger.txOutValue (Ledger.txInInfoResolved nextInput') == Ledger.txOutValue nextOutput'
              && inputMap `mapPointsTo` Ledger.txInInfoResolved prevInput'
              && prevInput `nodePointsTo` Ledger.txInInfoResolved nextInput'
              && outputMap `mapPointsTo` nextOutput'
              && inputsAtAddress expectedAddress == 3
              && outputsAtAddress expectedAddress == 2
              && checkMintedAmount (unPointer burntPointer) (-1)
          )
    ----
    RemoveInTheMiddle prev curr next ->
      fromMaybe False $ do
        (prevInput', prevInput) <- nodeInputRef prev
        (currInput', currInput) <- nodeInputRef curr
        (nextInput', nextInput) <- nodeInputRef next
        (prevOutput', prevOutput) <- nodeOutput (node'key prevInput)
        (nextOutput', nextOutput) <- nodeOutput (node'key nextInput)
        burntPointer <- node'next prevInput
        let expectedAddress = Ledger.txOutAddress . Ledger.txInInfoResolved $ prevInput'
        return
          ( hasUs (Ledger.txInInfoResolved prevInput')
              && hasUs (Ledger.txInInfoResolved currInput')
              && hasUs (Ledger.txInInfoResolved nextInput')
              && Ledger.txOutAddress prevOutput' == expectedAddress
              && Ledger.txOutAddress nextOutput' == expectedAddress
              && nextInput == nextOutput
              && Ledger.txOutValue (Ledger.txInInfoResolved nextInput') == Ledger.txOutValue nextOutput'
              && prevInput{node'next = node'next prevOutput} == prevOutput
              && Ledger.txOutValue (Ledger.txInInfoResolved prevInput') == Ledger.txOutValue prevOutput'
              && prevInput `nodePointsTo` Ledger.txInInfoResolved currInput'
              && currInput `nodePointsTo` Ledger.txInInfoResolved nextInput'
              && prevOutput `nodePointsTo` nextOutput'
              && inputsAtAddress expectedAddress == 3
              && outputsAtAddress expectedAddress == 2
              && checkMintedAmount (unPointer burntPointer) (-1)
          )
    ----
    RemoveGreatest prev next ->
      fromMaybe False $ do
        (prevInput', prevInput) <- nodeInputRef prev
        (nextInput', nextInput) <- nodeInputRef next
        (prevOutput', prevOutput) <- nodeOutput (node'key prevInput)
        burntPointer <- node'next prevInput
        let expectedAddress = Ledger.txOutAddress . Ledger.txInInfoResolved $ prevInput'
        return
          ( hasUs (Ledger.txInInfoResolved prevInput')
              && hasUs (Ledger.txInInfoResolved nextInput')
              && Ledger.txOutAddress prevOutput' == expectedAddress
              && prevInput{node'next = Nothing} == prevOutput
              && Ledger.txOutValue (Ledger.txInInfoResolved prevInput') == Ledger.txOutValue prevOutput'
              && prevInput `nodePointsTo` Ledger.txInInfoResolved nextInput'
              && isNothing (node'next nextInput)
              && isNothing (node'next prevOutput) -- redundant?
              && inputsAtAddress expectedAddress == 2
              && outputsAtAddress expectedAddress == 1
              && checkMintedAmount (unPointer burntPointer) (-1)
          )
  where
    info :: Ledger.TxInfo
    info = Ledger.scriptContextTxInfo ctx

    tokenAC :: Ledger.TxOutRef -> Ledger.AssetClass
    tokenAC ref =
      Ledger.assetClass
        (Ledger.ownCurrencySymbol ctx)
        (tokenName ref)

    hasUs :: Ledger.TxOut -> Bool
    hasUs txOut = isJust $ hasToken (Ledger.ownCurrencySymbol ctx) txOut

    mapInput :: Maybe (Ledger.TxInInfo, Map)
    mapInput = mapInput' info (Ledger.txInfoInputs info)

    mapOutput :: Maybe (Ledger.TxOut, Map)
    mapOutput = mapOutput' info (Ledger.txInfoOutputs info)

    nodeOutput :: Key -> Maybe (Ledger.TxOut, Node)
    nodeOutput = nodeOutput' info (Ledger.txInfoOutputs info)

    nodeInputRef :: Ledger.TxOutRef -> Maybe (Ledger.TxInInfo, Node)
    nodeInputRef = nodeInputRef' info (Ledger.txInfoInputs info)

    hasNFT :: Ledger.TxOut -> Bool
    hasNFT txOut = hasOne (unMapInstance inst) txOut

    inputsAtAddress :: Ledger.Address -> Integer
    inputsAtAddress address =
      length $
        (\txInInfo -> Ledger.txOutAddress (Ledger.txInInfoResolved txInInfo) == address) `filter` Ledger.txInfoInputs info

    outputsAtAddress :: Ledger.Address -> Integer
    outputsAtAddress address =
      length $
        (\txOut -> Ledger.txOutAddress txOut == address) `filter` Ledger.txInfoOutputs info

    mapPointsTo :: Map -> Ledger.TxOut -> Bool
    mapPointsTo map' txOut =
      maybe
        False
        (\pointer -> Value.assetClassValueOf (Ledger.txOutValue txOut) (unPointer pointer) == 1)
        (map'head map')

    nodePointsTo :: Node -> Ledger.TxOut -> Bool
    nodePointsTo node txOut =
      maybe
        False
        (\pointer -> Value.assetClassValueOf (Ledger.txOutValue txOut) (unPointer pointer) == 1)
        (node'next node)

    checkMintedAmount :: Ledger.AssetClass -> Integer -> Bool
    checkMintedAmount ac num = Value.assetClassValueOf (Ledger.txInfoMint info) ac == num