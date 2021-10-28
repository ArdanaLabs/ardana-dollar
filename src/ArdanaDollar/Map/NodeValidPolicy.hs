{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module ArdanaDollar.Map.NodeValidPolicy (
  mkNodeValidPolicy,
) where

import Ledger qualified
import Ledger.Value qualified as Value

import PlutusTx.IsData.Class (FromData)
import PlutusTx.Prelude

import ArdanaDollar.Map.TxUtils (
  hasOne,
  lookupToken,
  mapInput',
  mapOutput',
  nodeByKey',
  nodeByPointer',
  nodeInputRef',
  tokenName,
 )
import ArdanaDollar.Map.Types (
  Map,
  MapInstance,
  Node,
  Pointer (Pointer),
  TokenRedeemer (..),
 )
import ArdanaDollar.Map.Types qualified as T

{-# INLINEABLE mkNodeValidPolicy #-}
mkNodeValidPolicy ::
  forall k v.
  (Ord k, Eq v, FromData k, FromData v) =>
  MapInstance ->
  TokenRedeemer ->
  Ledger.ScriptContext ->
  Bool
mkNodeValidPolicy inst redeemer ctx =
  case redeemer of
    ----
    AddToEmptyMap ->
      fromMaybe False $ do
        (inputMap', inputMap) <- mapInput
        (outputMap', outputMap) <- mapOutput
        (newOutput', newOutput) <- T.map'head outputMap >>= nodeOutputByPointer
        let expectedTokenAC = tokenAC $ Ledger.txInInfoOutRef inputMap'
        let expectedAddress = Ledger.txOutAddress . Ledger.txInInfoResolved $ inputMap'
        return
          ( -- required token at inputs
            hasNFT (Ledger.txInInfoResolved inputMap')
              -- outputs at correct address
              && Ledger.txOutAddress outputMap' == expectedAddress
              && Ledger.txOutAddress newOutput' == expectedAddress
              -- input-output validation
              -- -- correct input linking
              && isNothing (T.map'head inputMap)
              -- -- correct output linking
              && T.map'head outputMap == Just (Pointer expectedTokenAC)
              && outputMap `mapPointsTo` newOutput'
              && isNothing (T.node'next newOutput)
              -- -- equality checks wrt Ledger.Value and (key, value) pairs
              && Ledger.txOutValue (Ledger.txInInfoResolved inputMap') == Ledger.txOutValue outputMap'
              -- quantative checks
              && inputsAtAddress expectedAddress == 1
              && outputsAtAddress expectedAddress == 2
              && checkMintedAmount expectedTokenAC 1
          )
    ----
    AddSmallest ->
      fromMaybe False $ do
        (inputMap', inputMap) <- mapInput
        (outputMap', outputMap) <- mapOutput
        (prevInput', prevInput) <- T.map'head inputMap >>= nodeInputByPointer
        (prevOutput', prevOutput) <- nodeOutputByKey (T.node'key prevInput)
        (newOutput', newOutput) <- T.map'head outputMap >>= nodeOutputByPointer
        let expectedTokenAC = tokenAC $ Ledger.txInInfoOutRef inputMap'
        let expectedAddress = Ledger.txOutAddress . Ledger.txInInfoResolved $ inputMap'
        return
          ( -- required token at inputs
            hasNFT (Ledger.txInInfoResolved inputMap')
              && hasUs prevInput'
              -- outputs at correct address
              && Ledger.txOutAddress outputMap' == expectedAddress
              && Ledger.txOutAddress newOutput' == expectedAddress
              && Ledger.txOutAddress prevOutput' == expectedAddress
              -- input-output validation
              -- -- correct input linking
              && inputMap `mapPointsTo` prevInput'
              -- -- correct output linking
              && T.map'head outputMap == Just (Pointer expectedTokenAC)
              && outputMap `mapPointsTo` newOutput'
              && newOutput `nodePointsTo` prevOutput'
              && T.node'key newOutput < T.node'key prevInput
              -- -- equality checks wrt Ledger.Value and (key, value) pairs
              && prevInput == prevOutput
              && Ledger.txOutValue prevInput' == Ledger.txOutValue prevOutput'
              && Ledger.txOutValue (Ledger.txInInfoResolved inputMap') == Ledger.txOutValue outputMap'
              -- quantative checks
              && inputsAtAddress expectedAddress == 2
              && outputsAtAddress expectedAddress == 3
              && checkMintedAmount expectedTokenAC 1
          )
    ----
    AddInTheMiddle prev ->
      fromMaybe False $ do
        (prevInput', prevInput) <- nodeInputByRef prev
        (nextInput', nextInput) <- T.node'next prevInput >>= nodeInputByPointer
        (prevOutput', prevOutput) <- nodeOutputByKey (T.node'key prevInput)
        (nextOutput', nextOutput) <- nodeOutputByKey (T.node'key nextInput)
        (newOutput', newOutput) <- T.node'next prevOutput >>= nodeOutputByPointer
        let expectedTokenAC = tokenAC $ Ledger.txInInfoOutRef prevInput'
        let expectedAddress = Ledger.txOutAddress . Ledger.txInInfoResolved $ prevInput'
        return
          ( -- required token at inputs
            hasUs (Ledger.txInInfoResolved prevInput')
              && hasUs nextInput'
              -- outputs at correct address
              && Ledger.txOutAddress prevOutput' == expectedAddress
              && Ledger.txOutAddress nextOutput' == expectedAddress
              && Ledger.txOutAddress newOutput' == expectedAddress
              -- input-output validation
              -- -- correct input linking
              && prevInput `nodePointsTo` nextInput'
              -- -- correct output linking
              && T.node'next prevOutput == Just (Pointer expectedTokenAC)
              && prevOutput `nodePointsTo` newOutput'
              && newOutput `nodePointsTo` nextOutput'
              && T.node'key prevOutput < T.node'key newOutput
              && T.node'key newOutput < T.node'key nextOutput
              -- -- equality checks wrt Ledger.Value and (key, value) pairs
              && prevInput{node'next = T.node'next prevOutput} == prevOutput
              && Ledger.txOutValue (Ledger.txInInfoResolved prevInput') == Ledger.txOutValue prevOutput'
              && nextInput == nextOutput
              && Ledger.txOutValue nextInput' == Ledger.txOutValue nextOutput'
              -- quantative checks
              && inputsAtAddress expectedAddress == 2
              && outputsAtAddress expectedAddress == 3
              && checkMintedAmount expectedTokenAC 1
          )
    ----
    AddGreatest prev ->
      fromMaybe False $ do
        (prevInput', prevInput) <- nodeInputByRef prev
        (prevOutput', prevOutput) <- nodeOutputByKey (T.node'key prevInput)
        (newOutput', newOutput) <- T.node'next prevOutput >>= nodeOutputByPointer
        let expectedTokenAC = tokenAC $ Ledger.txInInfoOutRef prevInput'
        let expectedAddress = Ledger.txOutAddress . Ledger.txInInfoResolved $ prevInput'
        return
          ( -- required token at inputs
            hasUs (Ledger.txInInfoResolved prevInput')
              -- outputs at correct address
              && Ledger.txOutAddress prevOutput' == expectedAddress
              && Ledger.txOutAddress newOutput' == expectedAddress
              -- input-output validation
              -- -- correct input linking
              && isNothing (T.node'next prevInput)
              -- -- correct output linking
              && T.node'next prevOutput == Just (Pointer expectedTokenAC)
              && prevOutput `nodePointsTo` newOutput'
              && isNothing (T.node'next newOutput)
              && T.node'key prevOutput < T.node'key newOutput
              -- -- equality checks wrt Ledger.Value and (key, value) pairs
              && prevInput{node'next = T.node'next prevOutput} == prevOutput
              && Ledger.txOutValue (Ledger.txInInfoResolved prevInput') == Ledger.txOutValue prevOutput'
              -- quantative checks
              && inputsAtAddress expectedAddress == 1
              && outputsAtAddress expectedAddress == 2
              && checkMintedAmount expectedTokenAC 1
          )
    ----
    RemoveFromOneElementMap ->
      fromMaybe False $ do
        (inputMap', inputMap) <- mapInput
        (prevInput', prevInput) <- T.map'head inputMap >>= nodeInputByPointer
        (outputMap', outputMap) <- mapOutput
        burntPointer <- T.map'head inputMap
        let expectedAddress = Ledger.txOutAddress . Ledger.txInInfoResolved $ inputMap'
        return
          ( -- required token at inputs
            hasNFT (Ledger.txInInfoResolved inputMap')
              && hasUs prevInput'
              -- outputs at correct address
              && Ledger.txOutAddress outputMap' == expectedAddress
              -- input-output validation
              -- -- correct input linking
              && inputMap `mapPointsTo` prevInput'
              && isNothing (T.node'next prevInput)
              -- -- correct output linking
              && isNothing (T.map'head outputMap)
              -- -- equality checks wrt Ledger.Value and (key, value) pairs
              && Ledger.txOutValue (Ledger.txInInfoResolved inputMap') == Ledger.txOutValue outputMap'
              -- quantative checks
              && inputsAtAddress expectedAddress == 2
              && outputsAtAddress expectedAddress == 1
              && checkMintedAmount (T.unPointer burntPointer) (-1)
          )
    ----
    RemoveSmallest ->
      fromMaybe False $ do
        (inputMap', inputMap) <- mapInput
        (outputMap', outputMap) <- mapOutput
        (prevInput', prevInput) <- T.map'head inputMap >>= nodeInputByPointer
        (nextInput', nextInput) <- T.node'next prevInput >>= nodeInputByPointer
        (nextOutput', nextOutput) <- nodeOutputByKey (T.node'key nextInput)
        burntPointer <- T.map'head inputMap
        let expectedAddress = Ledger.txOutAddress . Ledger.txInInfoResolved $ inputMap'
        return
          ( -- required token at inputs
            hasNFT (Ledger.txInInfoResolved inputMap')
              && hasUs prevInput'
              && hasUs nextInput'
              -- outputs at correct address
              && Ledger.txOutAddress outputMap' == expectedAddress
              && Ledger.txOutAddress nextOutput' == expectedAddress
              -- input-output validation
              -- -- correct input linking
              && inputMap `mapPointsTo` prevInput'
              && prevInput `nodePointsTo` nextInput'
              -- -- correct output linking
              && outputMap `mapPointsTo` nextOutput'
              -- -- equality checks wrt Ledger.Value and (key, value) pairs
              && nextInput == nextOutput
              && Ledger.txOutValue nextInput' == Ledger.txOutValue nextOutput'
              && Ledger.txOutValue (Ledger.txInInfoResolved inputMap') == Ledger.txOutValue outputMap'
              -- quantative checks
              && inputsAtAddress expectedAddress == 3
              && outputsAtAddress expectedAddress == 2
              && checkMintedAmount (T.unPointer burntPointer) (-1)
          )
    ----
    RemoveInTheMiddle prev ->
      fromMaybe False $ do
        (prevInput', prevInput) <- nodeInputByRef prev
        (currInput', currInput) <- T.node'next prevInput >>= nodeInputByPointer
        (nextInput', nextInput) <- T.node'next currInput >>= nodeInputByPointer
        (prevOutput', prevOutput) <- nodeOutputByKey (T.node'key prevInput)
        (nextOutput', nextOutput) <- nodeOutputByKey (T.node'key nextInput)
        burntPointer <- T.node'next prevInput
        let expectedAddress = Ledger.txOutAddress . Ledger.txInInfoResolved $ prevInput'
        return
          ( -- required token at inputs
            hasUs (Ledger.txInInfoResolved prevInput')
              && hasUs currInput'
              && hasUs nextInput'
              -- outputs at correct address
              && Ledger.txOutAddress prevOutput' == expectedAddress
              && Ledger.txOutAddress nextOutput' == expectedAddress
              -- input-output validation
              -- -- correct input linking
              && prevInput `nodePointsTo` currInput'
              && currInput `nodePointsTo` nextInput'
              -- -- correct output linking
              && prevOutput `nodePointsTo` nextOutput'
              -- -- equality checks wrt Ledger.Value and (key, value) pairs
              && prevInput{node'next = T.node'next prevOutput} == prevOutput
              && Ledger.txOutValue (Ledger.txInInfoResolved prevInput') == Ledger.txOutValue prevOutput'
              && nextInput == nextOutput
              && Ledger.txOutValue nextInput' == Ledger.txOutValue nextOutput'
              -- quantative checks
              && inputsAtAddress expectedAddress == 3
              && outputsAtAddress expectedAddress == 2
              && checkMintedAmount (T.unPointer burntPointer) (-1)
          )
    ----
    RemoveGreatest prev ->
      fromMaybe False $ do
        (prevInput', prevInput) <- nodeInputByRef prev
        (nextInput', nextInput) <- T.node'next prevInput >>= nodeInputByPointer
        (prevOutput', prevOutput) <- nodeOutputByKey (T.node'key prevInput)
        burntPointer <- T.node'next prevInput
        let expectedAddress = Ledger.txOutAddress . Ledger.txInInfoResolved $ prevInput'
        return
          ( -- required token at inputs
            hasUs (Ledger.txInInfoResolved prevInput')
              && hasUs nextInput'
              -- outputs at correct address
              && Ledger.txOutAddress prevOutput' == expectedAddress
              -- input-output validation
              -- -- correct input linking
              && prevInput `nodePointsTo` nextInput'
              && isNothing (T.node'next nextInput)
              -- -- correct output linking
              && isNothing (T.node'next prevOutput)
              -- -- equality checks wrt Ledger.Value and (key, value) pairs
              && prevInput{node'next = T.node'next prevOutput} == prevOutput
              && Ledger.txOutValue (Ledger.txInInfoResolved prevInput') == Ledger.txOutValue prevOutput'
              -- quantative checks
              && inputsAtAddress expectedAddress == 2
              && outputsAtAddress expectedAddress == 1
              && checkMintedAmount (T.unPointer burntPointer) (-1)
          )
  where
    info :: Ledger.TxInfo
    info = Ledger.scriptContextTxInfo ctx

    tokenAC :: Ledger.TxOutRef -> Ledger.AssetClass
    tokenAC ref =
      Value.assetClass
        (Ledger.ownCurrencySymbol ctx)
        (tokenName ref)

    hasUs :: Ledger.TxOut -> Bool
    hasUs txOut = isJust $ lookupToken (Ledger.ownCurrencySymbol ctx) txOut

    mapInput :: Maybe (Ledger.TxInInfo, Map)
    mapInput = mapInput' @k @v info (Ledger.txInfoInputs info)

    mapOutput :: Maybe (Ledger.TxOut, Map)
    mapOutput = mapOutput' @k @v info (Ledger.txInfoOutputs info)

    nodeOutputByKey :: k -> Maybe (Ledger.TxOut, Node k v)
    nodeOutputByKey = nodeByKey' info (Ledger.txInfoOutputs info)

    nodeOutputByPointer :: Pointer -> Maybe (Ledger.TxOut, Node k v)
    nodeOutputByPointer = nodeByPointer' info (Ledger.txInfoOutputs info)

    nodeInputByPointer :: Pointer -> Maybe (Ledger.TxOut, Node k v)
    nodeInputByPointer = nodeByPointer' info (Ledger.txInInfoResolved <$> Ledger.txInfoInputs info)

    nodeInputByRef :: Ledger.TxOutRef -> Maybe (Ledger.TxInInfo, Node k v)
    nodeInputByRef = nodeInputRef' info (Ledger.txInfoInputs info)

    hasNFT :: Ledger.TxOut -> Bool
    hasNFT txOut = hasOne (T.unMapInstance inst) txOut

    inputsAtAddress :: Ledger.Address -> Integer
    inputsAtAddress address =
      length $
        ( \txInInfo ->
            Ledger.txOutAddress
              (Ledger.txInInfoResolved txInInfo)
              == address
        )
          `filter` Ledger.txInfoInputs info

    outputsAtAddress :: Ledger.Address -> Integer
    outputsAtAddress address =
      length $
        (\txOut -> Ledger.txOutAddress txOut == address) `filter` Ledger.txInfoOutputs info

    mapPointsTo :: Map -> Ledger.TxOut -> Bool
    mapPointsTo map' txOut =
      maybe
        False
        (\pointer -> Value.assetClassValueOf (Ledger.txOutValue txOut) (T.unPointer pointer) == 1)
        (T.map'head map')

    nodePointsTo :: Node k v -> Ledger.TxOut -> Bool
    nodePointsTo node txOut =
      maybe
        False
        (\pointer -> Value.assetClassValueOf (Ledger.txOutValue txOut) (T.unPointer pointer) == 1)
        (T.node'next node)

    checkMintedAmount :: Ledger.AssetClass -> Integer -> Bool
    checkMintedAmount ac num = Value.assetClassValueOf (Ledger.txInfoMint info) ac == num
