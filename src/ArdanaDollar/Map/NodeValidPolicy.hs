{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-specialise #-}

module ArdanaDollar.Map.NodeValidPolicy (
  mkNodeValidPolicy,
) where

import Ledger qualified
import Ledger.Value qualified as Value

import PlutusTx.AssocMap qualified as M
import PlutusTx.IsData.Class (FromData)
import PlutusTx.Prelude

import ArdanaDollar.Map.TxUtils (
  inputsAt',
  isUnlocked,
  mapInput',
  mapOutput',
  nodeByKey',
  nodeByPointer',
  nodeInputRef',
  outputsAt',
  tokenName,
 )
import ArdanaDollar.Map.Types (
  Map,
  MapInstance,
  Node,
  NodeValidTokenRedeemer (..),
  Pointer (Pointer),
 )
import ArdanaDollar.Map.Types qualified as T

{-# INLINEABLE mkNodeValidPolicy #-}
mkNodeValidPolicy ::
  forall k v.
  (Ord k, Eq v, FromData k, FromData v) =>
  MapInstance ->
  NodeValidTokenRedeemer ->
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
        let expectedPointer = Pointer $ tokenAC $ Ledger.txInInfoOutRef inputMap'
        let mapAddress = Ledger.txOutAddress . Ledger.txInInfoResolved $ inputMap'
        return
          ( -- map not locked
            isUnlocked (T.map'lockState inputMap)
              -- outputs at correct address
              && Ledger.txOutAddress outputMap' == mapAddress
              && Ledger.txOutAddress newOutput' == mapAddress
              -- input-output validation
              -- -- correct input linking
              && isNothing (T.map'head inputMap)
              -- -- correct output linking
              && T.map'head outputMap == Just expectedPointer
              && outputMap `mapPointsTo` newOutput'
              && isNothing (T.node'next newOutput)
              -- -- equality checks wrt Ledger.Value and (key, value) pairs
              && inputMap{T.map'head = T.map'head outputMap} == outputMap
              && Ledger.txOutValue (Ledger.txInInfoResolved inputMap') == Ledger.txOutValue outputMap'
              -- quantative checks
              && inputsAt mapAddress == 1
              && outputsAt mapAddress == 2
              && onlyMintsOne
          )
    ----
    AddSmallest ->
      fromMaybe False $ do
        (inputMap', inputMap) <- mapInput
        (outputMap', outputMap) <- mapOutput
        (prevInput', prevInput) <- T.map'head inputMap >>= nodeInputByPointer
        (prevOutput', prevOutput) <- nodeOutputByKey (T.node'key prevInput)
        (newOutput', newOutput) <- T.map'head outputMap >>= nodeOutputByPointer
        let expectedPointer = Pointer $ tokenAC $ Ledger.txInInfoOutRef inputMap'
        let mapAddress = Ledger.txOutAddress . Ledger.txInInfoResolved $ inputMap'
        return
          ( -- map not locked
            isUnlocked (T.map'lockState inputMap)
              && isUnlocked (T.node'lockState prevInput)
              -- outputs at correct address
              && Ledger.txOutAddress outputMap' == mapAddress
              && Ledger.txOutAddress newOutput' == mapAddress
              && Ledger.txOutAddress prevOutput' == mapAddress
              -- input-output validation
              -- -- correct input linking
              && inputMap `mapPointsTo` prevInput'
              -- -- correct output linking
              && T.map'head outputMap == Just expectedPointer
              && outputMap `mapPointsTo` newOutput'
              && newOutput `nodePointsTo` prevOutput'
              && T.node'key newOutput < T.node'key prevInput
              -- -- equality checks wrt Ledger.Value and (key, value) pairs
              && prevInput == prevOutput
              && Ledger.txOutValue prevInput' == Ledger.txOutValue prevOutput'
              && inputMap{T.map'head = T.map'head outputMap} == outputMap
              && Ledger.txOutValue (Ledger.txInInfoResolved inputMap') == Ledger.txOutValue outputMap'
              -- quantative checks
              && inputsAt mapAddress == 2
              && outputsAt mapAddress == 3
              && onlyMintsOne
          )
    ----
    AddInTheMiddle prev ->
      fromMaybe False $ do
        (prevInput', prevInput) <- nodeInputByRef prev
        (nextInput', nextInput) <- T.node'next prevInput >>= nodeInputByPointer
        (prevOutput', prevOutput) <- nodeOutputByKey (T.node'key prevInput)
        (nextOutput', nextOutput) <- nodeOutputByKey (T.node'key nextInput)
        (newOutput', newOutput) <- T.node'next prevOutput >>= nodeOutputByPointer
        let expectedPointer = Pointer $ tokenAC $ Ledger.txInInfoOutRef prevInput'
        let mapAddress = Ledger.txOutAddress . Ledger.txInInfoResolved $ prevInput'
        return
          ( -- map not locked
            isUnlocked (T.node'lockState prevInput)
              && isUnlocked (T.node'lockState nextInput)
              -- outputs at correct address
              && Ledger.txOutAddress prevOutput' == mapAddress
              && Ledger.txOutAddress nextOutput' == mapAddress
              && Ledger.txOutAddress newOutput' == mapAddress
              -- input-output validation
              -- -- correct input linking
              && prevInput `nodePointsTo` nextInput'
              -- -- correct output linking
              && T.node'next prevOutput == Just expectedPointer
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
              && inputsAt mapAddress == 2
              && outputsAt mapAddress == 3
              && onlyMintsOne
          )
    ----
    AddGreatest prev ->
      fromMaybe False $ do
        (prevInput', prevInput) <- nodeInputByRef prev
        (prevOutput', prevOutput) <- nodeOutputByKey (T.node'key prevInput)
        (newOutput', newOutput) <- T.node'next prevOutput >>= nodeOutputByPointer
        let expectedPointer = Pointer $ tokenAC $ Ledger.txInInfoOutRef prevInput'
        let mapAddress = Ledger.txOutAddress . Ledger.txInInfoResolved $ prevInput'
        return
          ( -- map not locked
            isUnlocked (T.node'lockState prevInput)
              -- outputs at correct address
              && Ledger.txOutAddress prevOutput' == mapAddress
              && Ledger.txOutAddress newOutput' == mapAddress
              -- input-output validation
              -- -- correct input linking
              && isNothing (T.node'next prevInput)
              -- -- correct output linking
              && T.node'next prevOutput == Just expectedPointer
              && prevOutput `nodePointsTo` newOutput'
              && isNothing (T.node'next newOutput)
              && T.node'key prevOutput < T.node'key newOutput
              -- -- equality checks wrt Ledger.Value and (key, value) pairs
              && prevInput{node'next = T.node'next prevOutput} == prevOutput
              && Ledger.txOutValue (Ledger.txInInfoResolved prevInput') == Ledger.txOutValue prevOutput'
              -- quantative checks
              && inputsAt mapAddress == 1
              && outputsAt mapAddress == 2
              && onlyMintsOne
          )
    ----
    RemoveFromOneElementMap ->
      fromMaybe False $ do
        (inputMap', inputMap) <- mapInput
        (outputMap', outputMap) <- mapOutput
        (prevInput', prevInput) <- T.map'head inputMap >>= nodeInputByPointer
        let mapAddress = Ledger.txOutAddress . Ledger.txInInfoResolved $ inputMap'
        return
          ( -- map not locked
            isUnlocked (T.map'lockState inputMap)
              -- outputs at correct address
              && Ledger.txOutAddress outputMap' == mapAddress
              -- input-output validation
              -- -- correct input linking
              && inputMap `mapPointsTo` prevInput'
              && isNothing (T.node'next prevInput)
              -- -- correct output linking
              && isNothing (T.map'head outputMap)
              -- -- equality checks wrt Ledger.Value and (key, value) pairs
              && inputMap{T.map'head = T.map'head outputMap} == outputMap
              && Ledger.txOutValue (Ledger.txInInfoResolved inputMap') == Ledger.txOutValue outputMap'
              -- quantative checks
              && inputsAt mapAddress == 2
              && outputsAt mapAddress == 1
              && onlyBurnsOne
          )
    ----
    RemoveSmallest ->
      fromMaybe False $ do
        (inputMap', inputMap) <- mapInput
        (outputMap', outputMap) <- mapOutput
        (prevInput', prevInput) <- T.map'head inputMap >>= nodeInputByPointer
        (nextInput', nextInput) <- T.node'next prevInput >>= nodeInputByPointer
        (nextOutput', nextOutput) <- nodeOutputByKey (T.node'key nextInput)
        let mapAddress = Ledger.txOutAddress . Ledger.txInInfoResolved $ inputMap'
        return
          ( -- map not locked
            isUnlocked (T.map'lockState inputMap)
              && isUnlocked (T.node'lockState prevInput)
              -- outputs at correct address
              && Ledger.txOutAddress outputMap' == mapAddress
              && Ledger.txOutAddress nextOutput' == mapAddress
              -- input-output validation
              -- -- correct input linking
              && inputMap `mapPointsTo` prevInput'
              && prevInput `nodePointsTo` nextInput'
              -- -- correct output linking
              && outputMap `mapPointsTo` nextOutput'
              -- -- equality checks wrt Ledger.Value and (key, value) pairs
              && nextInput == nextOutput
              && Ledger.txOutValue nextInput' == Ledger.txOutValue nextOutput'
              && inputMap{T.map'head = T.map'head outputMap} == outputMap
              && Ledger.txOutValue (Ledger.txInInfoResolved inputMap') == Ledger.txOutValue outputMap'
              -- quantative checks
              && inputsAt mapAddress == 3
              && outputsAt mapAddress == 2
              && onlyBurnsOne
          )
    ----
    RemoveInTheMiddle prev ->
      fromMaybe False $ do
        (prevInput', prevInput) <- nodeInputByRef prev
        (currInput', currInput) <- T.node'next prevInput >>= nodeInputByPointer
        (nextInput', nextInput) <- T.node'next currInput >>= nodeInputByPointer
        (prevOutput', prevOutput) <- nodeOutputByKey (T.node'key prevInput)
        (nextOutput', nextOutput) <- nodeOutputByKey (T.node'key nextInput)
        let mapAddress = Ledger.txOutAddress . Ledger.txInInfoResolved $ prevInput'
        return
          ( -- map not locked
            isUnlocked (T.node'lockState prevInput)
              && isUnlocked (T.node'lockState nextInput)
              -- outputs at correct address
              && Ledger.txOutAddress prevOutput' == mapAddress
              && Ledger.txOutAddress nextOutput' == mapAddress
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
              && inputsAt mapAddress == 3
              && outputsAt mapAddress == 2
              && onlyBurnsOne
          )
    ----
    RemoveGreatest prev ->
      fromMaybe False $ do
        (prevInput', prevInput) <- nodeInputByRef prev
        (nextInput', nextInput) <- T.node'next prevInput >>= nodeInputByPointer
        (prevOutput', prevOutput) <- nodeOutputByKey (T.node'key prevInput)
        let mapAddress = Ledger.txOutAddress . Ledger.txInInfoResolved $ prevInput'
        return
          ( -- map not locked
            isUnlocked (T.node'lockState prevInput)
              -- outputs at correct address
              && Ledger.txOutAddress prevOutput' == mapAddress
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
              && inputsAt mapAddress == 2
              && outputsAt mapAddress == 1
              && onlyBurnsOne
          )
  where
    info :: Ledger.TxInfo
    info = Ledger.scriptContextTxInfo ctx

    tokenAC :: Ledger.TxOutRef -> Ledger.AssetClass
    tokenAC ref =
      Value.assetClass
        (Ledger.ownCurrencySymbol ctx)
        (tokenName ref)

    ownPointerCS :: T.PointerCS
    ownPointerCS = T.PointerCS (Ledger.ownCurrencySymbol ctx)

    mapInput :: Maybe (Ledger.TxInInfo, Map)
    mapInput = mapInput' @k @v info inst (Ledger.txInfoInputs info)

    mapOutput :: Maybe (Ledger.TxOut, Map)
    mapOutput = mapOutput' @k @v info inst (Ledger.txInfoOutputs info)

    nodeOutputByKey :: k -> Maybe (Ledger.TxOut, Node k v)
    nodeOutputByKey = nodeByKey' info ownPointerCS (Ledger.txInfoOutputs info)

    nodeOutputByPointer :: Pointer -> Maybe (Ledger.TxOut, Node k v)
    nodeOutputByPointer = nodeByPointer' info ownPointerCS (Ledger.txInfoOutputs info)

    nodeInputByPointer :: Pointer -> Maybe (Ledger.TxOut, Node k v)
    nodeInputByPointer = nodeByPointer' info ownPointerCS (Ledger.txInInfoResolved <$> Ledger.txInfoInputs info)

    nodeInputByRef :: Ledger.TxOutRef -> Maybe (Ledger.TxInInfo, Node k v)
    nodeInputByRef = nodeInputRef' info ownPointerCS (Ledger.txInfoInputs info)

    inputsAt :: Ledger.Address -> Integer
    inputsAt = inputsAt' info

    outputsAt :: Ledger.Address -> Integer
    outputsAt = outputsAt' info

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

    expectMinting :: Integer -> Bool
    expectMinting expected =
      let cs = Ledger.ownCurrencySymbol ctx
          minted = Ledger.txInfoMint info
          tokens = foldMap M.toList (M.lookup cs (Value.getValue minted))
       in case tokens of
            [(_, amt)] -> amt == expected
            _ -> False

    onlyMintsOne :: Bool
    onlyMintsOne = expectMinting 1

    onlyBurnsOne :: Bool
    onlyBurnsOne = expectMinting (-1)
