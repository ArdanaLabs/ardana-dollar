{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-specialise #-}

module ArdanaDollar.Map.SnapshotPolicy (
  mkSnapshotPolicy,
) where

import PlutusTx.IsData.Class (FromData)

import Ledger qualified
import Ledger.Value qualified as Value
import PlutusTx.AssocMap qualified as M
import PlutusTx.Prelude

import ArdanaDollar.Map.TxUtils (
  hasOne,
  incrementSV,
  inputsAt',
  isLockedFor,
  isUnlocked,
  lookupToken,
  mapInput',
  mapOutput',
  mapSnapshotOutput',
  nodeByKey',
  nodeInputRef',
  nodeSnapshotOutput',
  outputsAt',
  snapshotPermInputByRef',
  snapshotPermOutput',
 )
import ArdanaDollar.Map.Types (
  LockState (..),
  Map (..),
  MapInstance,
  MapSnapshot (..),
  Node (..),
  NodeSnapshot (..),
  Pointer (..),
  PointerCS (..),
  SnapshotCS (..),
  SnapshotPerm (..),
  SnapshotPointer (..),
  SnapshotTokenRedeemer (..),
  SnapshotVersion (..),
 )
import ArdanaDollar.Map.Types qualified as T

{-# INLINEABLE transformLockState #-}
transformLockState :: LockState -> SnapshotVersion -> Maybe LockState
transformLockState lockState pendingSnapshot =
  case lockState of
    Unlocked -> Just (LockedFor pendingSnapshot)
    r@(LockedFor v) | v == pendingSnapshot -> Just r
    r@(SnapshotDone v) | v == pendingSnapshot -> Just r
    _ -> Nothing

{-# INLINEABLE mkSnapshotPolicy #-}
mkSnapshotPolicy ::
  forall k v.
  (Ord k, Eq v, FromData k, FromData v) =>
  MapInstance ->
  PointerCS ->
  SnapshotTokenRedeemer ->
  Ledger.ScriptContext ->
  Bool
mkSnapshotPolicy inst pointerCS redeemer ctx =
  ----
  case redeemer of
    MakeSnapshotOfEmpty -> fromMaybe False $ do
      (inputMap', inputMap) <- mapInput
      (outputMap', outputMap) <- mapOutput

      let mapAddress = Ledger.txOutAddress . Ledger.txInInfoResolved $ inputMap'
          currSnapshot = T.map'nextVersion inputMap
          mapSnapshot = MapSnapshot (Ledger.txOutValue outputMap') Nothing currSnapshot

      outputMapSnapshot' <- mapSnapshotOutput mapSnapshot

      return
        ( -- map not locked
          isUnlocked (T.map'lockState inputMap)
            -- outputs at correct address
            && Ledger.txOutAddress outputMap' == mapAddress
            && Ledger.txOutAddress outputMapSnapshot' == mapAddress
            -- input-output validation
            -- -- correct input linking
            && isNothing (T.map'head inputMap)
            -- -- correct output linking
            -- -- equality checks wrt Ledger.Value and (key, value) pairs
            && inputMap{T.map'nextVersion = incrementSV currSnapshot} == outputMap
            && Ledger.txOutValue (Ledger.txInInfoResolved inputMap') == Ledger.txOutValue outputMap'
            -- quantative checks
            && inputsAt mapAddress == 1
            && outputsAt mapAddress == 2
            && expectOwnCS (Ledger.txInfoMint info) (snd $ Value.unAssetClass assetClass1, 1)
            -- snapshot specific
            -- -- Map snapshot carries no other tokens
            && Ledger.txOutValue outputMapSnapshot' == Value.assetClassValue assetClass1 1
        )
    ----
    InitiateSnapshot last' -> fromMaybe False $ do
      (inputMap', inputMap) <- mapInput
      (outputMap', outputMap) <- mapOutput

      (lastInput', lastInput) <- nodeInputByRef last'
      (lastOutput', lastOutput) <- nodeOutputByKey (T.node'key lastInput)

      start' <- T.map'head inputMap
      end' <- T.Pointer <$> lookupToken (T.unPointerCS pointerCS) lastOutput'

      let currSnapshot = T.map'nextVersion inputMap
          expectedSnapshotPerm = T.SnapshotPerm start' end' currSnapshot
          mapSnapshot = MapSnapshot (Ledger.txOutValue outputMap') (repackage <$> T.map'head inputMap) currSnapshot

      outputMapSnapshot' <- mapSnapshotOutput mapSnapshot

      outputSnapshotPerm' <- snapshotPermOutput expectedSnapshotPerm

      let mapAddress = Ledger.txOutAddress . Ledger.txInInfoResolved $ inputMap'

      return
        ( -- map not locked
          isUnlocked (T.map'lockState inputMap)
            && isUnlocked (T.node'lockState lastInput)
            -- outputs at correct address
            && Ledger.txOutAddress outputMap' == mapAddress
            && Ledger.txOutAddress lastOutput' == mapAddress
            && Ledger.txOutAddress outputMapSnapshot' == mapAddress
            && Ledger.txOutAddress outputSnapshotPerm' == mapAddress
            -- input-output validation
            -- -- correct input linking
            && isNothing (T.node'next lastInput)
            -- -- correct output linking
            -- -- equality checks wrt Ledger.Value and (key, value) pairs
            && inputMap{T.map'lockState = SnapshotDone currSnapshot, T.map'nextVersion = incrementSV currSnapshot} == outputMap
            && Ledger.txOutValue (Ledger.txInInfoResolved inputMap') == Ledger.txOutValue outputMap'
            && lastInput{T.node'lockState = LockedFor currSnapshot} == lastOutput
            && Ledger.txOutValue (Ledger.txInInfoResolved lastInput') == Ledger.txOutValue lastOutput'
            -- quantative checks
            && inputsAt mapAddress == 2
            && outputsAt mapAddress == 4
            && expectOwnCS (Ledger.txInfoMint info) (snd $ Value.unAssetClass assetClass1, 2)
            -- snapshot specific
            -- -- Map snapshot carries no other tokens
            && Value.assetClassValueOf (Ledger.txOutValue outputMapSnapshot') assetClass1 == 1
            -- -- SnapshotPerm carries no other tokens
            && Value.assetClassValueOf (Ledger.txOutValue outputSnapshotPerm') assetClass1 == 1
        )
    ----
    SplitSnapshot snapshotTokenRef leftNodeRef rightNodeRef -> fromMaybe False $ do
      (leftNodeInput', leftNodeInput) <- nodeInputByRef leftNodeRef
      (leftNodeOutput', leftNodeOutput) <- nodeOutputByKey (T.node'key leftNodeInput)

      (rightNodeInput', rightNodeInput) <- nodeInputByRef rightNodeRef
      (rightNodeOutput', rightNodeOutput) <- nodeOutputByKey (T.node'key rightNodeInput)

      (snapshotPermInput', snapshotPermInput) <- snapshotPermInputByRef snapshotTokenRef

      l <- lookupToken (unPointerCS pointerCS) leftNodeOutput'
      r <- lookupToken (unPointerCS pointerCS) rightNodeOutput'
      let v = snapshotPerm'version snapshotPermInput
          left = SnapshotPerm (snapshotPerm'start snapshotPermInput) (Pointer l) v
          right = SnapshotPerm (Pointer r) (snapshotPerm'end snapshotPermInput) v
      snapshotPermLeftOutput' <- snapshotPermOutput left
      snapshotPermRightOutput' <- snapshotPermOutput right

      let mapAddress = Ledger.txOutAddress . Ledger.txInInfoResolved $ snapshotPermInput'

      newLeftLockState <- transformLockState (T.node'lockState leftNodeInput) v
      newRightLockState <- transformLockState (T.node'lockState rightNodeInput) v

      return
        ( -- outputs at correct address
          Ledger.txOutAddress leftNodeOutput' == mapAddress
            && Ledger.txOutAddress rightNodeOutput' == mapAddress
            && Ledger.txOutAddress snapshotPermLeftOutput' == mapAddress
            && Ledger.txOutAddress snapshotPermRightOutput' == mapAddress
            -- input-output validation
            -- -- correct input linking
            && leftNodeInput `nodePointsTo` Ledger.txInInfoResolved rightNodeInput'
            -- -- correct output linking
            -- -- equality checks wrt Ledger.Value and (key, value) pairs
            && leftNodeInput{T.node'lockState = newLeftLockState} == leftNodeOutput
            && Ledger.txOutValue (Ledger.txInInfoResolved leftNodeInput') == Ledger.txOutValue leftNodeOutput'
            && rightNodeInput{T.node'lockState = newRightLockState} == rightNodeOutput
            && Ledger.txOutValue (Ledger.txInInfoResolved rightNodeInput') == Ledger.txOutValue rightNodeOutput'
            -- quantative checks
            && inputsAt mapAddress == 3
            && outputsAt mapAddress == 4
            && expectOwnCS (Ledger.txInfoMint info) (snd $ Value.unAssetClass assetClass1, 1)
            -- snapshot specific
            -- -- SnapshotPerm carries no other tokens
            && Ledger.txOutValue snapshotPermLeftOutput' == Value.assetClassValue assetClass1 1
            && Ledger.txOutValue snapshotPermRightOutput' == Value.assetClassValue assetClass1 1
        )
    ----
    MakeSnapshot snapshotTokenRef nodeRef -> fromMaybe False $ do
      (nodeInput', nodeInput) <- nodeInputByRef nodeRef
      (nodeOutput', nodeOutput) <- nodeOutputByKey (T.node'key nodeInput)

      (snapshotPerm', snapshotPerm) <- snapshotPermInputByRef snapshotTokenRef

      expectedPointer <- repackage . Pointer <$> lookupToken (unPointerCS pointerCS) nodeOutput'
      let v = T.snapshotPerm'version snapshotPerm
          expectedNodeSnapshot =
            NodeSnapshot
              (Ledger.txOutValue nodeOutput')
              nodeOutput
              (repackage <$> T.node'next nodeOutput)
              v
      nodeSnapshotOutput'' <- nodeSnapshotOutput expectedNodeSnapshot

      let mapAddress = Ledger.txOutAddress . Ledger.txInInfoResolved $ snapshotPerm'
          (burnt, minted) = Value.split (Ledger.txInfoMint info)

      return
        ( -- outputs at correct address
          Ledger.txOutAddress nodeOutput' == mapAddress
            && Ledger.txOutAddress nodeSnapshotOutput'' == mapAddress
            -- input-output validation
            -- -- correct input linking
            -- -- correct output linking
            -- -- equality checks wrt Ledger.Value and (key, value) pairs
            && nodeInput{T.node'lockState = SnapshotDone v} == nodeOutput
            && Ledger.txOutValue (Ledger.txInInfoResolved nodeInput') == Ledger.txOutValue nodeOutput'
            -- quantative checks
            && inputsAt mapAddress == 2
            && outputsAt mapAddress == 2
            && expectOwnCS minted (snd $ Value.unAssetClass $ unSnapshotPointer expectedPointer, 1)
            && expectOwnCS burnt (snd $ Value.unAssetClass assetClass1, 1)
            -- snapshot specific
            -- -- node locked for correct version
            && isLockedFor (T.node'lockState nodeInput) v
            -- -- making snapshot of correct entry
            && T.snapshotPerm'start snapshotPerm == T.snapshotPerm'end snapshotPerm
            && hasOne (unPointer $ T.snapshotPerm'start snapshotPerm) nodeOutput'
            -- -- SnapshotPerm carries no other tokens
            && Ledger.txOutValue nodeSnapshotOutput'' == Value.assetClassValue (unSnapshotPointer expectedPointer) 1
        )
  where
    info :: Ledger.TxInfo
    info = Ledger.scriptContextTxInfo ctx

    mapInput :: Maybe (Ledger.TxInInfo, Map)
    mapInput = mapInput' @k @v info inst (Ledger.txInfoInputs info)

    mapOutput :: Maybe (Ledger.TxOut, Map)
    mapOutput = mapOutput' @k @v info inst (Ledger.txInfoOutputs info)

    mapSnapshotOutput :: MapSnapshot -> Maybe Ledger.TxOut
    mapSnapshotOutput = mapSnapshotOutput' @k @v info assetClass1 (Ledger.txInfoOutputs info)

    nodeOutputByKey :: k -> Maybe (Ledger.TxOut, Node k v)
    nodeOutputByKey = nodeByKey' info pointerCS (Ledger.txInfoOutputs info)

    nodeInputByRef :: Ledger.TxOutRef -> Maybe (Ledger.TxInInfo, Node k v)
    nodeInputByRef = nodeInputRef' info pointerCS (Ledger.txInfoInputs info)

    snapshotPermOutput :: SnapshotPerm -> Maybe Ledger.TxOut
    snapshotPermOutput = snapshotPermOutput' @k @v info assetClass1 (Ledger.txInfoOutputs info)

    snapshotPermInputByRef :: Ledger.TxOutRef -> Maybe (Ledger.TxInInfo, SnapshotPerm)
    snapshotPermInputByRef = snapshotPermInputByRef' @k @v info assetClass1 (Ledger.txInfoInputs info)

    assetClass1 :: Value.AssetClass
    assetClass1 = Value.AssetClass (Ledger.ownCurrencySymbol ctx, Value.TokenName emptyByteString)

    repackage :: Pointer -> SnapshotPointer
    repackage (Pointer (Value.AssetClass (_, tn))) =
      SnapshotPointer (Value.AssetClass (Ledger.ownCurrencySymbol ctx, tn))

    nodePointsTo :: Node k v -> Ledger.TxOut -> Bool
    nodePointsTo node txOut =
      maybe
        False
        (\pointer -> Value.assetClassValueOf (Ledger.txOutValue txOut) (T.unPointer pointer) == 1)
        (T.node'next node)

    nodeSnapshotOutput :: NodeSnapshot k v -> Maybe Ledger.TxOut
    nodeSnapshotOutput = nodeSnapshotOutput' info (Ledger.txInfoOutputs info) (SnapshotCS $ Ledger.ownCurrencySymbol ctx)

    inputsAt :: Ledger.Address -> Integer
    inputsAt = inputsAt' info

    outputsAt :: Ledger.Address -> Integer
    outputsAt = outputsAt' info

    expectOwnCS :: Value.Value -> (Value.TokenName, Integer) -> Bool
    expectOwnCS value (tokenName, amount) =
      let cs = Ledger.ownCurrencySymbol ctx
          tokens = foldMap M.toList (M.lookup cs (Value.getValue value))
       in case tokens of
            [(tn, amt)] -> tn == tokenName && amt == amount
            _ -> False
