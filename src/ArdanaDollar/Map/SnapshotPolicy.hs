{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

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
  inputsAt',
  isLocked,
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
  SnapshotPerm (..),
  SnapshotPointer (..),
  SnapshotTokenRedeemer (..),
 )
import ArdanaDollar.Map.Types qualified as T

{-# INLINEABLE incrementSV #-}
incrementSV :: T.SnapshotVersion -> T.SnapshotVersion
incrementSV (T.SnapshotVersion v) = T.SnapshotVersion (v + 1)

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
  case redeemer of
    ----
    InitiateSnapshot last -> fromMaybe False $ do
      (!inputMap', !inputMap) <- mapInput
      (!outputMap', !outputMap) <- mapOutput

      (!lastInput', !lastInput) <- nodeInputByRef last
      (!lastOutput', !lastOutput) <- nodeOutputByKey (T.node'key lastInput)

      (!outputMapSnapshot', !outputMapSnapshot) <- mapSnapshotOutput

      !start' <- T.map'head inputMap
      !end' <- T.Pointer <$> lookupToken (T.unPointerCS pointerCS) lastOutput'

      let currSnapshot = T.map'nextVersion inputMap
          expectedSnapshotPerm = T.SnapshotPerm start' end' currSnapshot
      !outputSnapshotPerm' <- snapshotPermOutput expectedSnapshotPerm

      let mapAddress = Ledger.txOutAddress . Ledger.txInInfoResolved $ inputMap'

      return
        ( -- map not locked
          not (isLocked $ T.map'lockState inputMap)
            && not (isLocked $ T.node'lockState lastInput)
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
            && inputMap{T.map'lockState = LockedFor currSnapshot, T.map'nextVersion = incrementSV currSnapshot} == outputMap
            && Ledger.txOutValue (Ledger.txInInfoResolved inputMap') == Ledger.txOutValue outputMap'
            && lastInput{T.node'lockState = LockedFor currSnapshot} == lastOutput
            && Ledger.txOutValue (Ledger.txInInfoResolved lastInput') == Ledger.txOutValue lastOutput'
            -- quantative checks
            && inputsAt mapAddress == 2
            && outputsAt mapAddress == 3
            && expectMinting (snd $ Value.unAssetClass assetClass1) 2
            -- snapshot specific
            -- -- Map snapshot carries no other tokens
            && Ledger.txOutValue outputMapSnapshot' == Value.assetClassValue assetClass1 1
            -- -- Map snapshot carries correct data
            && T.mapSnapshot'assets outputMapSnapshot == Ledger.txOutValue outputMap'
            && T.mapSnapshot'version outputMapSnapshot == T.map'nextVersion inputMap
            && T.mapSnapshot'head outputMapSnapshot == (repackage <$> T.map'head inputMap)
            -- -- SnapshotPerm carries no other tokens
            && Ledger.txOutValue outputSnapshotPerm' == Value.assetClassValue assetClass1 1
        )
    ----
    SplitSnapshot snapshotTokenRef leftNodeRef rightNodeRef -> fromMaybe False $ do
      (!leftNodeInput', !leftNodeInput) <- nodeInputByRef leftNodeRef
      (!leftNodeOutput', !leftNodeOutput) <- nodeOutputByKey (T.node'key leftNodeInput)

      (!rightNodeInput', !rightNodeInput) <- nodeInputByRef rightNodeRef
      (!rightNodeOutput', !rightNodeOutput) <- nodeOutputByKey (T.node'key rightNodeInput)

      (!snapshotPermInput', !snapshotPermInput) <- snapshotPermInputByRef snapshotTokenRef

      l <- lookupToken (unPointerCS pointerCS) leftNodeOutput'
      r <- lookupToken (unPointerCS pointerCS) rightNodeOutput'
      let v = snapshotPerm'version snapshotPermInput
          left = SnapshotPerm (snapshotPerm'start snapshotPermInput) (Pointer l) v
          right = SnapshotPerm (Pointer r) (snapshotPerm'end snapshotPermInput) v
      snapshotPermLeftOutput' <- snapshotPermOutput left
      snapshotPermRightOutput' <- snapshotPermOutput right

      let mapAddress = Ledger.txOutAddress . Ledger.txInInfoResolved $ snapshotPermInput'

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
            && leftNodeInput{T.node'lockState = LockedFor v} == leftNodeOutput
            && Ledger.txOutValue (Ledger.txInInfoResolved leftNodeInput') == Ledger.txOutValue leftNodeOutput'
            && rightNodeInput{T.node'lockState = LockedFor v} == rightNodeOutput
            && Ledger.txOutValue (Ledger.txInInfoResolved rightNodeInput') == Ledger.txOutValue rightNodeOutput'
            -- quantative checks
            && inputsAt mapAddress == 3
            && outputsAt mapAddress == 4
            && expectMinting (snd $ Value.unAssetClass assetClass1) 1
            -- snapshot specific
            -- -- SnapshotPerm carries no other tokens
            && Ledger.txOutValue snapshotPermLeftOutput' == Value.assetClassValue assetClass1 1
            && Ledger.txOutValue snapshotPermRightOutput' == Value.assetClassValue assetClass1 1
        )
    ----
    MakeSnapshot snapshotTokenRef nodeRef -> fromMaybe False $ do
      (!nodeInput', !nodeInput) <- nodeInputByRef nodeRef
      (!nodeOutput', !nodeOutput) <- nodeOutputByKey (T.node'key nodeInput)

      (!snapshotPerm', !snapshotPerm) <- snapshotPermInputByRef snapshotTokenRef

      expectedPointer <- repackage . Pointer <$> lookupToken (unPointerCS pointerCS) nodeOutput'
      let expectedNodeSnapshot =
            NodeSnapshot
              (Ledger.txOutValue nodeOutput')
              (T.node'key nodeOutput)
              (T.node'value nodeOutput)
              (repackage <$> T.node'next nodeOutput)
              (T.snapshotPerm'version snapshotPerm)
      nodeSnapshotOutput'' <- nodeSnapshotOutput (unSnapshotPointer expectedPointer) expectedNodeSnapshot

      let mapAddress = Ledger.txOutAddress . Ledger.txInInfoResolved $ snapshotPerm'

      return
        ( -- outputs at correct address
          Ledger.txOutAddress nodeOutput' == mapAddress
            && Ledger.txOutAddress nodeSnapshotOutput'' == mapAddress
            -- input-output validation
            -- -- correct input linking
            -- -- correct output linking
            -- -- equality checks wrt Ledger.Value and (key, value) pairs
            && nodeInput == nodeOutput
            && Ledger.txOutValue (Ledger.txInInfoResolved nodeInput') == Ledger.txOutValue nodeOutput'
            -- quantative checks
            && inputsAt mapAddress == 2
            && outputsAt mapAddress == 2
            && expectMinting (snd $ Value.unAssetClass $ unSnapshotPointer expectedPointer) 1
            -- snapshot specific
            -- -- making snapshot of correct entry
            && T.snapshotPerm'start snapshotPerm == T.snapshotPerm'end snapshotPerm
            && hasOne (unPointer $ T.snapshotPerm'start snapshotPerm) nodeOutput'
            -- -- SnapshotPerm carries no other tokens
            && Ledger.txOutValue nodeSnapshotOutput''
              == Value.assetClassValue assetClass1 1
              <> Value.assetClassValue (unSnapshotPointer expectedPointer) 1
        )
  where
    info :: Ledger.TxInfo
    info = Ledger.scriptContextTxInfo ctx

    mapInput :: Maybe (Ledger.TxInInfo, Map)
    mapInput = mapInput' @k @v info inst (Ledger.txInfoInputs info)

    mapOutput :: Maybe (Ledger.TxOut, Map)
    mapOutput = mapOutput' @k @v info inst (Ledger.txInfoOutputs info)

    mapSnapshotOutput :: Maybe (Ledger.TxOut, MapSnapshot)
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
    nodePointsTo !node !txOut =
      maybe
        False
        (\pointer -> Value.assetClassValueOf (Ledger.txOutValue txOut) (T.unPointer pointer) == 1)
        (T.node'next node)

    nodeSnapshotOutput :: Value.AssetClass -> NodeSnapshot k v -> Maybe Ledger.TxOut
    nodeSnapshotOutput = nodeSnapshotOutput' info (Ledger.txInfoOutputs info)

    inputsAt :: Ledger.Address -> Integer
    inputsAt = inputsAt' info

    outputsAt :: Ledger.Address -> Integer
    outputsAt = outputsAt' info

    expectMinting :: Value.TokenName -> Integer -> Bool
    expectMinting tokenName amount =
      let cs = Ledger.ownCurrencySymbol ctx
          minted = Ledger.txInfoMint info
          tokens = maybe [] M.toList (M.lookup cs (Value.getValue minted))
       in case tokens of
            [(tn, amt)] -> tn == tokenName && amt == amount
            _ -> False