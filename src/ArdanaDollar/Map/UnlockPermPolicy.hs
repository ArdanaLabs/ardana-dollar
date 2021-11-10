{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module ArdanaDollar.Map.UnlockPermPolicy (
  mkUnlockPermPolicy,
) where

import Ledger.Value qualified as Value
import PlutusTx.IsData.Class (FromData)

import ArdanaDollar.Map.Types (
  LockState (..),
  Map (..),
  MapInstance,
  Node (..),
  NodeSnapshot (..),
  Pointer (..),
  PointerCS (..),
  SnapshotCS,
  UnlockPerm (..),
  UnlockPermCS (..),
  UnlockPermTokenRedeemer (..),
 )

import ArdanaDollar.Map.Types qualified as T

import Ledger qualified
import PlutusTx.AssocMap qualified as M
import PlutusTx.Prelude

import ArdanaDollar.Map.TxUtils (
  inputsAt',
  isSnapshotDone,
  lookupToken',
  mapInput',
  mapOutput',
  nodeByKey',
  nodeInputRef',
  nodeSnapshotInputByRef',
  nodeSnapshotOutput',
  outputsAt',
  unlockPermInputByRef',
  unlockPermOutput',
 )

{-# INLINEABLE mkUnlockPermPolicy #-}
mkUnlockPermPolicy ::
  forall k v.
  (Ord k, Eq v, FromData k, FromData v) =>
  MapInstance ->
  PointerCS ->
  SnapshotCS ->
  UnlockPermTokenRedeemer ->
  Ledger.ScriptContext ->
  Bool
mkUnlockPermPolicy mapInstance pointerCS snapshotCS redeemer ctx =
  case redeemer of
    ----
    InitiateUnlockPerm nodeSnapshotRef -> fromMaybe False $ do
      (nodeSnapshotInput', nodeSnapshotInput) <- nodeSnapshotInputByRef nodeSnapshotRef
      nodeSnapshotOutput'' <- nodeSnapshotOutput nodeSnapshotInput

      pointer <- Pointer <$> lookupToken' (unPointerCS pointerCS) (T.nodeSnapshot'assets nodeSnapshotInput)
      let unlock =
            UnlockPerm
              pointer
              pointer
              (T.node'next $ T.nodeSnapshot'datum nodeSnapshotInput)
              (T.nodeSnapshot'version nodeSnapshotInput)
      unlockOutput'' <- unlockPermOutput unlock

      let mapAddress = Ledger.txOutAddress . Ledger.txInInfoResolved $ nodeSnapshotInput'

      return
        ( -- outputs at correct address
          Ledger.txOutAddress nodeSnapshotOutput'' == mapAddress
            && Ledger.txOutAddress unlockOutput'' == mapAddress
            -- input-output validation
            -- -- correct input linking
            -- -- correct output linking
            -- -- equality checks wrt Ledger.Value and (key, value) pairs
            && Ledger.txOutValue (Ledger.txInInfoResolved nodeSnapshotInput') == Ledger.txOutValue nodeSnapshotOutput''
            -- quantative checks
            && inputsAt mapAddress == 1
            && outputsAt mapAddress == 2
            && expectOwnCS (Ledger.txInfoMint info) (snd $ Value.unAssetClass assetClass1, 1)
            -- unlock specific
            -- -- Unlock carries no other tokens
            && Ledger.txOutValue unlockOutput'' == Value.assetClassValue assetClass1 1
        )
    ----
    MergeUnlockPerm leftUnlockRef rightUnlockRef -> fromMaybe False $ do
      (leftUnlockInput', leftUnlockInput) <- unlockPermInputByRef leftUnlockRef
      (_, rightUnlockInput) <- unlockPermInputByRef rightUnlockRef

      let unlock =
            UnlockPerm
              (T.unlockPerm'start leftUnlockInput)
              (T.unlockPerm'end rightUnlockInput)
              (T.unlockPerm'next rightUnlockInput)
              (T.unlockPerm'version leftUnlockInput)
      unlockPermOutput'' <- unlockPermOutput unlock

      let mapAddress = Ledger.txOutAddress . Ledger.txInInfoResolved $ leftUnlockInput'

      return
        ( -- outputs at correct address
          Ledger.txOutAddress unlockPermOutput'' == mapAddress
            -- quantative checks
            && inputsAt mapAddress == 2
            && outputsAt mapAddress == 1
            && expectOwnCS (Ledger.txInfoMint info) (snd $ Value.unAssetClass assetClass1, -1)
            -- unlock specific
            -- -- Unlock carries correct data
            && T.unlockPerm'next leftUnlockInput == Just (T.unlockPerm'start rightUnlockInput)
            && T.unlockPerm'version leftUnlockInput == T.unlockPerm'version rightUnlockInput
            -- -- Unlock carries no other tokens
            && Ledger.txOutValue unlockPermOutput'' == Value.assetClassValue assetClass1 1
        )
    ----
    MakeUnlockPerm lastRef unlockRef -> fromMaybe False $ do
      (inputMap', inputMap) <- mapInput
      (outputMap', outputMap) <- mapOutput

      (lastInput', lastInput) <- nodeInputByRef lastRef
      (lastOutput', lastOutput) <- nodeOutputByKey (T.node'key lastInput)

      (_, unlockInput) <- unlockPermInputByRef unlockRef

      let mapAddress = Ledger.txOutAddress . Ledger.txInInfoResolved $ inputMap'

      return
        ( -- map not locked
          isSnapshotDone (T.map'lockState inputMap) (T.unlockPerm'version unlockInput)
            && isSnapshotDone (T.node'lockState lastInput) (T.unlockPerm'version unlockInput)
            -- outputs at correct address
            && Ledger.txOutAddress outputMap' == mapAddress
            && Ledger.txOutAddress lastOutput' == mapAddress
            -- input-output validation
            -- -- correct input linking
            && isNothing (T.node'next lastInput)
            -- -- correct output linking
            -- -- equality checks wrt Ledger.Value and (key, value) pairs
            && inputMap{T.map'lockState = Unlocked} == outputMap
            && Ledger.txOutValue (Ledger.txInInfoResolved inputMap') == Ledger.txOutValue outputMap'
            && lastInput{T.node'lockState = Unlocked} == lastOutput
            && Ledger.txOutValue (Ledger.txInInfoResolved lastInput') == Ledger.txOutValue lastOutput'
            -- quantative checks
            && inputsAt mapAddress == 3
            && outputsAt mapAddress == 2
            && expectOwnCS (Ledger.txInfoMint info) (snd $ Value.unAssetClass assetClass1, -1)
            -- unlock specific
            -- -- Unlock carries correct data
            && T.map'head inputMap == Just (T.unlockPerm'start unlockInput)
            && lookupToken' (unPointerCS pointerCS) (Ledger.txOutValue lastOutput') == Just (unPointer $ T.unlockPerm'end unlockInput)
        )
  where
    info :: Ledger.TxInfo
    info = Ledger.scriptContextTxInfo ctx

    nodeSnapshotInputByRef :: Ledger.TxOutRef -> Maybe (Ledger.TxInInfo, NodeSnapshot k v)
    nodeSnapshotInputByRef = nodeSnapshotInputByRef' info snapshotCS (Ledger.txInfoInputs info)

    unlockPermInputByRef :: Ledger.TxOutRef -> Maybe (Ledger.TxInInfo, UnlockPerm)
    unlockPermInputByRef = unlockPermInputByRef' @k @v info (UnlockPermCS $ Ledger.ownCurrencySymbol ctx) (Ledger.txInfoInputs info)

    nodeSnapshotOutput :: NodeSnapshot k v -> Maybe Ledger.TxOut
    nodeSnapshotOutput = nodeSnapshotOutput' info (Ledger.txInfoOutputs info) snapshotCS

    unlockPermOutput :: UnlockPerm -> Maybe Ledger.TxOut
    unlockPermOutput = unlockPermOutput' @k @v info (Ledger.txInfoOutputs info) (UnlockPermCS $ Ledger.ownCurrencySymbol ctx)

    nodeInputByRef :: Ledger.TxOutRef -> Maybe (Ledger.TxInInfo, Node k v)
    nodeInputByRef = nodeInputRef' info pointerCS (Ledger.txInfoInputs info)

    nodeOutputByKey :: k -> Maybe (Ledger.TxOut, Node k v)
    nodeOutputByKey = nodeByKey' info pointerCS (Ledger.txInfoOutputs info)

    mapInput :: Maybe (Ledger.TxInInfo, Map)
    mapInput = mapInput' @k @v info mapInstance (Ledger.txInfoInputs info)

    mapOutput :: Maybe (Ledger.TxOut, Map)
    mapOutput = mapOutput' @k @v info mapInstance (Ledger.txInfoOutputs info)

    inputsAt :: Ledger.Address -> Integer
    inputsAt = inputsAt' info

    outputsAt :: Ledger.Address -> Integer
    outputsAt = outputsAt' info

    assetClass1 :: Value.AssetClass
    assetClass1 = Value.AssetClass (Ledger.ownCurrencySymbol ctx, Value.TokenName emptyByteString)

    expectOwnCS :: Value.Value -> (Value.TokenName, Integer) -> Bool
    expectOwnCS value (tokenName, amount) =
      let cs = Ledger.ownCurrencySymbol ctx
          tokens = maybe [] M.toList (M.lookup cs (Value.getValue value))
       in case tokens of
            [(tn, amt)] -> tn == tokenName && amt == amount
            _ -> False
