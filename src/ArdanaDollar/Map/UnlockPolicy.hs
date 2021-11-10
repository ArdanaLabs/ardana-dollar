{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-specialise #-}

module ArdanaDollar.Map.UnlockPolicy (
  mkUnlockPolicy,
) where

import ArdanaDollar.Map.Types (
  LockState (..),
  Map (..),
  MapInstance,
  Node (..),
  PointerCS (..),
  Unlock (..),
  UnlockCS (..),
  UnlockTokenRedeemer (..),
 )
import ArdanaDollar.Map.Types qualified as T

import ArdanaDollar.Map.TxUtils (
  inputsAt',
  isSnapshotDone,
  mapInput',
  mapOutput',
  nodeByKey',
  nodeInputRef',
  outputsAt',
  unlockInputByRef',
  unlockOutput',
  unlockOutputs',
 )

import Ledger qualified
import Ledger.Value qualified as Value
import PlutusTx.AssocMap qualified as M
import PlutusTx.IsData.Class (FromData)
import PlutusTx.Prelude

{-# INLINEABLE mkUnlockPolicy #-}
mkUnlockPolicy ::
  forall k v.
  (Eq v, Ord k, FromData k, FromData v) =>
  MapInstance ->
  PointerCS ->
  UnlockTokenRedeemer ->
  Ledger.ScriptContext ->
  Bool
mkUnlockPolicy mapInstance pointerCS redeemer ctx =
  case redeemer of
    ----
    GenerateUnlock snapshotVersion -> fromMaybe False $ do
      (inputMap', inputMap) <- mapInput
      (outputMap', outputMap) <- mapOutput

      unlockOutput'' <- unlockOutput (Unlock snapshotVersion)

      let mapAddress = Ledger.txOutAddress . Ledger.txInInfoResolved $ inputMap'

      return
        ( -- outputs at correct address
          Ledger.txOutAddress outputMap' == mapAddress
            && Ledger.txOutAddress unlockOutput'' == mapAddress
            -- input-output validation
            -- -- correct input linking
            -- -- correct output linking
            -- -- equality checks wrt Ledger.Value and (key, value) pairs
            && inputMap == outputMap
            && Ledger.txOutValue (Ledger.txInInfoResolved inputMap') == Ledger.txOutValue outputMap'
            -- quantative checks
            && inputsAt mapAddress == 1
            && outputsAt mapAddress == 2
            && expectOwnCS (Ledger.txInfoMint info) (snd $ Value.unAssetClass assetClass1, 1)
            -- unlock specific
            && Ledger.txOutValue unlockOutput'' == Value.assetClassValue assetClass1 1
            && case T.map'lockState inputMap of
              Unlocked -> snapshotVersion < map'nextVersion inputMap
              SnapshotDone v -> snapshotVersion < v
              _ -> False
        )
    ----
    CloneUnlock unlockRef -> fromMaybe False $ do
      (unlockInput', unlockInput) <- unlockInputByRef unlockRef
      let unlockOutputs'' = unlockOutputs unlockInput
          len = length unlockOutputs''

      let mapAddress = Ledger.txOutAddress . Ledger.txInInfoResolved $ unlockInput'

      return
        ( -- outputs at correct address
          all (\txOut -> Ledger.txOutAddress txOut == mapAddress) unlockOutputs''
            -- -- quantative checks
            && inputsAt mapAddress == 1
            && outputsAt mapAddress == len
            && expectOwnCS (Ledger.txInfoMint info) (snd $ Value.unAssetClass assetClass1, len - 1)
            -- -- unlock specific
            && all (\txOut -> Ledger.txOutValue txOut == Value.assetClassValue assetClass1 1) unlockOutputs''
        )
    ----
    DoUnlock unlockRef nodeRef -> fromMaybe False $ do
      (unlockInput', unlockInput) <- unlockInputByRef unlockRef

      (nodeInput', nodeInput) <- nodeInputByRef nodeRef
      (nodeOutput', nodeOutput) <- nodeOutputByKey (T.node'key nodeInput)

      let mapAddress = Ledger.txOutAddress . Ledger.txInInfoResolved $ unlockInput'

      return
        ( -- outputs at correct address
          Ledger.txOutAddress nodeOutput' == mapAddress
            -- input-output validation
            -- -- correct input linking
            -- -- correct output linking
            -- -- equality checks wrt Ledger.Value and (key, value) pairs
            && nodeInput {T.node'lockState = Unlocked} == nodeOutput
            && Ledger.txOutValue (Ledger.txInInfoResolved nodeInput') == Ledger.txOutValue nodeOutput'
            -- quantative checks
            && inputsAt mapAddress == 2
            && outputsAt mapAddress == 1
            && expectOwnCS (Ledger.txInfoMint info) (snd $ Value.unAssetClass assetClass1, -1)
            -- unlock specific
            && isSnapshotDone (T.node'lockState nodeInput) (T.unlock'version unlockInput)
        )
  where
    info :: Ledger.TxInfo
    info = Ledger.scriptContextTxInfo ctx

    mapInput :: Maybe (Ledger.TxInInfo, Map)
    mapInput = mapInput' @k @v info mapInstance (Ledger.txInfoInputs info)

    mapOutput :: Maybe (Ledger.TxOut, Map)
    mapOutput = mapOutput' @k @v info mapInstance (Ledger.txInfoOutputs info)

    unlockOutput :: Unlock -> Maybe Ledger.TxOut
    unlockOutput = unlockOutput' @k @v info (Ledger.txInfoOutputs info) (UnlockCS $ Ledger.ownCurrencySymbol ctx)

    unlockOutputs :: Unlock -> [Ledger.TxOut]
    unlockOutputs = unlockOutputs' @k @v info (Ledger.txInfoOutputs info) (UnlockCS $ Ledger.ownCurrencySymbol ctx)

    unlockInputByRef :: Ledger.TxOutRef -> Maybe (Ledger.TxInInfo, Unlock)
    unlockInputByRef = unlockInputByRef' @k @v info (UnlockCS $ Ledger.ownCurrencySymbol ctx) (Ledger.txInfoInputs info)

    nodeInputByRef :: Ledger.TxOutRef -> Maybe (Ledger.TxInInfo, Node k v)
    nodeInputByRef = nodeInputRef' info pointerCS (Ledger.txInfoInputs info)

    nodeOutputByKey :: k -> Maybe (Ledger.TxOut, Node k v)
    nodeOutputByKey = nodeByKey' info pointerCS (Ledger.txInfoOutputs info)

    inputsAt :: Ledger.Address -> Integer
    inputsAt = inputsAt' info

    outputsAt :: Ledger.Address -> Integer
    outputsAt = outputsAt' info

    expectOwnCS :: Value.Value -> (Value.TokenName, Integer) -> Bool
    expectOwnCS value (tokenName, amount) =
      let cs = Ledger.ownCurrencySymbol ctx
          tokens = maybe [] M.toList (M.lookup cs (Value.getValue value))
       in case tokens of
            [(tn, amt)] -> tn == tokenName && amt == amount
            _ -> False

    assetClass1 :: Value.AssetClass
    assetClass1 = Value.AssetClass (Ledger.ownCurrencySymbol ctx, Value.TokenName emptyByteString)
