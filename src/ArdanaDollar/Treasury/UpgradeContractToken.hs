{-# LANGUAGE TemplateHaskell #-}

module ArdanaDollar.Treasury.UpgradeContractToken (
  upgradeContractTokenAssetClass,
  upgradeContractTokenCurrency,
  upgradeContractTokenMintingConstraints,
  upgradeContractTokenMintingPolicy,
  upgradeContractTokenName,
) where

--------------------------------------------------------------------------------

import Data.Kind (Type)

--------------------------------------------------------------------------------

import Ledger qualified
import Ledger.Constraints (ScriptLookups, TxConstraints)
import Ledger.Constraints qualified as Constraints
import Ledger.Contexts qualified as Contexts
import Ledger.Crypto (PubKeyHash)
import Ledger.Typed.Scripts qualified as Scripts
import Ledger.Value qualified as Value
import Plutus.V1.Ledger.Api (Credential (..))
import Plutus.V1.Ledger.Value.Extra (valueSubsetOf)
import PlutusTx qualified
import PlutusTx.PointFree.Extra ((>>>))
import PlutusTx.Prelude

--------------------------------------------------------------------------------

import ArdanaDollar.Treasury.Types (TreasuryUpgradeContractTokenParams (..))

-- TODO: Unify with StateToken
upgradeContractTokenMintingConstraints ::
  forall (a :: Type) (i :: Type) (o :: Type).
  TreasuryUpgradeContractTokenParams ->
  (ScriptLookups a, TxConstraints i o, Value.Value)
upgradeContractTokenMintingConstraints params =
  ( Constraints.mintingPolicy $ upgradeContractTokenMintingPolicy params
  , Constraints.mustMintValue (toMint 2)
  , toMint 1
  )
  where
    toMint :: Integer -> Value.Value
    toMint i = Value.assetClassValue (upgradeContractTokenAssetClass params) i

{-# INLINEABLE upgradeContractTokenName #-}
upgradeContractTokenName :: Value.TokenName
upgradeContractTokenName = Value.TokenName "UpgradeToken"

{-# INLINEABLE mkUpgradeContractTokenMintingPolicy #-}
mkUpgradeContractTokenMintingPolicy :: TreasuryUpgradeContractTokenParams -> () -> Contexts.ScriptContext -> Bool
mkUpgradeContractTokenMintingPolicy params () sc =
  traceIfFalse "expected utxos missing from input" hasCorrectInput
    && traceIfFalse "should have minted exactly two tokens" hasCorrectOutput
    && traceIfFalse "is not signed by initialOwner" isSigned
    && traceIfFalse "minting wrong token" isCorrectlyForging
  where
    info :: Contexts.TxInfo
    info = Ledger.scriptContextTxInfo sc

    signatories :: [PubKeyHash]
    signatories = Ledger.txInfoSignatories info

    upgradeTokenAssetClass :: Value.AssetClass
    upgradeTokenAssetClass =
      Value.assetClass (Ledger.ownCurrencySymbol sc) upgradeContractTokenName

    hasCorrectInput :: Bool
    hasCorrectInput = flip any (Contexts.txInfoInputs info) $ \input ->
      Contexts.txInInfoOutRef input == upgradeToken'initialOutput params

    -- TODO: check for datum and state tokens!
    hasCorrectOutput :: Bool
    hasCorrectOutput =
      (2 ==) . length $
        flip filter (Contexts.txInfoOutputs info) $
          \Contexts.TxOut
            { txOutAddress = addr
            , txOutValue = v
            } ->
              isScriptAddress addr
                && v `valueSubsetOf` forgeValue 1
                && Value.assetClassValueOf v upgradeTokenAssetClass == 1

    forgeValue :: Integer -> Value.Value
    forgeValue i = Value.assetClassValue upgradeTokenAssetClass i

    isSigned :: Bool
    isSigned = upgradeToken'initialOwner params `elem` signatories

    isCorrectlyForging :: Bool
    isCorrectlyForging = Contexts.txInfoMint info `valueSubsetOf` forgeValue 2

    isScriptAddress :: Ledger.Address -> Bool
    isScriptAddress =
      Ledger.addressCredential >>> \case
        ScriptCredential _ -> True
        PubKeyCredential _ -> False

{-# INLINEABLE upgradeContractTokenMintingPolicy #-}
upgradeContractTokenMintingPolicy :: TreasuryUpgradeContractTokenParams -> Ledger.MintingPolicy
upgradeContractTokenMintingPolicy params =
  Ledger.mkMintingPolicyScript $
    $$(PlutusTx.compile [||Scripts.wrapMintingPolicy . mkUpgradeContractTokenMintingPolicy||])
      `PlutusTx.applyCode` PlutusTx.liftCode params

{-# INLINEABLE upgradeContractTokenCurrency #-}
upgradeContractTokenCurrency :: TreasuryUpgradeContractTokenParams -> Value.CurrencySymbol
upgradeContractTokenCurrency =
  Contexts.scriptCurrencySymbol . upgradeContractTokenMintingPolicy

{-# INLINEABLE upgradeContractTokenAssetClass #-}
upgradeContractTokenAssetClass :: TreasuryUpgradeContractTokenParams -> Value.AssetClass
upgradeContractTokenAssetClass params =
  Value.assetClass (upgradeContractTokenCurrency params) upgradeContractTokenName
