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
import Ledger.Contexts (ScriptPurpose (Minting))
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
  case Contexts.scriptContextPurpose sc of
    Minting cs ->
      traceIfFalse "expected utxos missing from input" hasCorrectInput
        && traceIfFalse "should have minted exactly two tokens" (hasCorrectOutput cs)
        && traceIfFalse "is not signed by initialOwner" isSigned
        && traceIfFalse "minting wrong token" (isCorrectlyForging cs)
    _ -> traceError "minting policy is not minting"
  where
    info :: Contexts.TxInfo
    info = Ledger.scriptContextTxInfo sc

    signatories :: [PubKeyHash]
    signatories = Ledger.txInfoSignatories info

    upgradeTokenAssetClass :: Value.CurrencySymbol -> Value.AssetClass
    upgradeTokenAssetClass = flip Value.assetClass upgradeContractTokenName

    hasCorrectInput :: Bool
    hasCorrectInput = flip any (Contexts.txInfoInputs info) $ \input ->
      Contexts.txInInfoOutRef input == upgradeToken'initialOutput params

    -- TODO: check for datum and state tokens!
    hasCorrectOutput :: Value.CurrencySymbol -> Bool
    hasCorrectOutput cs =
      (2 ==) . length $
        flip filter (Contexts.txInfoOutputs info) $
          \Contexts.TxOut
            { txOutAddress = addr
            , txOutValue = v
            } ->
              isScriptAddress addr
                && v `valueSubsetOf` forgeValue cs 1
                && Value.assetClassValueOf v (upgradeTokenAssetClass cs) == 1

    forgeValue :: Value.CurrencySymbol -> Integer -> Value.Value
    forgeValue cs i = Value.assetClassValue (upgradeTokenAssetClass cs) i

    isSigned :: Bool
    isSigned = upgradeToken'initialOwner params `elem` signatories

    isCorrectlyForging :: Value.CurrencySymbol -> Bool
    isCorrectlyForging cs = Contexts.txInfoMint info `valueSubsetOf` forgeValue cs 2

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
