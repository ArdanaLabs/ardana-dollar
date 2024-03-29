{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module ArdanaDollar.Treasury.StateToken (
  treasuryStateTokenMintingConstraints,
  treasuryStateTokenName,
  treasuryStateTokenCurrency,
  treasuryStateTokenAssetClass,
  treasuryStateTokenParams,
) where

--------------------------------------------------------------------------------

import Data.Kind (Type)

--------------------------------------------------------------------------------

import Ledger qualified
import Ledger.Constraints (ScriptLookups, TxConstraints)
import Ledger.Constraints qualified as Constraints
import Ledger.Contexts qualified as Contexts
import Ledger.Typed.Scripts qualified as Scripts
import Ledger.Value qualified as Value
import Plutus.V1.Ledger.Api (Credential (..))
import Plutus.V1.Ledger.Value.Extra (valueSubsetOf)
import PlutusTx qualified
import PlutusTx.PointFree.Extra ((>>>))
import PlutusTx.Prelude

--------------------------------------------------------------------------------

import ArdanaDollar.Treasury.Types (
  TreasuryDatum,
  TreasuryStateTokenParams (..),
  isInitialDatum,
 )

treasuryStateTokenParams :: Contexts.TxOutRef -> TreasuryStateTokenParams
treasuryStateTokenParams = TreasuryStateTokenParams treasuryStateTokenName

treasuryStateTokenMintingConstraints ::
  forall (a :: Type) (i :: Type) (o :: Type).
  TreasuryStateTokenParams ->
  (ScriptLookups a, TxConstraints i o, Value.Value)
treasuryStateTokenMintingConstraints params =
  ( Constraints.mintingPolicy $ treasuryStateTokenMintingPolicy params
  , Constraints.mustMintValue toMint
  , toMint
  )
  where
    toMint :: Value.Value
    toMint = Value.assetClassValue tstAssetClass 1

    tstAssetClass :: Value.AssetClass
    tstAssetClass = treasuryStateTokenAssetClass params

{-# INLINEABLE treasuryStateTokenName #-}
treasuryStateTokenName :: Value.TokenName
treasuryStateTokenName = Value.TokenName "TreasuryToken"

{-# INLINEABLE mkTreasuryStateTokenMintingPolicy #-}
mkTreasuryStateTokenMintingPolicy :: TreasuryStateTokenParams -> () -> Contexts.ScriptContext -> Bool
mkTreasuryStateTokenMintingPolicy params () sc =
  traceIfFalse "expected utxo missing from input" hasCorrectInput
    && traceIfFalse "expected datum missing from output" hasCorrectOutput
    && traceIfFalse "minting wrong token" isCorrectlyForging
  where
    stateTokenAssetClass :: Value.AssetClass
    stateTokenAssetClass =
      Value.assetClass (Ledger.ownCurrencySymbol sc) (stateToken params)

    forgeValue :: Value.Value
    forgeValue = Value.assetClassValue stateTokenAssetClass 1

    info :: Contexts.TxInfo
    info = Ledger.scriptContextTxInfo sc

    hasCorrectInput :: Bool
    hasCorrectInput = flip any (Contexts.txInfoInputs info) $ \input ->
      Contexts.txInInfoOutRef input == initialOutput params

    hasCorrectOutput :: Bool
    hasCorrectOutput =
      flip any (Contexts.txInfoOutputs info) $
        \Contexts.TxOut
          { txOutAddress = addr
          , txOutValue = v
          , txOutDatumHash = dh
          } ->
            isScriptAddress addr
              && forgeValue `valueSubsetOf` v
              && Value.assetClassValueOf v stateTokenAssetClass == 1
              && maybeEmpty isCorrectDatum (dh >>= flip Contexts.findDatum info)

    isCorrectlyForging :: Bool
    isCorrectlyForging = forgeValue `valueSubsetOf` Contexts.txInfoMint info

    isScriptAddress :: Ledger.Address -> Bool
    isScriptAddress =
      Ledger.addressCredential >>> \case
        ScriptCredential _ -> True
        PubKeyCredential _ -> False

    isCorrectDatum :: Ledger.Datum -> Bool
    isCorrectDatum (Ledger.Datum d) =
      maybeEmpty isInitialDatum (PlutusTx.fromBuiltinData @TreasuryDatum d)

    maybeEmpty :: (a -> Bool) -> Maybe a -> Bool
    maybeEmpty cond = \case
      Nothing -> False
      Just x -> cond x

{-# INLINEABLE treasuryStateTokenMintingPolicy #-}
treasuryStateTokenMintingPolicy :: TreasuryStateTokenParams -> Ledger.MintingPolicy
treasuryStateTokenMintingPolicy params =
  Ledger.mkMintingPolicyScript $
    $$(PlutusTx.compile [||Scripts.wrapMintingPolicy . mkTreasuryStateTokenMintingPolicy||])
      `PlutusTx.applyCode` PlutusTx.liftCode params

{-# INLINEABLE treasuryStateTokenCurrency #-}
treasuryStateTokenCurrency :: TreasuryStateTokenParams -> Value.CurrencySymbol
treasuryStateTokenCurrency =
  Contexts.scriptCurrencySymbol . treasuryStateTokenMintingPolicy

{-# INLINEABLE treasuryStateTokenAssetClass #-}
treasuryStateTokenAssetClass :: TreasuryStateTokenParams -> Value.AssetClass
treasuryStateTokenAssetClass params =
  Value.assetClass (treasuryStateTokenCurrency params) treasuryStateTokenName
