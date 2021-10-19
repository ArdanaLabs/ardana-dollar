{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-specialise #-}

module ArdanaDollar.Treasury.CanSpendToken (
  canSpendTokenMintingConstraints,
  canSpendTokenCurrency,
  canSpendTokenAssetClass,
  canSpendTokenName,
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
import PlutusTx qualified
import PlutusTx.Prelude

--------------------------------------------------------------------------------

import ArdanaDollar.Treasury.Types (Treasury (..))

--------------------------------------------------------------------------------

canSpendTokenMintingConstraints ::
  forall (a :: Type) (i :: Type) (o :: Type).
  Treasury ->
  (ScriptLookups a, TxConstraints i o, Value.Value)
canSpendTokenMintingConstraints treasury =
  ( Constraints.mintingPolicy $ canSpendTokenMintingPolicy treasury
  , Constraints.mustMintValue toMint
  , toMint
  )
  where
    toMint :: Value.Value
    toMint = Value.assetClassValue (canSpendTokenAssetClass treasury) 1

{-# INLINEABLE canSpendTokenName #-}
canSpendTokenName :: Value.TokenName
canSpendTokenName = Value.TokenName "CanSpend"

{-# INLINEABLE mkCanSpendTokenMintingPolicy #-}
mkCanSpendTokenMintingPolicy :: Treasury -> () -> Contexts.ScriptContext -> Bool
mkCanSpendTokenMintingPolicy treasury () sc = hasCorrectInput && hasCorrectOutput
  where
    info :: Contexts.TxInfo
    info = Ledger.scriptContextTxInfo sc

    canSpendAssetClass :: Value.AssetClass
    canSpendAssetClass =
      Value.assetClass (Ledger.ownCurrencySymbol sc) canSpendTokenName

    hasCorrectInput :: Bool
    hasCorrectInput =
      flip any (Contexts.txInfoInputs info) $ \input ->
        Value.assetClassValueOf
          (Contexts.txOutValue . Contexts.txInInfoResolved $ input)
          (treasury'stateTokenSymbol treasury)
          == 1

    hasCorrectOutput :: Bool
    hasCorrectOutput =
      flip any (Contexts.txInfoOutputs info) $ \Contexts.TxOut {txOutValue = v} ->
        Value.assetClassValueOf v canSpendAssetClass == 1

{-# INLINEABLE canSpendTokenMintingPolicy #-}
canSpendTokenMintingPolicy :: Treasury -> Ledger.MintingPolicy
canSpendTokenMintingPolicy treasury =
  Ledger.mkMintingPolicyScript $
    $$(PlutusTx.compile [||Scripts.wrapMintingPolicy . mkCanSpendTokenMintingPolicy||])
      `PlutusTx.applyCode` PlutusTx.liftCode treasury

{-# INLINEABLE canSpendTokenCurrency #-}
canSpendTokenCurrency :: Treasury -> Value.CurrencySymbol
canSpendTokenCurrency = Contexts.scriptCurrencySymbol . canSpendTokenMintingPolicy

{-# INLINEABLE canSpendTokenAssetClass #-}
canSpendTokenAssetClass :: Treasury -> Value.AssetClass
canSpendTokenAssetClass treasury =
  Value.assetClass (canSpendTokenCurrency treasury) canSpendTokenName
