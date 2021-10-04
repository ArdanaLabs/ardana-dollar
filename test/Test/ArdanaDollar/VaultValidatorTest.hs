{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-specialise #-}

module Test.ArdanaDollar.VaultValidatorTest (vaultValidatorTests) where

import Data.Semigroup ((<>))

import Ledger qualified
import Ledger.Ada qualified as Ada
import Ledger.Crypto (pubKeyHash)
import Ledger.Scripts (mkValidatorScript)
import PlutusTx qualified
import PlutusTx.Prelude hiding (Semigroup (..), mconcat)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Plutus.Context
import Test.Tasty.Plutus.Script.Unit
import Wallet.Emulator.Types (knownWallet, walletPubKey)

import ArdanaDollar.Vault

vaultValidatorTests :: TestTree
vaultValidatorTests = testGroup "Vault Validator" [simpleValidatorTest]

simpleValidatorTest :: TestTree
simpleValidatorTest = withValidator "Simple" validator $ do
  shouldValidate "Example" testData $
    spendsFromPubKeySigned userHash (Ada.lovelaceValueOf 10)
      <> output (Output (OwnType . PlutusTx.toBuiltinData $ VaultDatum 10 0) (Ada.lovelaceValueOf 10))
  shouldn'tValidate "Missing input" testData $
    output (Output (OwnType . PlutusTx.toBuiltinData $ VaultDatum 10 0) mempty)
  where
    userHash :: Ledger.PubKeyHash
    userHash = pubKeyHash $ walletPubKey (knownWallet 1)

    validator :: Ledger.Validator
    validator =
      mkValidatorScript $
        $$(PlutusTx.compile [||go||])
          `PlutusTx.applyCode` ( $$(PlutusTx.compile [||mkVaultValidator||])
                                  `PlutusTx.applyCode` PlutusTx.liftCode dUSDAsset
                                  `PlutusTx.applyCode` PlutusTx.liftCode userHash
                               )
      where
        go ::
          (VaultDatum -> VaultRedeemer -> Ledger.ScriptContext -> Bool) ->
          (BuiltinData -> BuiltinData -> BuiltinData -> ())
        go = toTestValidator

    testData {- VaultDatum -> VaultRedeemer -> Value -> -} :: TestData 'ForSpending
    testData = SpendingTest (VaultDatum 0 0) CollateralRedeemer mempty
