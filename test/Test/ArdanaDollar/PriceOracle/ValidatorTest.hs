{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-specialise #-}

module Test.ArdanaDollar.PriceOracle.ValidatorTest (priceOracleValidatorTests) where

import Data.Semigroup ((<>))

import Ledger qualified
import Ledger.Ada qualified as Ada
import Ledger.Crypto (pubKeyHash)
import Ledger.Scripts (mkValidatorScript)
import Ledger.Oracle qualified as Oracle
import PlutusTx qualified
import PlutusTx.Prelude hiding (Semigroup (..), mconcat)
import qualified PlutusTx.UniqueMap as UniqueMap
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Plutus.Context
import Test.Tasty.Plutus.Script.Unit
import Wallet.Emulator.Types (knownWallet, walletPubKey)

import ArdanaDollar.PriceOracle.OnChain

priceOracleValidatorTests :: TestTree
priceOracleValidatorTests = testGroup "PriceOracle Validator" [simpleValidatorTest]

simpleValidatorTest :: TestTree
simpleValidatorTest = withValidator "Simple" validator $ do
  shouldValidate "Example" testData $
    spendsFromPubKeySigned userHash (Ada.lovelaceValueOf 0)
      <> output (Output (OwnType . PlutusTx.toBuiltinData $ PriceTracking UniqueMap.empty UniqueMap.empty 0) (Ada.lovelaceValueOf 0))
  shouldn'tValidate "Missing input" testData $
    output (Output (OwnType . PlutusTx.toBuiltinData $ PriceTracking UniqueMap.empty UniqueMap.empty 0) mempty)
  where
    userHash :: Ledger.PubKeyHash
    userHash = pubKeyHash $ walletPubKey (knownWallet 1)

    params :: OracleValidatorParams
    params = OracleValidatorParams "" "" "" "USD"

    validator :: Ledger.Validator
    validator =
      mkValidatorScript $
        $$(PlutusTx.compile [||go||])
          `PlutusTx.applyCode` ( $$(PlutusTx.compile [||mkOracleValidator||])
                                  `PlutusTx.applyCode` PlutusTx.liftCode params
                               )
      where
        go ::
          (Oracle.SignedMessage PriceTracking -> () -> Ledger.ScriptContext -> Bool) ->
          (BuiltinData -> BuiltinData -> BuiltinData -> ())
        go = toTestValidator

    testData {- Oracle.SignedMessage PriceTracking -> VaultRedeemer -> Value -> -} :: TestData 'ForSpending
    testData = SpendingTest (PriceTracking UniqueMap.empty UniqueMap.empty 0) () mempty
