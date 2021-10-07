{-# LANGUAGE TemplateHaskell #-}

--{-# OPTIONS_GHC -fno-specialise #-}

module Test.ArdanaDollar.PriceOracle.ValidatorTest (priceOracleValidatorTests) where

import Data.Semigroup ((<>))

import Ledger qualified
import Ledger.Ada qualified as Ada
import Ledger.Crypto (pubKeyHash)
import Ledger.Oracle qualified as Oracle
import Ledger.Scripts (mkValidatorScript)
import Plutus.V1.Ledger.Api (singleton)
import PlutusTx qualified
import PlutusTx.Prelude hiding (Semigroup (..), mconcat)
import PlutusTx.UniqueMap qualified as UniqueMap
import Test.Tasty (testGroup)
import Test.Tasty.Options (OptionSet, setOption)
import Test.Tasty.Plutus.Context
import Test.Tasty.Plutus.Script.Unit
import Test.Tasty.Runners (TestTree (..))
import Wallet.Emulator.Types (knownWallet, walletPubKey)

import ArdanaDollar.PriceOracle.OnChain

priceOracleValidatorTests :: TestTree
priceOracleValidatorTests = testGroup "PriceOracle Validator" [simpleValidatorTest
                                                              ,newPriceTrackingOutOfRange]

setTimeRangeOpt :: Integer -> Integer -> OptionSet -> OptionSet
setTimeRangeOpt from to =
  setOption
    ( TimeRange
        ( Ledger.Interval
            (Ledger.LowerBound (Ledger.Finite (Ledger.POSIXTime from)) True)
            (Ledger.UpperBound (Ledger.Finite (Ledger.POSIXTime to)) True)
        )
    )

simpleValidatorTest :: TestTree
simpleValidatorTest = PlusTestOptions (setTimeRangeOpt 500 600) $
  withValidator "Simple" validator $ do
    shouldValidate "Example" testData $
      spendsFromPubKeySigned userHash (Ada.lovelaceValueOf 0)
        <> input (Input (OwnType . PlutusTx.toBuiltinData $ ownerSignedEmptyPriceTracking) oracleCS)
        <> output (Output (OwnType . PlutusTx.toBuiltinData $ ownerSignedEmptyPriceTracking) oracleCS)
  where
    oracleCS = singleton currSym "PriceTracking" 1
    userPK :: Ledger.PubKey
    userPK = walletPubKey (knownWallet 1)

    userPrivK :: Ledger.PrivateKey
    userPrivK = Ledger.privateKey1

    userHash :: Ledger.PubKeyHash
    userHash = pubKeyHash userPK

    -- mock currency symbol. can't construct with minter due to cycle
    currSym = "123456789012345678901234567890ef"

    params :: OracleValidatorParams
    params = OracleValidatorParams currSym userPK userHash "PriceTracking"

    validator :: Ledger.Validator
    validator =
      mkValidatorScript $
        $$(PlutusTx.compile [||go||])
          `PlutusTx.applyCode` oracleCompiledTypedValidator params
      where
        {-# INLINEABLE go #-}
        go ::
          (Oracle.SignedMessage PriceTracking -> () -> Ledger.ScriptContext -> Bool) ->
          (BuiltinData -> BuiltinData -> BuiltinData -> ())
        go = toTestValidator

    testData {- Oracle.SignedMessage PriceTracking -> () -> Value -> -} :: TestData 'ForSpending
    testData = SpendingTest ownerSignedEmptyPriceTracking () mempty

    ownerSignedEmptyPriceTracking :: Oracle.SignedMessage PriceTracking
    ownerSignedEmptyPriceTracking = Oracle.signMessage (PriceTracking UniqueMap.empty UniqueMap.empty (Ledger.POSIXTime 550)) userPrivK

newPriceTrackingOutOfRange :: TestTree
newPriceTrackingOutOfRange = PlusTestOptions (setTimeRangeOpt 500 600) $
  withValidator "PriceTracking Error" validator $ do
    shouldn'tValidate "Timestamp Beyond TimeRange UpperBound" testData $
      spendsFromPubKeySigned userHash (Ada.lovelaceValueOf 0)
        <> input (Input (OwnType . PlutusTx.toBuiltinData $ ownerSignedEmptyPriceTracking) oracleCS)
        <> output (Output (OwnType . PlutusTx.toBuiltinData $ ownerSignedEmptyPriceTracking) oracleCS)
  where
    oracleCS = singleton currSym "PriceTracking" 1
    userPK :: Ledger.PubKey
    userPK = walletPubKey (knownWallet 1)

    userPrivK :: Ledger.PrivateKey
    userPrivK = Ledger.privateKey1

    userHash :: Ledger.PubKeyHash
    userHash = pubKeyHash userPK

    -- mock currency symbol. can't construct with minter due to cycle
    currSym = "123456789012345678901234567890ef"

    params :: OracleValidatorParams
    params = OracleValidatorParams currSym userPK userHash "PriceTracking"

    validator :: Ledger.Validator
    validator =
      mkValidatorScript $
        $$(PlutusTx.compile [||go||])
          `PlutusTx.applyCode` oracleCompiledTypedValidator params
      where
        {-# INLINEABLE go #-}
        go ::
          (Oracle.SignedMessage PriceTracking -> () -> Ledger.ScriptContext -> Bool) ->
          (BuiltinData -> BuiltinData -> BuiltinData -> ())
        go = toTestValidator

    testData {- Oracle.SignedMessage PriceTracking -> () -> Value -> -} :: TestData 'ForSpending
    testData = SpendingTest ownerSignedEmptyPriceTracking () mempty

    ownerSignedEmptyPriceTracking :: Oracle.SignedMessage PriceTracking
    ownerSignedEmptyPriceTracking = Oracle.signMessage (PriceTracking UniqueMap.empty UniqueMap.empty (Ledger.POSIXTime 650)) userPrivK


