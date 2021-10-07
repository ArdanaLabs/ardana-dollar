{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-specialise #-}

module Test.ArdanaDollar.TreasuryValidatorTest (treasuryValidatorTests) where

--------------------------------------------------------------------------------

import Control.Exception (catch, throwIO)
import Control.Monad.IO.Class (liftIO)
import Data.Semigroup ((<>))
import Hedgehog (Property, PropertyT, forAll, property)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Gen.Extra qualified as Gen (integer)
import System.Exit (ExitCode (ExitSuccess))
import System.IO.Silently (capture)
import Prelude (IO, putStrLn)

--------------------------------------------------------------------------------

import Ledger qualified
import Ledger.Scripts (mkValidatorScript)
import Ledger.Value qualified as Value (assetClassValue)
import PlutusTx qualified
import PlutusTx.Prelude hiding (Semigroup (..), mconcat)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.Plutus.Context
import Test.Tasty.Plutus.Script.Unit

--------------------------------------------------------------------------------

import ArdanaDollar.Treasury.OnChain (mkTreasuryValidator)
import ArdanaDollar.Treasury.Types

import Hedgehog.Gen.ArdanaDollar qualified as ArdanaGen
import Hedgehog.Gen.Plutus qualified as PlutusGen
import TastyDefaultMain (defaultMain)

--------------------------------------------------------------------------------

treasuryValidatorTests :: TestTree
treasuryValidatorTests =
  testGroup
    "Treasury Validator"
    [ testProperty "Upgrade path should transfer token" upgradePathTransfersToken
    , testProperty "Upgrade path should not burn upgrade token" upgradePathBurnToken
    , testProperty "Upgrade path should not mint upgrade token" upgradePathMintToken
    , testProperty "Upgrade path should not miss upgrade token" upgradePathMissingToken
    , testProperty "Upgrade path updates datum" upgradePathUpdatesDatum
    ]

upgradePathProperty ::
  ( TreasuryDatum ->
    Ledger.ValidatorHash ->
    Ledger.Value ->
    Ledger.Value ->
    PropertyT IO (WithScript 'ForSpending ())
  ) ->
  PropertyT IO ()
upgradePathProperty validationBuilder = do
  treasury <- forAll ArdanaGen.treasury
  ncHash <- forAll PlutusGen.validatorHash
  treasuryDatum <- forAll ArdanaGen.treasuryDatum

  let sToken = Value.assetClassValue (treasury'stateTokenSymbol treasury) 1
      uToken = Value.assetClassValue (treasury'upgradeTokenSymbol treasury) 1

  validation <- validationBuilder treasuryDatum ncHash sToken uToken
  let test = withValidator "Upgrade path" (validator treasury) validation

  -- TODO: Unfortunately, `capture` catches a bit more of the output than it
  -- should and so it "eats" part of the test framework output. Maybe with some
  -- Haskell's IO magic it could be fixed?
  (stdOutput, result) <-
    liftIO . capture $
      (defaultMain (const test) >> return ExitSuccess)
        `catch` (\(e :: ExitCode) -> return e)
  liftIO $ case result of
    ExitSuccess -> return ()
    _ -> putStrLn stdOutput >> throwIO result
  where
    validator :: Treasury -> Ledger.Validator
    validator t =
      mkValidatorScript $
        $$(PlutusTx.compile [||go||])
          `PlutusTx.applyCode` ( $$(PlutusTx.compile [||mkTreasuryValidator||])
                                  `PlutusTx.applyCode` PlutusTx.liftCode t
                               )
      where
        go ::
          (TreasuryDatum -> TreasuryAction -> Ledger.ScriptContext -> Bool) ->
          (BuiltinData -> BuiltinData -> BuiltinData -> ())
        go = toTestValidator

upgradePathTransfersToken :: Property
upgradePathTransfersToken = property $ upgradePathProperty validationBuilder
  where
    validationBuilder ::
      TreasuryDatum ->
      Ledger.ValidatorHash ->
      Ledger.Value ->
      Ledger.Value ->
      PropertyT IO (WithScript 'ForSpending ())
    validationBuilder td ncHash sToken uToken = do
      let newDatum = td {currentContract = ncHash}
          oldContractST = contractScriptType (currentContract td)
          newContractST = contractScriptType ncHash

          rightContext =
            input (Input oldContractST uToken)
              <> input (Input newContractST mempty)
              <> output (Output (OwnType . PlutusTx.toBuiltinData $ newDatum) (sToken <> uToken))
              <> output (Output oldContractST mempty)
              <> output (Output newContractST uToken)
          wrongContext =
            input (Input oldContractST uToken)
              <> input (Input newContractST mempty)
              <> output (Output (OwnType . PlutusTx.toBuiltinData $ newDatum) (sToken <> uToken))
              <> output (Output oldContractST uToken)
              <> output (Output newContractST mempty)
          testData = SpendingTest td (InitiateUpgrade $ NewContract ncHash) (sToken <> uToken)

      return $ do
        shouldValidate "transfers token" testData rightContext
        shouldn'tValidate "does not transfer token" testData wrongContext

upgradePathBurnToken :: Property
upgradePathBurnToken = property $ upgradePathProperty validationBuilder
  where
    validationBuilder ::
      TreasuryDatum ->
      Ledger.ValidatorHash ->
      Ledger.Value ->
      Ledger.Value ->
      PropertyT IO (WithScript 'ForSpending ())
    validationBuilder td ncHash sToken uToken = do
      let newDatum = td {currentContract = ncHash}
          oldContractST = contractScriptType (currentContract td)
          newContractST = contractScriptType ncHash

          burnContractContext =
            input (Input oldContractST uToken)
              <> input (Input newContractST mempty)
              <> output (Output (OwnType . PlutusTx.toBuiltinData $ newDatum) (sToken <> uToken))
              <> output (Output oldContractST mempty)
              <> output (Output newContractST mempty)
          burnTreasuryContext =
            input (Input oldContractST uToken)
              <> input (Input newContractST mempty)
              <> output (Output (OwnType . PlutusTx.toBuiltinData $ newDatum) sToken)
              <> output (Output oldContractST mempty)
              <> output (Output newContractST uToken)
          burnBothContext =
            input (Input oldContractST uToken)
              <> input (Input newContractST mempty)
              <> output (Output (OwnType . PlutusTx.toBuiltinData $ newDatum) sToken)
              <> output (Output oldContractST mempty)
              <> output (Output newContractST mempty)
          testData = SpendingTest td (InitiateUpgrade $ NewContract ncHash) (sToken <> uToken)

      return $ do
        shouldn'tValidate "burns contract's upgrade token" testData burnContractContext
        shouldn'tValidate "burns Treasury's upgrade token" testData burnTreasuryContext
        shouldn'tValidate "burns both upgrade token" testData burnBothContext

upgradePathMintToken :: Property
upgradePathMintToken = property $ upgradePathProperty validationBuilder
  where
    validationBuilder ::
      TreasuryDatum ->
      Ledger.ValidatorHash ->
      Ledger.Value ->
      Ledger.Value ->
      PropertyT IO (WithScript 'ForSpending ())
    validationBuilder td ncHash sToken uToken = do
      let newDatum = td {currentContract = ncHash}
          oldContractST = contractScriptType (currentContract td)
          newContractST = contractScriptType ncHash

          mintContractContext =
            input (Input oldContractST uToken)
              <> input (Input newContractST mempty)
              <> output (Output (OwnType . PlutusTx.toBuiltinData $ newDatum) (sToken <> uToken))
              <> output (Output oldContractST uToken)
              <> output (Output newContractST uToken)
          testData = SpendingTest td (InitiateUpgrade $ NewContract ncHash) (sToken <> uToken)

      return $ shouldn'tValidate "mints contract's upgrade token" testData mintContractContext

upgradePathMissingToken :: Property
upgradePathMissingToken = property $ upgradePathProperty validationBuilder
  where
    validationBuilder ::
      TreasuryDatum ->
      Ledger.ValidatorHash ->
      Ledger.Value ->
      Ledger.Value ->
      PropertyT IO (WithScript 'ForSpending ())
    validationBuilder td ncHash sToken uToken = do
      let newDatum = td {currentContract = ncHash}
          oldContractST = contractScriptType (currentContract td)
          newContractST = contractScriptType ncHash

          missingContractContext =
            input (Input oldContractST mempty)
              <> input (Input newContractST mempty)
              <> output (Output (OwnType . PlutusTx.toBuiltinData $ newDatum) (sToken <> uToken))
              <> output (Output oldContractST mempty)
              <> output (Output newContractST mempty)
          missingContractTestData =
            SpendingTest td (InitiateUpgrade $ NewContract ncHash) (sToken <> uToken)
          missingTreasuryContext =
            input (Input oldContractST uToken)
              <> input (Input newContractST mempty)
              <> output (Output (OwnType . PlutusTx.toBuiltinData $ newDatum) (sToken <> uToken))
              <> output (Output oldContractST mempty)
              <> output (Output newContractST uToken)
          missingTreasuryTestData =
            SpendingTest td (InitiateUpgrade $ NewContract ncHash) sToken

      return $ do
        shouldn'tValidate "burns contract's upgrade token" missingContractTestData missingContractContext
        shouldn'tValidate "burns Treasury's upgrade token" missingTreasuryTestData missingTreasuryContext

upgradePathUpdatesDatum :: Property
upgradePathUpdatesDatum = property $ upgradePathProperty validationBuilder
  where
    validationBuilder ::
      TreasuryDatum ->
      Ledger.ValidatorHash ->
      Ledger.Value ->
      Ledger.Value ->
      PropertyT IO (WithScript 'ForSpending ())
    validationBuilder td ncHash sToken uToken = do
      ncHash' <- forAll PlutusGen.validatorHash
      i <- forAll $ Gen.filter (/= auctionDanaAmount td) Gen.integer
      cc <- forAll $ Gen.filter (/= costCenters td) ArdanaGen.treasuryCostCenters

      let newDatum = td {currentContract = ncHash}
          oldContractST = contractScriptType (currentContract td)
          newContractST = contractScriptType ncHash

          ctxBuilder dat =
            input (Input oldContractST uToken)
              <> input (Input newContractST mempty)
              <> output (Output (OwnType . PlutusTx.toBuiltinData $ dat) (sToken <> uToken))
              <> output (Output oldContractST mempty)
              <> output (Output newContractST uToken)

          testData = SpendingTest td (InitiateUpgrade $ NewContract ncHash) (sToken <> uToken)

      return $ do
        shouldn'tValidate "does not change datum" testData (ctxBuilder td)
        shouldn'tValidate "saves wrong validator hash" testData $
          ctxBuilder (td {currentContract = ncHash'})
        shouldn'tValidate "changes auction DANA" testData $
          ctxBuilder (newDatum {auctionDanaAmount = i})
        shouldn'tValidate "changes cost centers" testData $
          ctxBuilder (newDatum {costCenters = cc})

contractScriptType :: Ledger.ValidatorHash -> ExternalType
contractScriptType = (`ScriptType` PlutusTx.toBuiltinData ())
