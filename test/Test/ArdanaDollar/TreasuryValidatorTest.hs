{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-specialise #-}

module Test.ArdanaDollar.TreasuryValidatorTest (treasuryValidatorTests) where

--------------------------------------------------------------------------------

import Control.Exception (catch, throwIO)
import Control.Monad.IO.Class (liftIO)
import Data.Semigroup ((<>))
import GHC.IO.Encoding
import Hedgehog (Property, PropertyT, forAll, property)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Gen.Extra qualified as Gen (integer)
import System.Exit (ExitCode (ExitSuccess))
import System.IO.Silently (capture)
import Prelude (IO, mconcat, putStrLn)

--------------------------------------------------------------------------------

import Ledger qualified
import Ledger.Credential (Credential (PubKeyCredential, ScriptCredential))
import Ledger.Scripts (mkValidatorScript)
import Ledger.Value qualified as Value (assetClassValue, isZero)
import PlutusTx qualified
import PlutusTx.Prelude hiding (Semigroup (..), mconcat)
import PlutusTx.UniqueMap qualified as UniqueMap
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.Plutus.Context
import Test.Tasty.Plutus.Script.Unit

--------------------------------------------------------------------------------

import ArdanaDollar.Treasury.CanSpendToken (canSpendTokenAssetClass)
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
    [ testGroup
        "Upgrade path"
        [ testProperty "Upgrade path should transfer token" upgradePathTransfersToken
        , testProperty "Upgrade path should not burn upgrade token" upgradePathBurnToken
        , testProperty "Upgrade path should not mint upgrade token" upgradePathMintToken
        , testProperty "Upgrade path should not miss upgrade token" upgradePathMissingToken
        , testProperty "Upgrade path updates datum" upgradePathUpdatesDatum
        ]
    , testGroup
        "Spend funds"
        [ testProperty "Spend funds should spend funds" spendFundsSpendsFunds
        , testProperty "Spend funds modify datum" spendFundsUpdatesDatum
        , testProperty "Spend funds should not pay someone else" spendFundsPaysSomeoneElse
        ]
    ]

--------------------------------- UPGRADE PATH ---------------------------------
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

  liftIO (runTest test)
  where
    validator :: Treasury -> Ledger.Validator
    validator t =
      mkValidatorScript $
        $$(PlutusTx.compile [||go||])
          `PlutusTx.applyCode` ( $$(PlutusTx.compile [||mkTreasuryValidator||])
                                  `PlutusTx.applyCode` PlutusTx.liftCode t
                                  `PlutusTx.applyCode` PlutusTx.liftCode (canSpendTokenAssetClass t)
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

--------------------------------- SPEND FUNDS ----------------------------------
spendFundsProperty ::
  ( TreasuryDatum ->
    Ledger.Value ->
    Ledger.Value ->
    (Ledger.ValidatorHash, Ledger.Value) ->
    (BuiltinByteString, Ledger.Value) ->
    Ledger.Address ->
    PropertyT IO (WithScript 'ForSpending ())
  ) ->
  PropertyT IO ()
spendFundsProperty validationBuilder = do
  treasury <- forAll ArdanaGen.treasury
  treasuryDatum <-
    forAll $
      Gen.filter (\td -> length (costCenters td) > 0) ArdanaGen.treasuryDatum
  adminHash <- forAll PlutusGen.validatorHash
  ccWithValue <- forAll $ Gen.element (UniqueMap.toList $ costCenters treasuryDatum)
  address <- forAll PlutusGen.address

  let sToken = Value.assetClassValue (treasury'stateTokenSymbol treasury) 1
      uToken = Value.assetClassValue (treasury'upgradeTokenSymbol treasury) 1
      canSpendToken = Value.assetClassValue (canSpendTokenAssetClass treasury) 1

  validation <-
    validationBuilder
      treasuryDatum
      sToken
      uToken
      (adminHash, canSpendToken)
      ccWithValue
      address
  let test = withValidator "Spending cost center funds" (validator treasury) validation

  liftIO (runTest test)
  where
    validator :: Treasury -> Ledger.Validator
    validator t =
      mkValidatorScript $
        $$(PlutusTx.compile [||go||])
          `PlutusTx.applyCode` ( $$(PlutusTx.compile [||mkTreasuryValidator||])
                                  `PlutusTx.applyCode` PlutusTx.liftCode t
                                  `PlutusTx.applyCode` PlutusTx.liftCode (canSpendTokenAssetClass t)
                               )
      where
        go ::
          (TreasuryDatum -> TreasuryAction -> Ledger.ScriptContext -> Bool) ->
          (BuiltinData -> BuiltinData -> BuiltinData -> ())
        go = toTestValidator

spendFundsSpendsFunds :: Property
spendFundsSpendsFunds = property $ spendFundsProperty validationBuilder
  where
    validationBuilder ::
      TreasuryDatum ->
      Ledger.Value ->
      Ledger.Value ->
      (Ledger.ValidatorHash, Ledger.Value) ->
      (BuiltinByteString, Ledger.Value) ->
      Ledger.Address ->
      PropertyT IO (WithScript 'ForSpending ())
    validationBuilder td sToken uToken (vh, csToken) (ccName, ccValue) address = do
      (Just subvalue) <- forAll $ PlutusGen.subvalue True ccValue
      anotherValue <- forAll PlutusGen.positiveValue

      let treasuryValue = mconcat $ UniqueMap.elems (costCenters td)
          newDatum =
            td
              { costCenters =
                  UniqueMap.insert
                    ccName
                    (ccValue <> inv subvalue)
                    (costCenters td)
              }
          spendParams =
            TreasurySpendParams
              { treasurySpend'value = subvalue
              , treasurySpend'costCenter = ccName
              , treasurySpend'beneficiary = address
              }
          adminContractST = contractScriptType vh
          beneficiaryST = case Ledger.addressCredential address of
            PubKeyCredential pkh -> PubKeyType pkh
            ScriptCredential vh' -> contractScriptType vh'

          rightContext =
            input (Input adminContractST csToken)
              <> output
                ( Output
                    (OwnType . PlutusTx.toBuiltinData $ newDatum)
                    (sToken <> uToken <> treasuryValue <> inv subvalue)
                )
              <> output (Output adminContractST csToken)
              <> output (Output beneficiaryST subvalue)
          rightContext' =
            input (Input adminContractST $ csToken <> anotherValue)
              <> output
                ( Output
                    (OwnType . PlutusTx.toBuiltinData $ newDatum)
                    (sToken <> uToken <> treasuryValue <> inv subvalue)
                )
              <> output (Output adminContractST csToken)
              <> output (Output beneficiaryST $ subvalue <> anotherValue)
          wrongContext =
            input (Input adminContractST csToken)
              <> output
                ( Output
                    (OwnType . PlutusTx.toBuiltinData $ newDatum)
                    (sToken <> uToken <> treasuryValue)
                )
              <> output (Output adminContractST csToken)
              <> output (Output beneficiaryST mempty)
          generateContext =
            input (Input adminContractST csToken)
              <> output
                ( Output
                    (OwnType . PlutusTx.toBuiltinData $ newDatum)
                    (sToken <> uToken <> treasuryValue)
                )
              <> output (Output adminContractST csToken)
              <> output (Output beneficiaryST subvalue)
          noTokenContext =
            input (Input adminContractST mempty)
              <> output
                ( Output
                    (OwnType . PlutusTx.toBuiltinData $ newDatum)
                    (sToken <> uToken <> treasuryValue <> inv subvalue)
                )
              <> output (Output adminContractST mempty)
              <> output (Output beneficiaryST subvalue)
          testData = SpendingTest td (SpendFundsFromCostCenter spendParams) (sToken <> uToken <> treasuryValue)

      return $ do
        shouldValidate "spends funds" testData rightContext
        shouldValidate "spends funds and transfer other value" testData rightContext'
        shouldn'tValidate "does not spend funds" testData wrongContext
        shouldn'tValidate "does generate funds" testData generateContext
        shouldn'tValidate "does not show CanSpend token" testData noTokenContext

spendFundsUpdatesDatum :: Property
spendFundsUpdatesDatum = property $ spendFundsProperty validationBuilder
  where
    validationBuilder ::
      TreasuryDatum ->
      Ledger.Value ->
      Ledger.Value ->
      (Ledger.ValidatorHash, Ledger.Value) ->
      (BuiltinByteString, Ledger.Value) ->
      Ledger.Address ->
      PropertyT IO (WithScript 'ForSpending ())
    validationBuilder td sToken uToken (vh, csToken) (ccName, ccValue) address = do
      (Just subvalue) <- forAll $ PlutusGen.subvalue True ccValue
      ncHash <- forAll PlutusGen.validatorHash
      i <- forAll $ Gen.filter (/= 0) Gen.integer
      anotherValue <- forAll $ Gen.filter (not . Value.isZero) PlutusGen.value
      anotherCostCenters <- forAll ArdanaGen.treasuryCostCenters

      let treasuryValue = mconcat $ UniqueMap.elems (costCenters td)
          spendParams =
            TreasurySpendParams
              { treasurySpend'value = subvalue
              , treasurySpend'costCenter = ccName
              , treasurySpend'beneficiary = address
              }
          adminContractST = contractScriptType vh
          beneficiaryST = case Ledger.addressCredential address of
            PubKeyCredential pkh -> PubKeyType pkh
            ScriptCredential vh' -> contractScriptType vh'

          datumContextBuilder dat =
            input (Input adminContractST csToken)
              <> output
                ( Output
                    (OwnType . PlutusTx.toBuiltinData $ dat)
                    (sToken <> uToken <> treasuryValue <> inv subvalue)
                )
              <> output (Output adminContractST csToken)
              <> output (Output beneficiaryST subvalue)

          correctDatumContext =
            datumContextBuilder $
              td
                { costCenters =
                    UniqueMap.insert
                      ccName
                      (ccValue <> inv subvalue)
                      (costCenters td)
                }
          oldDatumContext = datumContextBuilder td
          danaAmountDatumContext =
            datumContextBuilder $
              td
                { costCenters =
                    UniqueMap.insert
                      ccName
                      (ccValue <> inv subvalue)
                      (costCenters td)
                , auctionDanaAmount = i + auctionDanaAmount td
                }
          currentContractDatumContext =
            datumContextBuilder $
              td
                { costCenters =
                    UniqueMap.insert
                      ccName
                      (ccValue <> inv subvalue)
                      (costCenters td)
                , currentContract = ncHash
                }
          danaAmountDatumContext' =
            datumContextBuilder $
              td
                { auctionDanaAmount = i + auctionDanaAmount td
                }
          currentContractDatumContext' =
            datumContextBuilder $
              td
                { currentContract = ncHash
                }
          additionalValueCostCenterContext =
            datumContextBuilder $
              td
                { costCenters =
                    UniqueMap.insert
                      ccName
                      (ccValue <> inv (subvalue <> anotherValue))
                      (costCenters td)
                }
          additionalCostCenterContext =
            datumContextBuilder $
              td
                { costCenters =
                    UniqueMap.insert
                      ccName
                      (ccValue <> inv (subvalue <> anotherValue))
                      (UniqueMap.unionWith const anotherCostCenters (costCenters td))
                }

          testData = SpendingTest td (SpendFundsFromCostCenter spendParams) (sToken <> uToken <> treasuryValue)

      return $ do
        shouldValidate "modifies cost centers correctly" testData correctDatumContext
        shouldn'tValidate "does not modify datum" testData oldDatumContext
        shouldn'tValidate "modifies DANA amount" testData danaAmountDatumContext
        shouldn'tValidate "modifies currentContract" testData currentContractDatumContext
        shouldn'tValidate "modifies only DANA amount" testData danaAmountDatumContext'
        shouldn'tValidate "modifies only currentContract" testData currentContractDatumContext'
        shouldn'tValidate "modifies too much of cost center" testData additionalValueCostCenterContext
        shouldn'tValidate "modifies more cost centers" testData additionalCostCenterContext

spendFundsPaysSomeoneElse :: Property
spendFundsPaysSomeoneElse = property $ spendFundsProperty validationBuilder
  where
    validationBuilder ::
      TreasuryDatum ->
      Ledger.Value ->
      Ledger.Value ->
      (Ledger.ValidatorHash, Ledger.Value) ->
      (BuiltinByteString, Ledger.Value) ->
      Ledger.Address ->
      PropertyT IO (WithScript 'ForSpending ())
    validationBuilder td sToken uToken (vh, csToken) (ccName, ccValue) address = do
      (Just subvalue) <- forAll $ PlutusGen.subvalue True ccValue
      address' <- forAll PlutusGen.address

      let treasuryValue = mconcat $ UniqueMap.elems (costCenters td)
          newDatum =
            td
              { costCenters =
                  UniqueMap.insert
                    ccName
                    (ccValue <> inv subvalue)
                    (costCenters td)
              }
          spendParams =
            TreasurySpendParams
              { treasurySpend'value = subvalue
              , treasurySpend'costCenter = ccName
              , treasurySpend'beneficiary = address
              }
          adminContractST = contractScriptType vh
          beneficiaryST = case Ledger.addressCredential address of
            PubKeyCredential pkh -> PubKeyType pkh
            ScriptCredential vh' -> contractScriptType vh'
          otherST = case Ledger.addressCredential address' of
            PubKeyCredential pkh -> PubKeyType pkh
            ScriptCredential vh' -> contractScriptType vh'

          wrongContext =
            input (Input adminContractST csToken)
              <> output
                ( Output
                    (OwnType . PlutusTx.toBuiltinData $ newDatum)
                    (sToken <> uToken <> treasuryValue <> inv subvalue)
                )
              <> output (Output adminContractST csToken)
              <> output (Output otherST subvalue)
          wrongContext' = wrongContext <> output (Output beneficiaryST mempty)
          testData = SpendingTest td (SpendFundsFromCostCenter spendParams) (sToken <> uToken <> treasuryValue)

      return $ do
        shouldn'tValidate "pays to someone else" testData wrongContext
        shouldn'tValidate "pays to someone else" testData wrongContext'

------------------------------------ HELPERS -----------------------------------
-- TODO: Unfortunately, `capture` catches a bit more of the output than it
-- should and so it "eats" part of the test framework output. Maybe with some
-- Haskell's IO magic it could be fixed?
runTest :: TestTree -> IO ()
runTest test = do
  setLocaleEncoding utf8
  (stdOutput, result) <-
    capture $
      (defaultMain (const test) >> return ExitSuccess)
        `catch` (\(e :: ExitCode) -> return e)
  case result of
    ExitSuccess -> return ()
    _ -> putStrLn stdOutput >> throwIO result
