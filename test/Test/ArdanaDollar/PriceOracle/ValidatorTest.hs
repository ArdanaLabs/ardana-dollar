{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

--{-# OPTIONS_GHC -fno-specialise #-}

module Test.ArdanaDollar.PriceOracle.ValidatorTest (priceOracleValidatorTests) where

import Data.Semigroup ((<>))

import Ledger qualified
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
import Prelude (String,show,error)

import ArdanaDollar.PriceOracle.OnChain

priceOracleValidatorTests :: TestTree
priceOracleValidatorTests =
  testGroup
    "PriceOracle Validator"
    [ parametricValidatorTest $ TestParameters "Parametric Test" "state token not returned" False 400 465 1 (TestDatumParameters 1 450) (TestDatumParameters 1 460)

    , parametricValidatorTest $ TestParameters "Parametric Test" "output signed by another" True 400 465 1 (TestDatumParameters 1 450) (TestDatumParameters 2 460)

    , parametricValidatorTest $ TestParameters "Parametric Test" "everything is fine" True 400 465 1 (TestDatumParameters 1 450) (TestDatumParameters 1 460)
    ]

---- The test model domain
--

data TestParameters = TestParameters
  { subTreeName :: String
  , testName :: String
  , stateTokenReturned :: Bool
  , timeRangeLowerBound :: Integer
  , timeRangeUpperBound :: Integer
  , ownerWallet :: Integer
  , inputDatum :: TestDatumParameters
  , outputDatum :: TestDatumParameters
  }

data TestDatumParameters = TestDatumParameters
  { signedByWallet :: Integer
  , timeStamp :: Integer
  }

---- Constraints on the model domain
--

type ModelConstraint = TestParameters -> Bool

inputDatumInRange :: ModelConstraint
inputDatumInRange TestParameters { .. } = timeStamp inputDatum >= timeRangeLowerBound && timeStamp inputDatum <= timeRangeUpperBound

outputDatumInRange :: ModelConstraint
outputDatumInRange TestParameters { .. } = timeStamp outputDatum >= timeRangeLowerBound && timeStamp outputDatum <= timeRangeUpperBound

-- TODO clarify expected behaviour of this constraint
rangeWithinSizeLimit :: ModelConstraint
rangeWithinSizeLimit TestParameters { .. } =
   let rangeLen = timeRangeUpperBound - timeRangeLowerBound
    in rangeLen > 0 && rangeLen <= 1000

inputSignedByOwner :: ModelConstraint
inputSignedByOwner TestParameters { .. } = signedByWallet inputDatum == ownerWallet

outputSignedByOwner :: ModelConstraint
outputSignedByOwner TestParameters { .. } = signedByWallet outputDatum == ownerWallet

---- Constraint composition forms the validation model
--

data ExpectedResult = Validates | DoesNotValidate

testResultModel :: TestParameters -> ExpectedResult
testResultModel p | all ($ p) [ --the constraints required for validation
                                inputDatumInRange
                              , outputDatumInRange
                              , rangeWithinSizeLimit
                              , stateTokenReturned
                              , inputSignedByOwner
                              , outputSignedByOwner
                              ] = Validates
testResultModel _ = DoesNotValidate

withExpectedResult :: ExpectedResult -> String -> TestData 'ForSpending -> ContextBuilder 'ForSpending -> WithScript 'ForSpending ()
withExpectedResult Validates = shouldValidate
withExpectedResult DoesNotValidate = shouldn'tValidate

---- helpers
--

-- somewhat janky. would be nice if Ledger provided this feature for arbitrary integers
lookupPrivateKey :: Integer -> Ledger.PrivateKey
lookupPrivateKey 1 = Ledger.privateKey1
lookupPrivateKey 2 = Ledger.privateKey2
lookupPrivateKey 3 = Ledger.privateKey3
lookupPrivateKey 4 = Ledger.privateKey4
lookupPrivateKey 5 = Ledger.privateKey5
lookupPrivateKey i = Prelude.error ("test parameterisation error: PrivateKey" <> Prelude.show i <> " not found.")

-- TODO fire some hedgehog randomness into the pricetracking data - it shouldn't matter to the model what's in there
modelDatum :: TestDatumParameters -> Oracle.SignedMessage PriceTracking 
modelDatum TestDatumParameters { .. } =
  Oracle.signMessage (PriceTracking UniqueMap.empty UniqueMap.empty (Ledger.POSIXTime timeStamp)) signedByPrivK
  where
    signedByPrivK :: Ledger.PrivateKey
    signedByPrivK = lookupPrivateKey signedByWallet

modelTestData :: TestDatumParameters -> TestData 'ForSpending
modelTestData tdp = SpendingTest (modelDatum tdp) () mempty

setTimeRangeOpt :: Integer -> Integer -> OptionSet -> OptionSet
setTimeRangeOpt from to =
  setOption
    ( TimeRange
        ( Ledger.Interval
            (Ledger.LowerBound (Ledger.Finite (Ledger.POSIXTime from)) True)
            (Ledger.UpperBound (Ledger.Finite (Ledger.POSIXTime to)) True)
        )
    )

setTimeRangeOption :: TestParameters -> OptionSet -> OptionSet
setTimeRangeOption TestParameters {..} = setTimeRangeOpt timeRangeLowerBound timeRangeUpperBound

---- reification of the test using plutus-extra. mmmmm Tasty!
--

parametricValidatorTest :: TestParameters -> TestTree
parametricValidatorTest tp@TestParameters {..} =
  PlusTestOptions (setTimeRangeOption tp) $
    withValidator subTreeName validator $ do
      withExpectedResult (testResultModel tp) testName (modelTestData inputDatum) $
        spendsFromPubKeySigned ownerPubKeyHash mempty
          <> output (Output (OwnType . PlutusTx.toBuiltinData $ modelDatum outputDatum) returnedValue)
  where

    returnedValue :: Ledger.Value
    returnedValue = if stateTokenReturned
                       then singleton currSym "PriceTracking" 1
                       else mempty
    ownerPubKey :: Ledger.PubKey
    ownerPubKey = walletPubKey (knownWallet ownerWallet)

    ownerPubKeyHash :: Ledger.PubKeyHash
    ownerPubKeyHash = pubKeyHash ownerPubKey

    -- mock currency symbol. can't construct with minter due to cycle
    currSym = "123456789012345678901234567890ef"

    params :: OracleValidatorParams
    params = OracleValidatorParams currSym ownerPubKey ownerPubKeyHash "PriceTracking"

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

