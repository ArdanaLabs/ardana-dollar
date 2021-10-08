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
import Prelude (String,show,error,IO)

import ArdanaDollar.PriceOracle.OnChain

priceOracleValidatorTests :: IO TestTree
priceOracleValidatorTests = do
  exploratory <- priceOracleExploratoryGeneratedTests
  return $ testGroup "Price Oracle Validator" [priceOracleValidatorRegressionTests, exploratory]


-- TODO compute maximally efficient exploration of the test parameter space
-- we want a good distribution here so we are likely to pick up on edge cases
-- using the constrants to filter/drive the generator would be a good idea
priceOracleExploratoryGeneratedTests :: IO TestTree
priceOracleExploratoryGeneratedTests = return $ testGroup "Exploratory generated tests"
    [ parametricValidatorTest $ TestParameters "Generated test" "totally random I swear" correctNFTCurrency True False 400 465 1 (JustSignedBy 1) (TestDatumParameters 1 450) (TestDatumParameters 1 460)
    ]


  -- TODO if one of these fails we want to know the parameters so we can inspect, correct,
  -- and add the parameters to our corpus of regression tests if appropriate
  -- perhaps that means running the tests in a context that can catch the error...
  -- a minimally acceptable solution for this would be printing the TestParameters to the
  -- console in such a way they can be easily copy pasted into an editor

priceOracleValidatorRegressionTests :: TestTree
priceOracleValidatorRegressionTests =
  --TODO check that these fail for specific and known reasons so that they can be named appropriately
  testGroup
    "Regression Tests"
    [ parametricValidatorTest $ TestParameters "Parametric Test" "state token not returned" correctNFTCurrency True False 400 465 1 (JustSignedBy 1) (TestDatumParameters 1 450) (TestDatumParameters 1 460)

    , parametricValidatorTest $ TestParameters "Parametric Test" "output signed by another" correctNFTCurrency True True 400 465 1 (JustSignedBy 1) (TestDatumParameters 1 450) (TestDatumParameters 2 460)

    , parametricValidatorTest $ TestParameters "Parametric Test" "everything is fine" correctNFTCurrency True True 400 465 1 (JustSignedBy 1) (TestDatumParameters 1 450) (TestDatumParameters 1 460)
    , parametricValidatorTest $ TestParameters "Parametric Test" "tx signed by another" correctNFTCurrency True True 400 465 1 (JustSignedBy 2) (TestDatumParameters 1 450) (TestDatumParameters 1 460)
    , parametricValidatorTest $ TestParameters "Parametric Test" "tx not signed" correctNFTCurrency True True 400 465 1 NoSigner (TestDatumParameters 1 450) (TestDatumParameters 1 460)


    ]



---- The test model domain
--
-- test instances can be generated by generating one of these


-- mock currency symbol. can't construct with minter due to cycle
mockCurrencySymbol :: Ledger.CurrencySymbol
mockCurrencySymbol = "123456789012345678901234567890ef"

correctNFTCurrency :: (Ledger.CurrencySymbol, Ledger.TokenName)
correctNFTCurrency = ("123456789012345678901234567890ef", "PriceTracking")


data TestParameters = TestParameters
  { subTreeName :: String
  , testName :: String
  , stateNFTCurrency :: (Ledger.CurrencySymbol, Ledger.TokenName)
  , scriptOwnsStateToken :: Bool -- scriptStartingValue? we could parameterise by the value itself - would that be useful?
  , stateTokenReturned :: Bool -- scriptEndingValue? what about who the script pays to - how do we test paying to some random address?
  , timeRangeLowerBound :: Integer
  , timeRangeUpperBound :: Integer
  , ownerWallet :: Integer
  , spenderParams :: SpenderParams
  , inputDatum :: TestDatumParameters
  , outputDatum :: TestDatumParameters
  }

data SpenderParams = NoSigner | JustSignedBy Integer | SignedByWithValue Integer Ledger.Value

data TestDatumParameters = TestDatumParameters
  { signedByWallet :: Integer
  , timeStamp :: Integer
  }

---- Constraints on the model domain
--
-- our model of why things should or should not validate

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

txSignedByOwner :: ModelConstraint
txSignedByOwner TestParameters { .. } = case spenderParams of
                                          NoSigner -> False
                                          JustSignedBy signer -> signer == ownerWallet
                                          SignedByWithValue signer _ -> signer == ownerWallet

---- Constraint composition forms the validation model
--
-- how we expect the validator to behave given our set of constraints

data ExpectedResult = Validates | DoesNotValidate

testResultModel :: TestParameters -> ExpectedResult
testResultModel p | all ($ p) [ --the constraints required for validation
                                inputDatumInRange
                              , outputDatumInRange
                              , rangeWithinSizeLimit
                              , stateTokenReturned
                              , inputSignedByOwner
                              , outputSignedByOwner
                              , scriptOwnsStateToken
                              , txSignedByOwner
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

modelTestData :: TestParameters -> TestData 'ForSpending
modelTestData TestParameters { .. } = SpendingTest (modelDatum inputDatum) () hasValue
  where
    hasValue :: Ledger.Value
    hasValue = if scriptOwnsStateToken
                  then singleton mockCurrencySymbol "PriceTracking" 1
                  else mempty



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

spendingAction :: SpenderParams -> ContextBuilder p -> ContextBuilder p
spendingAction NoSigner = id
spendingAction (JustSignedBy signer) = (<>) (signedWith $ pubKeyHash $ walletPubKey $ knownWallet signer)
spendingAction (SignedByWithValue signer value) = (<>) (spendsFromPubKeySigned (pubKeyHash $ walletPubKey $ knownWallet signer) value)

---- reification of the test using plutus-extra. mmmmm Tasty!
--

parametricValidatorTest :: TestParameters -> TestTree
parametricValidatorTest tp@TestParameters {..} =
  PlusTestOptions (setTimeRangeOption tp) $
    withValidator subTreeName validator $ do
      withExpectedResult (testResultModel tp) testName (modelTestData tp) $
        spendingAction spenderParams $
           output (Output (OwnType . PlutusTx.toBuiltinData $ modelDatum outputDatum) returnedValue)
  where

    returnedValue :: Ledger.Value
    returnedValue = if stateTokenReturned
                       then singleton mockCurrencySymbol "PriceTracking" 1
                       else mempty

    ownerPubKey :: Ledger.PubKey
    ownerPubKey = walletPubKey (knownWallet ownerWallet)

    ownerPubKeyHash :: Ledger.PubKeyHash
    ownerPubKeyHash = pubKeyHash ownerPubKey

    params :: OracleValidatorParams
    params = OracleValidatorParams (fst stateNFTCurrency) ownerPubKey ownerPubKeyHash "USD"

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

