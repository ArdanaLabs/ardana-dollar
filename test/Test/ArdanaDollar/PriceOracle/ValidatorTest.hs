{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

--{-# OPTIONS_GHC -fno-specialise #-}

module Test.ArdanaDollar.PriceOracle.ValidatorTest (priceOracleValidatorTests) where

import Data.Semigroup ((<>))

import Ledger qualified
import Ledger.Crypto (pubKeyHash)
import Ledger.Oracle qualified as Oracle
import Ledger.Scripts (mkValidatorScript)
import Plutus.V1.Ledger.Api (singleton, getValue)
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
import qualified PlutusTx.AssocMap as AssocMap
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
    [ parametricValidatorTest $ TestParameters "Generated test" "totally random I swear" correctNFTCurrency True 400 465 1 (JustSignedBy 1) (TestDatumParameters 1 450) (OutputParams mempty (Just (TestDatumParameters 1 460)))

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
    [ parametricValidatorTest $ TestParameters "Parametric Test" "state token not returned" correctNFTCurrency True 400 465 1 (JustSignedBy 1) (TestDatumParameters 1 450) (OutputParams mempty (Just (TestDatumParameters 1 460)))


    , parametricValidatorTest $ TestParameters "Parametric Test" "output signed by another" correctNFTCurrency True 400 465 1 (JustSignedBy 1) (TestDatumParameters 1 450) (OutputParams correctStateTokenValue (Just (TestDatumParameters 2 460)))


    , parametricValidatorTest $ TestParameters "Parametric Test" "everything is fine" correctNFTCurrency True 400 465 1 (JustSignedBy 1) (TestDatumParameters 1 450) (OutputParams correctStateTokenValue (Just (TestDatumParameters 1 460)))

    , parametricValidatorTest $ TestParameters "Parametric Test" "tx signed by another" correctNFTCurrency True 400 465 1 (JustSignedBy 2) (TestDatumParameters 1 450) (OutputParams correctStateTokenValue (Just (TestDatumParameters 1 460)))

    , parametricValidatorTest $ TestParameters "Parametric Test" "tx not signed" correctNFTCurrency True 400 465 1 NoSigner (TestDatumParameters 1 450) (OutputParams correctStateTokenValue (Just (TestDatumParameters 1 460)))

    ]



---- The test model domain
--
-- test instances can be generated by generating one of these


-- mock currency symbol. can't construct with minter due to cycle
mockCurrencySymbol :: Ledger.CurrencySymbol
mockCurrencySymbol = "123456789012345678901234567890ef"

correctNFTCurrency :: (Ledger.CurrencySymbol, Ledger.TokenName)
correctNFTCurrency = ("123456789012345678901234567890ef", "PriceTracking")

correctStateTokenValue :: Ledger.Value
correctStateTokenValue = singleton (fst correctNFTCurrency) (snd correctNFTCurrency) 1


data TestParameters = TestParameters
  { subTreeName :: String
  , testName :: String
  , stateNFTCurrency :: (Ledger.CurrencySymbol, Ledger.TokenName)
  , scriptOwnsStateToken :: Bool -- scriptStartingValue? we could parameterise by the value itself - would that be useful?
--  , scriptKeepsStateToken :: Bool -- spend to self, spend to somewhere else, don't spend
  , timeRangeLowerBound :: Integer
  , timeRangeUpperBound :: Integer
  , ownerWallet :: Integer
  , transactorParams :: SpenderParams
  , inputDatum :: TestDatumParameters
  , outputParams :: OutputParams
  }

data SpenderParams = NoSigner | JustSignedBy Integer | SignedByWithValue Integer Ledger.Value

data OutputParams =
  OutputParams {
    tokenOutput :: Ledger.Value
  , datumOutput :: Maybe TestDatumParameters
  }

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
outputDatumInRange TestParameters { .. } =
  case datumOutput outputParams of
    Nothing -> False
    Just so -> timeStamp so >= timeRangeLowerBound && timeStamp so <= timeRangeUpperBound

-- TODO clarify expected behaviour of this constraint
rangeWithinSizeLimit :: ModelConstraint
rangeWithinSizeLimit TestParameters { .. } =
   let rangeLen = timeRangeUpperBound - timeRangeLowerBound
    in rangeLen > 0 && rangeLen <= 1000

inputSignedByOwner :: ModelConstraint
inputSignedByOwner TestParameters { .. } = signedByWallet inputDatum == ownerWallet

outputDatumSignedByOwner :: ModelConstraint
outputDatumSignedByOwner TestParameters { .. } =
  case datumOutput outputParams of
    Nothing -> False
    Just d -> signedByWallet d == ownerWallet

txSignedByOwner :: ModelConstraint
txSignedByOwner TestParameters { .. } = case transactorParams of
                                          NoSigner -> False
                                          JustSignedBy signer -> signer == ownerWallet
                                          SignedByWithValue signer _ -> signer == ownerWallet

stateTokenReturned :: ModelConstraint
stateTokenReturned TestParameters { .. } =
  case AssocMap.lookup mockCurrencySymbol $ getValue $ tokenOutput outputParams of
    Nothing -> False
    Just so -> case AssocMap.lookup "PriceTracking" so of
                 Just 1  -> True
                 _ -> False
    

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
                              , outputDatumSignedByOwner
                              , scriptOwnsStateToken
                              , txSignedByOwner
                              ] = Validates
testResultModel _ = DoesNotValidate

withExpectedResult :: ExpectedResult -> String -> TestData 'ForSpending -> ContextBuilder 'ForSpending -> WithScript 'ForSpending ()
withExpectedResult Validates = shouldValidate
withExpectedResult DoesNotValidate = shouldn'tValidate

---- helpers
--

-- somewhat janky. would be nice if Ledger provided this feature for arbitrary integers (maybe it does and I'm just being janky for no reason)
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

transactorSpendingAction :: SpenderParams -> ContextBuilder p -> ContextBuilder p
transactorSpendingAction NoSigner = id
transactorSpendingAction (JustSignedBy signer) =
  (<>) (signedWith $ pubKeyHash $ walletPubKey $ knownWallet signer)
transactorSpendingAction (SignedByWithValue signer value) =
  (<>) (spendsFromPubKeySigned (pubKeyHash $ walletPubKey $ knownWallet signer) value)

scriptOutputAction :: OutputParams -> ContextBuilder p
scriptOutputAction OutputParams { .. } =
  case datumOutput of
    Nothing -> output (Output (OwnType . PlutusTx.toBuiltinData $ ()) tokenOutput)
    Just d ->
      output (Output (OwnType . PlutusTx.toBuiltinData $ modelDatum d) tokenOutput)

---- reification of the test using plutus-extra. mmmmm Tasty!
--

parametricValidatorTest :: TestParameters -> TestTree
parametricValidatorTest tp@TestParameters {..} =
  PlusTestOptions (setTimeRangeOption tp) $
    withValidator subTreeName validator $ do
      withExpectedResult (testResultModel tp) testName (modelTestData tp) $
        transactorSpendingAction transactorParams $
           scriptOutputAction outputParams
  where

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

