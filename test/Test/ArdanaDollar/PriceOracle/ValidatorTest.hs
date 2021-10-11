{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

--{-# OPTIONS_GHC -fno-specialise #-}

module Test.ArdanaDollar.PriceOracle.ValidatorTest (priceOracleValidatorRegressionTests, priceOracleValidatorGeneratedTests) where

import Data.Semigroup ((<>))

import ArdanaDollar.PriceOracle.OnChain
import Control.Exception (catch, throwIO)
import Control.Monad.Trans.State
import Data.Kind (Type)
import Data.Map qualified as M
import Hedgehog (MonadGen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Gen.Plutus qualified as HP
import Hedgehog.Range qualified as Range
import Ledger qualified
import Ledger.Crypto (pubKeyHash)
import Ledger.Oracle qualified as Oracle
import Ledger.Scripts (mkValidatorScript)
import Ledger.Value qualified as Value
import Plutus.V1.Ledger.Api (getValue, singleton)
import PlutusTx qualified
import PlutusTx.AssocMap qualified as AssocMap
import PlutusTx.Prelude hiding (Semigroup (..), mconcat)
import PlutusTx.UniqueMap qualified as UniqueMap
import System.Exit (ExitCode (..))
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.Options (OptionSet, setOption)
import Test.Tasty.Plutus.Context
import Test.Tasty.Plutus.Script.Unit
import Test.Tasty.Runners (TestTree (..))
import Wallet.Emulator.Types (knownWallet, walletPubKey)
import Prelude (Bounded, Enum, Eq, IO, Ord, Show, String, maxBound, minBound, putStrLn, show)
import Prelude qualified (Eq (..))

---------------------------------------------------------------------------------
-- mock currency symbol. can't construct with minter due to cycle
mockCurrencySymbol :: Ledger.CurrencySymbol
mockCurrencySymbol = "123456789012345678901234567890ef"

correctNFTCurrency :: (Ledger.CurrencySymbol, Ledger.TokenName)
correctNFTCurrency = ("123456789012345678901234567890ef", "PriceTracking")

correctStateTokenValue :: Ledger.Value
correctStateTokenValue = uncurry singleton correctNFTCurrency 1

---------------------------------------------------------------------------------

priceOracleValidatorGeneratedTests :: IO TestTree
priceOracleValidatorGeneratedTests = genSpaceTreeIO

-- maybe we want some of these
priceOracleValidatorRegressionTests :: TestTree
priceOracleValidatorRegressionTests =
  testGroup
    "Regression Tests"
    []

wrapParametricTest :: NamedTestParameters -> IO ()
wrapParametricTest p =
  let tt = parametricValidatorTest p
   in defaultMain tt
        `catch` ( \e ->
                    case e of
                      ExitSuccess -> return ()
                      _ -> explainExpectedTestResult (parameters p) >> throwIO e
                )

explainExpectedTestResult :: TestParameters -> IO ()
explainExpectedTestResult p =
  case constraintViolations p of
    [] -> putStrLn "\nExpected the validate but got failure\n"
    e ->
      putStrLn $
        "\nExpected validation failure due to the following model constraint violations " <> show e <> "\n"
          <> show p
          <> "\n"

---- The test model domain
--
-- test instances can be generated by generating one of these

nameGeneratedParams :: TestParameters -> NamedTestParameters
nameGeneratedParams tp =
  NamedTestParameters
    { subTreeName = "Generated Price Oracle Validator Test"
    , testName = "violations: " <> show (constraintViolations tp)
    , parameters = tp
    }

data NamedTestParameters = NamedTestParameters
  { subTreeName :: String
  , testName :: String
  , parameters :: TestParameters
  }
  deriving (Show)

data TestParameters = TestParameters
  { stateNFTCurrency :: (Ledger.CurrencySymbol, Ledger.TokenName)
  , timeRangeLowerBound :: Integer
  , timeRangeUpperBound :: Integer
  , ownerWallet :: Integer
  , transactorParams :: SpenderParams
  , inputParams :: StateUTXOParams
  , outputParams :: StateUTXOParams
  , peggedCurrency :: BuiltinByteString
  }
  deriving (Show)

data SpenderParams = NoSigner | JustSignedBy Integer | SignedByWithValue Integer Ledger.Value
  deriving (Show)

data StateUTXOParams = StateUTXOParams
  { stateTokenValue :: Ledger.Value
  , stateDatumValue :: TestDatumParameters
  }
  deriving (Show)

-- NOTE: we are not modelling missing/invalid datum
-- I haven't figured out a nice way to do that with Tasty.Plutus yet
data TestDatumParameters = TestDatumParameters
  { signedByWallet :: Integer
  , timeStamp :: Integer
  }
  deriving (Show)

----- generatora
--

-- TODO rejection sampling is super inefficient.
-- I don't recommending upping the sample count...
genSpaceTreeIO :: IO TestTree
genSpaceTreeIO = do
  s <- runSpaceExplorationIO 1
  let nps = nameGeneratedParams <$> flattenSpaceExploration s
  return $ testGroup "Price Oracle Validator Generated Test Space Exploration" $ parametricValidatorTest <$> nps

flattenSpaceExploration :: SpaceExploration -> [TestParameters]
flattenSpaceExploration SpaceExploration {..} = shouldPass <> (snd =<< M.toList singleDimFailures) <> detritus

emptySpaceExploration :: SpaceExploration
emptySpaceExploration = SpaceExploration [] M.empty []

runSpaceExplorationIO :: Integer -> IO SpaceExploration
runSpaceExplorationIO coverage =
  Gen.sample $ execStateT (spaceExploration coverage) emptySpaceExploration

data SpaceExploration = SpaceExploration
  { shouldPass :: [TestParameters]
  , singleDimFailures :: M.Map Constraint [TestParameters]
  , detritus :: [TestParameters]
  }

spaceCovered :: Integer -> SpaceExploration -> Bool
spaceCovered coverage SpaceExploration {..} =
  length shouldPass >= coverage
    && length detritus >= coverage
    && and (hasCoverage singleDimFailures <$> [minBound .. maxBound])
  where
    hasCoverage :: M.Map Constraint [TestParameters] -> Constraint -> Bool
    hasCoverage m c =
      case M.lookup c m of
        Nothing -> False
        Just so -> length so >= coverage

insertPointInSpace :: Integer -> TestParameters -> SpaceExploration -> SpaceExploration
insertPointInSpace coverage p s@SpaceExploration {..} =
  case constraintViolations p of
    [] ->
      if length shouldPass < coverage
        then s {shouldPass = p : shouldPass}
        else s
    [c] -> case M.lookup c singleDimFailures of
      Nothing -> s {singleDimFailures = M.insert c [p] singleDimFailures}
      Just so ->
        if length so < coverage
          then s {singleDimFailures = M.insert c (p : so) singleDimFailures}
          else s
    _ ->
      if length detritus < coverage
        then s {detritus = p : detritus}
        else s

spaceExploration ::
  forall (m :: Type -> Type).
  MonadGen m =>
  Integer ->
  StateT SpaceExploration m ()
spaceExploration coverage = do
  s <- get
  if spaceCovered coverage s
    then return ()
    else do
      p <- genTestParameters
      put $ insertPointInSpace coverage p s
      spaceExploration coverage

genFailingSingleConstraint :: forall (m :: Type -> Type). MonadGen m => Constraint -> m TestParameters
genFailingSingleConstraint constraint = do
  tp <- genTestParameters
  case constraintViolations tp of
    [c] | c Prelude.== constraint -> return tp
    _ -> genFailingSingleConstraint constraint

genTestParameters :: forall (m :: Type -> Type). MonadGen m => m TestParameters
genTestParameters = do
  (tlb, tub) <- genTimeRange
  w <- genKnownWalletIdx
  sp <- genSpenderParams
  ip <- genStateUTXOParams
  op <- genStateUTXOParams
  pc <- HP.builtinByteString (Range.linear 0 6)
  return $
    TestParameters
      { stateNFTCurrency = correctNFTCurrency --TODO generate
      , timeRangeLowerBound = tlb
      , timeRangeUpperBound = tub
      , ownerWallet = w
      , transactorParams = sp
      , inputParams = ip
      , outputParams = op
      , peggedCurrency = pc
      }

genSpenderParams :: forall (m :: Type -> Type). MonadGen m => m SpenderParams
genSpenderParams = do
  b <- Gen.bool
  if b
    then do
      w <- genKnownWalletIdx
      return $ JustSignedBy w
    else do
      -- hmmm janky ifology
      b' <- Gen.bool
      if b'
        then return NoSigner
        else do
          w <- genKnownWalletIdx
          v <- HP.value
          return $ SignedByWithValue w v

genStateUTXOParams :: forall (m :: Type -> Type). MonadGen m => m StateUTXOParams
genStateUTXOParams = do
  stok <- genStateToken
  d <- genTestDatumParameters
  return $ StateUTXOParams stok d

genKnownWalletIdx :: forall (m :: Type -> Type). MonadGen m => m Integer
genKnownWalletIdx = Gen.integral (Range.linear 1 5)

genTimeStamp :: forall (m :: Type -> Type). MonadGen m => m Integer
genTimeStamp = Gen.integral (Range.linear 0 100_000)

genTimeRange :: forall (m :: Type -> Type). MonadGen m => m (Integer, Integer)
genTimeRange = do
  t1 <- Gen.integral (Range.linear 0 99_000)
  t2 <- Gen.integral (Range.linear t1 100_000)
  return (t1, t2)

genTestDatumParameters :: forall (m :: Type -> Type). MonadGen m => m TestDatumParameters
genTestDatumParameters = do
  walletIdx <- genKnownWalletIdx
  timestamped <- genTimeStamp
  return $ TestDatumParameters walletIdx timestamped

genStateTokenCurrencySymbol :: forall (m :: Type -> Type). MonadGen m => m Value.CurrencySymbol
genStateTokenCurrencySymbol = do
  b <- Gen.bool
  if b
    then return $ fst correctNFTCurrency
    else HP.currencySymbol

genStateTokenTokenName :: forall (m :: Type -> Type). MonadGen m => m Value.TokenName
genStateTokenTokenName = do
  b <- Gen.bool
  if b
    then return $ snd correctNFTCurrency
    else HP.tokenName

genStateTokenAmount :: forall (m :: Type -> Type). MonadGen m => m Integer
genStateTokenAmount = Gen.integral (Range.linear 0 3)

genStateToken :: forall (m :: Type -> Type). MonadGen m => m Ledger.Value
genStateToken = do
  s <- genStateTokenCurrencySymbol
  n <- genStateTokenTokenName
  v <- genStateTokenAmount
  return $ singleton s n v

---- Constraints on the model domain
--
-- our model of why things should or should not validate

data Constraint
  = OutputDatumTimestampInRange
  | --  | InputDatumTimestampInRange
    RangeWithinSizeLimit
  | --  | InputDatumSignedByOwner
    OutputDatumSignedByOwner
  | TransactionSignedByOwner
  | StateTokenReturned
  deriving stock (Prelude.Enum, Prelude.Eq, Prelude.Ord, Prelude.Bounded, Prelude.Show)

type ModelCheck = TestParameters -> Bool

checkConstraint :: Constraint -> ModelCheck
--checkConstraint InputDatumTimestampInRange = inputDatumInRange
checkConstraint OutputDatumTimestampInRange = outputDatumInRange
checkConstraint RangeWithinSizeLimit = rangeWithinSizeLimit
--checkConstraint InputDatumSignedByOwner = inputSignedByOwner
checkConstraint OutputDatumSignedByOwner = outputDatumSignedByOwner
checkConstraint TransactionSignedByOwner = txSignedByOwner
checkConstraint StateTokenReturned = stateTokenReturned

constraintViolations :: TestParameters -> [Constraint]
constraintViolations p = filter (not . flip checkConstraint p) [minBound .. maxBound]

inputDatumInRange :: ModelCheck
inputDatumInRange TestParameters {..} =
  let so = stateDatumValue inputParams
   in timeStamp so >= timeRangeLowerBound && timeStamp so <= timeRangeUpperBound

outputDatumInRange :: ModelCheck
outputDatumInRange TestParameters {..} =
  let so = stateDatumValue outputParams
   in timeStamp so >= timeRangeLowerBound && timeStamp so <= timeRangeUpperBound

-- TODO clarify expected behaviour of this constraint
rangeWithinSizeLimit :: ModelCheck
rangeWithinSizeLimit TestParameters {..} =
  let rangeLen = timeRangeUpperBound - timeRangeLowerBound
   in rangeLen > 0 && rangeLen <= 10000

inputSignedByOwner :: ModelCheck
inputSignedByOwner TestParameters {..} =
  let so = stateDatumValue inputParams
   in signedByWallet so == ownerWallet

outputDatumSignedByOwner :: ModelCheck
outputDatumSignedByOwner TestParameters {..} =
  let d = stateDatumValue outputParams
   in signedByWallet d == ownerWallet

txSignedByOwner :: ModelCheck
txSignedByOwner TestParameters {..} = case transactorParams of
  NoSigner -> False
  JustSignedBy signer -> signer == ownerWallet
  SignedByWithValue signer _ -> signer == ownerWallet

stateTokenReturned :: ModelCheck
stateTokenReturned TestParameters {..} =
  case AssocMap.lookup mockCurrencySymbol $ getValue $ stateTokenValue outputParams of
    Nothing -> False
    Just so -> case AssocMap.lookup (snd stateNFTCurrency) so of
      Just 1 -> True
      _ -> False

---- Constraint composition forms the validation model
--
-- if all model constraints are satisfied we expect the test to pass

withExpectedResult :: TestParameters -> String -> TestData 'ForSpending -> ContextBuilder 'ForSpending -> WithScript 'ForSpending ()
withExpectedResult p | null (constraintViolations p) = shouldValidate
withExpectedResult _ = shouldn'tValidate

---- helpers
--

lookupPrivateKey :: Integer -> Ledger.PrivateKey
lookupPrivateKey i = Ledger.knownPrivateKeys!!(i - 1)

-- TODO parameterise/randomise the payload data - shouldn't matter what's in there
modelDatum :: TestDatumParameters -> Oracle.SignedMessage PriceTracking
modelDatum TestDatumParameters {..} =
  Oracle.signMessage (PriceTracking UniqueMap.empty UniqueMap.empty (Ledger.POSIXTime timeStamp)) signedByPrivK
  where
    signedByPrivK :: Ledger.PrivateKey
    signedByPrivK = lookupPrivateKey signedByWallet

modelTestData :: TestParameters -> TestData 'ForSpending
modelTestData TestParameters {..} =
  SpendingTest (modelDatum $ stateDatumValue inputParams) () $ stateTokenValue inputParams

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

scriptOutputAction :: StateUTXOParams -> ContextBuilder p
scriptOutputAction StateUTXOParams {..} =
  output (Output (OwnType . PlutusTx.toBuiltinData $ modelDatum stateDatumValue) stateTokenValue)

---- reification of the test using plutus-extra. mmmmm Tasty!
--

parametricValidatorTest :: NamedTestParameters -> TestTree
parametricValidatorTest ntp@NamedTestParameters {parameters = tp@TestParameters {..}, ..} =
  PlusTestOptions (setTimeRangeOption tp) $
    withValidator subTreeName validator $ do
      withExpectedResult tp testName (modelTestData tp) $
        transactorSpendingAction transactorParams $
          scriptOutputAction outputParams
  where
    ownerPubKey :: Ledger.PubKey
    ownerPubKey = walletPubKey (knownWallet ownerWallet)

    ownerPubKeyHash :: Ledger.PubKeyHash
    ownerPubKeyHash = pubKeyHash ownerPubKey

    params :: OracleValidatorParams
    params = OracleValidatorParams (fst stateNFTCurrency) ownerPubKey ownerPubKeyHash peggedCurrency

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