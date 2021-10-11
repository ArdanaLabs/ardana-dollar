{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.ArdanaDollar.PriceOracle.OnChain.Model.Checker
  ( parametricValidatorTest
  , nameGeneratedParams
  ) where

import ArdanaDollar.PriceOracle.OnChain

import Test.ArdanaDollar.PriceOracle.OnChain.Model.Constraints
import Test.ArdanaDollar.PriceOracle.OnChain.Model.Parameters

import Test.Tasty.Plutus.Context
import Test.Tasty.Plutus.Script.Unit
import Test.Tasty.Options (OptionSet, setOption)
import Test.Tasty.Runners (TestTree (..))

import Ledger qualified
import Ledger.Crypto (pubKeyHash)
import Ledger.Oracle qualified as Oracle
import Plutus.V1.Ledger.Api (mkValidatorScript)
import PlutusTx qualified
import PlutusTx.Prelude hiding (Semigroup (..))
import PlutusTx.UniqueMap qualified as UniqueMap
import Prelude (String,Show (..),Semigroup(..))
import Wallet.Emulator.Types (knownWallet, walletPubKey)


nameGeneratedParams :: TestParameters -> NamedTestParameters
nameGeneratedParams tp =
  NamedTestParameters
    { subTreeName = "Generated"
    , testName = "violations: " <> show (constraintViolations tp)
    , parameters = tp
    }

data NamedTestParameters = NamedTestParameters
  { subTreeName :: String
  , testName :: String
  , parameters :: TestParameters
  }
  deriving (Show)


lookupPrivateKey :: Integer -> Ledger.PrivateKey
lookupPrivateKey i = Ledger.knownPrivateKeys !! (i - 1)

-- TODO parameterise/randomise the payload data - it shouldn't matter what's in there
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

withExpectedResult :: TestParameters -> String -> TestData 'ForSpending -> ContextBuilder 'ForSpending -> WithScript 'ForSpending ()
withExpectedResult p | null (constraintViolations p) = shouldValidate
withExpectedResult _ = shouldn'tValidate

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

