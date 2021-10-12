{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.ArdanaDollar.PriceOracle.OnChain.Model.Reification (
  runValidatorTest
) where

import ArdanaDollar.PriceOracle.OnChain

import Test.ArdanaDollar.PriceOracle.OnChain.Model.Constraints
import Test.ArdanaDollar.PriceOracle.OnChain.Model.Parameters

import Data.Kind (Type)
import Prelude --(Maybe(..),Either(..),Bool(..),fst,($),(.),Integer)
import Hedgehog

import Safe (lastMay)
import Data.Text (Text)
import qualified Data.Text as Text
import Text.PrettyPrint (
  Doc,
  Style (lineLength),
  colon,
  hang,
  int,
  renderStyle,
  style,
  text,
  vcat,
  ($+$),
  (<+>),
 )
import Text.Show.Pretty (ppDoc)
import PlutusTx.UniqueMap qualified as UniqueMap
import Ledger.Address (scriptHashAddress)
import Ledger (
  Context (..),
  Datum (..),
  DatumHash,
  datumHash,
  Redeemer (..),
  PubKey,
  PubKeyHash,
  POSIXTime (..),
  LowerBound (..),
  UpperBound (..),
  Extended( Finite ),
  PrivateKey,
  TxOutRef (..),
  TxOut (..),
  TxInInfo (..),
  TxId (..),
  Value,
  ValidatorHash,
  pubKeyHash,
  knownPrivateKeys,
  TxId,
  pubKeyHashAddress,
  Interval (..), 
  )
import Plutus.V1.Ledger.Api (
   POSIXTimeRange,
  )
import Plutus.V1.Ledger.Contexts (
  ScriptPurpose (Spending),
  ScriptContext (..),
  TxInfo (..),
  )
import Wallet.Emulator.Wallet (
  knownWallet,
  walletPubKey,
  )
import Ledger.Oracle (
  SignedMessage,
  signMessage,
  )
import PlutusTx (
  applyCode,
  compile,
  toBuiltinData,
  )
import PlutusTx.Builtins (
  BuiltinData,
  BuiltinString,
  appendString,
  trace,
 )
import PlutusTx.IsData.Class (
  FromData (fromBuiltinData),
 )
import Plutus.V1.Ledger.Scripts (
  Validator,
  mkValidatorScript,
  runScript,
 )


---- ripped from Tasty.Plutus

{-# INLINEABLE toTestValidator #-}
toTestValidator ::
  forall (datum :: Type) (redeemer :: Type).
  (FromData datum, FromData redeemer) =>
  (datum -> redeemer -> ScriptContext -> Bool) ->
  (BuiltinData -> BuiltinData -> BuiltinData -> ())
toTestValidator f d r p = case fromBuiltinData d of
  Nothing -> reportParseFailed "Datum"
  Just d' -> case fromBuiltinData r of
    Nothing -> reportParseFailed "Redeemer"
    Just r' -> case fromBuiltinData p of
      Nothing -> reportParseFailed "ScriptContext"
      Just p' ->
        if f d' r' p'
          then reportPass
          else reportFail

{-# INLINEABLE reportParseFailed #-}
reportParseFailed :: BuiltinString -> ()
reportParseFailed what = report ("Parse failed: " `appendString` what)

{-# INLINEABLE reportPass #-}
reportPass :: ()
reportPass = report "Pass"

{-# INLINEABLE reportFail #-}
reportFail :: ()
reportFail = report "Fail"

{-# INLINEABLE report #-}
report :: BuiltinString -> ()
report what = trace ("hedgehog-plutus: " `appendString` what) ()

--- Tasty.Plutus.Context.Internal helpers

datumWithHash :: BuiltinData -> (DatumHash, Datum)
datumWithHash dt = (datumHash dt', dt')
  where
    dt' :: Datum
    dt' = Datum dt

justDatumHash :: BuiltinData -> Maybe DatumHash
justDatumHash = Just . datumHash . Datum

----- wrapping the oracle validator a la Tasty.Plutus

validator :: TestParameters -> Validator
validator tp@TestParameters {..} =
  mkValidatorScript $
    $$(compile [||go||])
      `applyCode` oracleCompiledTypedValidator params
  where
    {-# INLINEABLE go #-}
    go ::
      (SignedMessage PriceTracking -> () -> ScriptContext -> Bool) ->
      (BuiltinData -> BuiltinData -> BuiltinData -> ())
    go = toTestValidator

    ownerPubKey :: PubKey
    ownerPubKey = walletPubKey (knownWallet ownerWallet)

    ownerPubKeyHash :: PubKeyHash
    ownerPubKeyHash = pubKeyHash ownerPubKey

    params :: OracleValidatorParams
    params = OracleValidatorParams (fst stateNFTCurrency) ownerPubKey ownerPubKeyHash peggedCurrency

------ absolute hackery

runValidatorTest :: MonadTest m => TestParameters -> m Bool
runValidatorTest params =
  case runScript ctx val dat red of
    Left err -> footnoteShow err >> return False
    Right (_,logs) -> deliverResult params ctx logs
  where
    val = validator params
    ctx = buildCtx params
    dat = Datum $ buildInputDatum params
    red = buildRedeemer params

deliverResult :: MonadTest m => TestParameters -> Context -> [Text] -> m Bool
deliverResult p ctx logs =
  let shouldPass = null $ constraintViolations p
   in case (shouldPass, lastMay logs >>= Text.stripPrefix "hedgehog-plutus: ") of
        (_, Nothing) -> failWithFootnote noOutcome
        (False, Just "Pass") -> failWithFootnote unexpectedSuccess
        (False, Just "Fail") -> return True
        (True, Just "Pass") -> return True
        (True, Just "Fail") -> failWithFootnote unexpectedFailure
        (_, Just t) -> case Text.stripPrefix "Parse failed: " t of
          Nothing -> failWithFootnote $ internalError t
          Just t' -> failWithFootnote $ noParse t'
  where
    failWithFootnote :: MonadTest m => String -> m Bool
    failWithFootnote s = footnote s >> return False
    noOutcome :: String
    noOutcome =
      renderStyle ourStyle $
        "No outcome from run"
          $+$ dumpState
          $+$ ""
          $+$ "Did you forget to use toTestValidator or toTestMintingPolicy?"
    unexpectedSuccess :: String
    unexpectedSuccess =
      renderStyle ourStyle $
        "Unexpected success" $+$ dumpState
    unexpectedFailure :: String
    unexpectedFailure =
      renderStyle ourStyle $
        "Unexpected failure" $+$ dumpState
    internalError :: Text -> String
    internalError msg =
      renderStyle ourStyle $
        ("Internal error" <+> (text . show $ msg)) $+$ dumpState
    noParse :: Text -> String
    noParse what =
      renderStyle ourStyle $
        ((text . show $ what) <+> "did not parse") $+$ dumpState
    dumpState :: Doc
    dumpState =
      ""
        $+$ hang "Context" 4 (ppDoc ctx)
        $+$ hang "Inputs" 4 dumpInputs
        $+$ hang "Logs" 4 dumpLogs
        $+$ hang "Expected violations" 4 (ppDoc $ constraintViolations p)
    dumpInputs :: Doc
    dumpInputs =
      "Parameters"
          $+$ ppDoc p
    dumpLogs :: Doc
    dumpLogs = vcat . fmap go . zip [1 ..] $ logs
    go :: (Int, Text) -> Doc
    go (ix, line) = (int ix <> colon) <+> (text . show $ line)
    ourStyle :: Style
    ourStyle = style {lineLength = 80}


testTxId :: TestParameters -> TxId
testTxId _ = "abcd"

testValidatorHash :: TestParameters -> ValidatorHash
testValidatorHash _ = "90ab"

testSpendyVal :: TestParameters -> Value
testSpendyVal _ = mempty

testFee :: TestParameters -> Value
testFee _ = mempty

buildCtx :: TestParameters -> Context
buildCtx tp@TestParameters { .. } = Context . toBuiltinData $ context
  where
    context :: ScriptContext
    context = ScriptContext go
                . Spending
                . TxOutRef (testTxId tp)
                $ 0
    go :: TxInfo
    go =
      let dt = toBuiltinData . modelDatum . stateDatumValue $ inputParams
          baseInfo = baseTxInfo tp
          inInfo = scriptTxInInfo tp
          inData = datumWithHash dt
       in baseInfo
            { txInfoInputs = inInfo : txInfoInputs baseInfo
            , txInfoData = inData : txInfoData baseInfo
            }

testTimeRange :: TestParameters -> POSIXTimeRange
testTimeRange TestParameters { .. } = Interval
            (LowerBound (Finite (Ledger.POSIXTime timeRangeLowerBound)) True)
            (UpperBound (Finite (Ledger.POSIXTime timeRangeUpperBound)) True)

baseTxInfo :: TestParameters -> TxInfo
baseTxInfo tp@TestParameters { } =
  let valHash = testValidatorHash tp
   in TxInfo
        { txInfoInputs = scriptTxInInfo tp : spenderTxInInfo tp
        , txInfoOutputs = [scriptTxOut valHash tp]
        , txInfoFee = testFee tp
        , txInfoMint = mempty
        , txInfoDCert = []
        , txInfoWdrl = []
        , txInfoValidRange = testTimeRange tp
        , txInfoSignatories = txSignatories tp
        , txInfoData = datumWithHash <$> [buildInputDatum tp, buildOutputDatum tp]
        , txInfoId = TxId "testTx"
        }

txSignatories :: TestParameters -> [PubKeyHash]
txSignatories TestParameters { .. } =
    case transactorParams of
      NoSigner -> []
      JustSignedBy signer -> [go signer]
      SignedByWithValue signer _ -> [go signer]
  where
    go = pubKeyHash . walletPubKey . knownWallet

scriptTxInInfo :: TestParameters -> TxInInfo
scriptTxInInfo tp@TestParameters { .. } =
  let dat = toBuiltinData . modelDatum . stateDatumValue $ inputParams
      v = stateTokenValue inputParams
  in TxInInfo (TxOutRef (testTxId tp) 0) $
       TxOut (scriptHashAddress . testValidatorHash $ tp) v . justDatumHash $ dat

spenderTxInInfo :: TestParameters -> [TxInInfo]
spenderTxInInfo tp@TestParameters { .. } =
  TxInInfo (TxOutRef (testTxId tp) 1) <$>
    case transactorParams of
      NoSigner -> []
      JustSignedBy signer -> [TxOut (go signer) mempty Nothing]
      SignedByWithValue signer v -> [TxOut (go signer) v Nothing]
  where
    go = pubKeyHashAddress . pubKeyHash . walletPubKey . knownWallet

scriptTxOut :: ValidatorHash -> TestParameters -> TxOut
scriptTxOut valHash tp@TestParameters{ .. } =
  TxOut (scriptHashAddress valHash) (stateTokenValue outputParams) . justDatumHash $ buildOutputDatum tp

buildInputDatum :: TestParameters -> BuiltinData
buildInputDatum TestParameters { .. } =
  toBuiltinData . modelDatum . stateDatumValue $ inputParams

buildOutputDatum :: TestParameters -> BuiltinData
buildOutputDatum TestParameters { .. } =
  toBuiltinData . modelDatum . stateDatumValue $ outputParams

-- TODO parameterise/randomise the payload data - it shouldn't matter what's in there
modelDatum :: TestDatumParameters -> SignedMessage PriceTracking
modelDatum TestDatumParameters {..} =
  signMessage (PriceTracking UniqueMap.empty UniqueMap.empty (POSIXTime timeStamp)) signedByPrivK
  where
    signedByPrivK :: PrivateKey
    signedByPrivK = lookupPrivateKey signedByWallet
    lookupPrivateKey :: Integer -> Ledger.PrivateKey
    lookupPrivateKey i = knownPrivateKeys !! fromInteger (i - 1)


buildRedeemer :: TestParameters -> Redeemer
buildRedeemer _ = Redeemer . toBuiltinData $ ()


