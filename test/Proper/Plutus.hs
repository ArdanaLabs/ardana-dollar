{-# LANGUAGE TypeFamilies #-}

module Proper.Plutus (
  Proper (..),
  IsCheck,
  toTestValidator,
) where

import Data.Kind (
  Type,
 )
import Data.String (
  fromString,
 )
import Data.Text (Text)
import Data.Text qualified as Text
import Hedgehog (
  Group (..),
  MonadGen,
  MonadTest,
  Property,
  failure,
  footnote,
  footnoteShow,
  forAll,
  property,
  success,
  (===),
 )
import Hedgehog.Gen qualified as Gen
import Ledger (
  Datum (..),
  DatumHash,
  PubKeyHash,
  Redeemer (..),
  TxId (..),
  TxInInfo (..),
  TxOut (..),
  TxOutRef (..),
  Value,
  always,
  datumHash,
  runScript,
 )
import Ledger.Address (scriptHashAddress)
import Plutus.V1.Ledger.Api (
  FromData (fromBuiltinData),
  POSIXTimeRange,
 )
import Plutus.V1.Ledger.Contexts (
  ScriptContext (..),
  ScriptPurpose (Spending),
  TxInfo (..),
 )
import Plutus.V1.Ledger.Scripts (
  Context (..),
  Validator,
  ValidatorHash,
 )
import PlutusTx (
  toBuiltinData,
 )
import PlutusTx.Builtins (
  BuiltinData,
  BuiltinString,
  appendString,
  trace,
 )
import Safe (
  headMay,
  lastMay,
 )
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
import Prelude (
  Bool (..),
  Bounded (..),
  Either (..),
  Enum,
  Eq,
  Int,
  Maybe (..),
  Ord,
  Show (..),
  String,
  filter,
  fmap,
  mempty,
  not,
  null,
  snd,
  zip,
  ($),
  (.),
  (<$>),
  (<>),
  (>>),
  (>>=),
 )

-- these things combined make a valid Check
class (Enum c, Eq c, Ord c, Bounded c, Show c) => IsCheck c

class Proper model where
  -- a model for a validator context
  data Model model :: Type

  -- checks are constraints that must hold in order for the validator to validate
  data Check model :: Type

  -- a model may either pass or fail a check
  check :: Model model -> Check model -> Bool

  -- a model can be generated that fails specified checks
  genModel :: MonadGen m => [Check model] -> m (Model model)

  -- to test a validator you must specify how to build one from the model
  validator :: Model model -> Validator

  --e.g.
  --validator model@MyModel{..} =
  --  mkValidatorScript $
  --    $$(compile [||go||])
  --      `applyCode` myValidator (buildParams model)
  --  where
  --    {-# INLINEABLE go #-}
  --    go ::
  --      (MyDatum -> MyRedeemer -> ScriptContext -> Bool) ->
  --      (BuiltinData -> BuiltinData -> BuiltinData -> ())
  --    go = toTestValidator

  -- to specify a context override the default implementations in the context api to your liking
  -- (this is below the property tests)

  -- the property tests
  checkViolations ::
    IsCheck (Check model) =>
    Model model ->
    [Check model]
  checkViolations x = filter (not . check x) [minBound .. maxBound]

  genChecks ::
    MonadGen m =>
    IsCheck (Check model) =>
    model ->
    m [Check model]
  genChecks _ = Gen.subsequence [minBound .. maxBound]

  selfTestAll ::
    IsCheck (Check model) =>
    Show (Model model) =>
    model ->
    Property
  selfTestAll m =
    property $ do
      violations <- forAll $ genChecks m
      model <- forAll $ genModel violations
      checkViolations model === violations

  selfTestGivenViolations ::
    IsCheck (Check model) =>
    Show (Model model) =>
    [Check model] ->
    Property
  selfTestGivenViolations violations =
    property $ do
      model <- forAll $ genModel violations
      checkViolations model === violations

  selfTestGroup ::
    IsCheck (Check model) =>
    Show (Model model) =>
    Show model =>
    model ->
    Group
  selfTestGroup model =
    Group (fromString $ show model) $
      [ (fromString $ "selfTestAll_" <> show model, selfTestAll model)
      , (fromString $ "selfTestSuccesses_" <> show model, selfTestGivenViolations ([] :: [Check model]))
      ]
        <> [ ( fromString $ "selfTestSingleViolation_" <> show model <> "_" <> show violation
             , selfTestGivenViolations [violation]
             )
           | violation <- ([minBound .. maxBound] :: [Check model])
           ]

  validatorTestAll ::
    IsCheck (Check model) =>
    Show (Model model) =>
    model ->
    Property
  validatorTestAll m =
    property $ do
      violations <- forAll $ genChecks m
      model <- forAll $ genModel violations
      reify model

  validatorTestGivenViolations ::
    IsCheck (Check model) =>
    Show (Model model) =>
    [Check model] ->
    Property
  validatorTestGivenViolations violations =
    property $ do
      model <- forAll $ genModel violations
      reify model

  validatorTestGroup ::
    IsCheck (Check model) =>
    Show (Model model) =>
    Show model =>
    model ->
    Group
  validatorTestGroup model =
    Group (fromString $ show model) $
      [ (fromString $ "validatorTestAll_" <> show model, validatorTestAll model)
      , (fromString $ "validatorTestSuccesses_" <> show model, validatorTestGivenViolations ([] :: [Check model]))
      ]
        <> [ ( fromString $ "validatorTestSingleViolation_" <> show model <> "_" <> show violation
             , validatorTestGivenViolations [violation]
             )
           | violation <- ([minBound .. maxBound] :: [Check model])
           ]

  -- all of this Context generation api could be improved
  -- the idea is to give lots of options for overriding while having some sensible defaults

  hasTxId :: Model model -> TxId
  hasTxId _ = "abcd"

  hasValidatorHash :: Model model -> ValidatorHash
  hasValidatorHash _ = "90ab"

  hasSpendingValue :: Model model -> Value
  hasSpendingValue _ = mempty

  hasTxInfoId :: Model model -> TxId
  hasTxInfoId _ = TxId "testTx"

  hasFee :: Model model -> Value
  hasFee _ = mempty

  hasInputData :: Model model -> [(Value, BuiltinData)]
  hasInputData _ = []

  hasOutputData :: Model model -> [(Value, BuiltinData)]
  hasOutputData _ = []

  hasTxSignatories :: Model model -> [PubKeyHash]
  hasTxSignatories _ = []

  hasTimeRange :: Model model -> POSIXTimeRange
  hasTimeRange _ = always

  hasBaseTxInfo :: Model model -> TxInfo
  hasBaseTxInfo model =
    TxInfo
      { txInfoInputs = hasScriptTxInInfo model <> hasSpenderTxInInfo model
      , txInfoOutputs = hasTxOuts model
      , txInfoFee = hasFee model
      , txInfoMint = mempty
      , txInfoDCert = []
      , txInfoWdrl = []
      , txInfoValidRange = hasTimeRange model
      , txInfoSignatories = hasTxSignatories model
      , txInfoData = datumWithHash <$> (snd <$> hasInputData model) <> (snd <$> hasOutputData model)
      , txInfoId = hasTxInfoId model
      }

  hasTxOuts :: Model model -> [TxOut]
  hasTxOuts model =
    (\(v, d) -> TxOut (scriptHashAddress $ hasValidatorHash model) v (justDatumHash d)) <$> hasOutputData model

  hasSpenderTxInInfo :: Model model -> [TxInInfo]
  hasSpenderTxInInfo _ = []

  hasScriptTxInInfo :: Model model -> [TxInInfo]
  hasScriptTxInInfo model =
    let sha = scriptHashAddress $ hasValidatorHash model
     in ( \(v, d) ->
            TxInInfo (TxOutRef (hasTxId model) 0) $
              TxOut sha v $ justDatumHash d
        )
          <$> hasInputData model

  hasCtx :: Model model -> Context
  hasCtx model = Context . toBuiltinData $ context
    where
      context :: ScriptContext
      context =
        ScriptContext go
          . Spending
          . TxOutRef (hasTxId model)
          $ 0
      go :: TxInfo
      go =
        let baseInfo = hasBaseTxInfo model
            inInfo = hasScriptTxInInfo model
            inData = datumWithHash . snd <$> hasInputData model
         in baseInfo
              { txInfoInputs = inInfo <> txInfoInputs baseInfo
              , txInfoData = inData <> txInfoData baseInfo
              }

  hasRedeemer :: Model model -> Redeemer
  hasRedeemer _ = Redeemer $ toBuiltinData ()

  hasDatum :: Model model -> Datum
  hasDatum model =
    case headMay (hasInputData model) of
      Nothing -> Datum $ toBuiltinData ()
      Just (_, so) -> Datum so

  reify :: Show (Model model) => IsCheck (Check model) => MonadTest t => Model model -> t ()
  reify model =
    case runScript ctx val dat red of
      Left err -> footnoteShow err >> failure
      Right (_, logs) -> deliverResult model ctx logs
    where
      val = validator model
      ctx = hasCtx model
      dat = hasDatum model
      red = hasRedeemer model

  deliverResult :: Show (Model model) => IsCheck (Check model) => MonadTest m => Model model -> Context -> [Text] -> m ()
  deliverResult p ctx logs =
    let shouldPass = null $ checkViolations p
     in case (shouldPass, lastMay logs >>= Text.stripPrefix "proper-plutus: ") of
          (_, Nothing) -> failWithFootnote noOutcome
          (False, Just "Pass") -> failWithFootnote unexpectedSuccess
          (False, Just "Fail") -> success
          (True, Just "Pass") -> success
          (True, Just "Fail") -> failWithFootnote unexpectedFailure
          (_, Just t) -> case Text.stripPrefix "Parse failed: " t of
            Nothing -> failWithFootnote $ internalError t
            Just t' -> failWithFootnote $ noParse t'
    where
      failWithFootnote :: MonadTest m => String -> m ()
      failWithFootnote s = footnote s >> failure
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
          $+$ hang "Expected violations" 4 (ppDoc $ checkViolations p)
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

datumWithHash :: BuiltinData -> (DatumHash, Datum)
datumWithHash dt = (datumHash dt', dt')
  where
    dt' :: Datum
    dt' = Datum dt

justDatumHash :: BuiltinData -> Maybe DatumHash
justDatumHash = Just . datumHash . Datum

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
report what = trace ("proper-plutus: " `appendString` what) ()
