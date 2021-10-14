{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Proper.Plutus (
  Proper (..),
  IsProperty,
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
  failure,
  footnote,
  footnoteShow,
  forAll,
  property,
  success,
  (===),
 )
import Hedgehog qualified
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
  mkValidatorScript,
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
  or,
  snd,
  zip,
  ($),
  (.),
  (<$>),
  (<>),
  (>>),
  (>>=),
 )

defaultValidator :: Validator
defaultValidator =
  mkValidatorScript $
    $$(compile [||go||])
      `applyCode` $$(compile [||\_ _ _ -> False||])
  where
    {-# INLINEABLE go #-}
    go ::
      (() -> () -> ScriptContext -> Bool) ->
      (BuiltinData -> BuiltinData -> BuiltinData -> ())
    go = toTestValidator

-- these things combined make a valid Property
-- perhaps this should be relaxed to allow richer property types
class (Enum c, Eq c, Ord c, Bounded c, Show c) => IsProperty c

data Result = Pass | Fail deriving (Show)

class Proper model where
  -- a model for a validator context
  data Model model :: Type

  -- properties are things that may be true of a model
  data Property model :: Type

  -- check whether a property is satisfied
  hasProperty :: Model model -> Property model -> Bool

  hasProperties ::
    IsProperty (Property model) =>
    Model model ->
    [Property model]
  hasProperties x = filter (hasProperty x) [minBound .. maxBound]

  -- properties may be in a positive or negative context
  -- by default all properties are negative and should cause a failure
  shouldCauseFailure :: Property model -> Bool
  shouldCauseFailure _ = True

  expect :: [Property model] -> Result
  expect p = if or (shouldCauseFailure <$> p) then Fail else Pass

  -- generate a model that satisfies specified properties
  genModel :: MonadGen m => [Property model] -> m (Model model)

  -- genProperties has a sensible default but some properties may be mutually exclusive
  -- so this can be overridden
  genProperties ::
    MonadGen m =>
    IsProperty (Property model) =>
    model ->
    m [Property model]
  genProperties _ = Gen.subsequence [minBound .. maxBound]

  -- to test a validator specify how to build one from the model
  -- a default is provided to enable construction of the model before the validator is written
  validator :: Model model -> Validator
  validator _ = defaultValidator

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

  -- Context Api

  -- to specify a context override the default implementations in the context api
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

  reifyTest :: Show (Model model) => IsProperty (Property model) => MonadTest t => Model model -> t ()
  reifyTest model =
    case runScript ctx val dat red of
      Left err -> footnoteShow err >> failure
      Right (_, logs) -> deliverResult model ctx logs
    where
      val = validator model
      ctx = hasCtx model
      dat = hasDatum model
      red = hasRedeemer model

  deliverResult :: Show (Model model) => IsProperty (Property model) => MonadTest m => Model model -> Context -> [Text] -> m ()
  deliverResult p ctx logs =
    let shouldPass = expect $ hasProperties p
     in case (shouldPass, lastMay logs >>= Text.stripPrefix "proper-plutus: ") of
          (_, Nothing) -> failWithFootnote noOutcome
          (Fail, Just "Pass") -> failWithFootnote unexpectedSuccess
          (Fail, Just "Fail") -> success
          (Pass, Just "Pass") -> success
          (Pass, Just "Fail") -> failWithFootnote unexpectedFailure
          -- TODO perhaps we want to test for parse failures?
          -- e.g. with a property like HasIncorrectDatum
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
          $+$ hang "Expected " 4 (ppDoc $ expect $ hasProperties p)
          $+$ hang "Properties " 4 (ppDoc $ hasProperties p)
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

  -- HedgeHog property tests
  --
  selfTestAll ::
    IsProperty (Property model) =>
    Show (Model model) =>
    model ->
    Hedgehog.Property
  selfTestAll m =
    property $ do
      properties' <- forAll $ genProperties m
      model <- forAll $ genModel properties'
      hasProperties model === properties'

  selfTestGivenProperties ::
    IsProperty (Property model) =>
    Show (Model model) =>
    [Property model] ->
    Hedgehog.Property
  selfTestGivenProperties properties' =
    property $ do
      model <- forAll $ genModel properties'
      hasProperties model === properties'

  selfTestGroup ::
    IsProperty (Property model) =>
    Show (Model model) =>
    Show model =>
    model ->
    Group
  selfTestGroup model =
    Group (fromString $ show model) $
      [ (fromString $ "selfTestAll_" <> show model, selfTestAll model)
      , (fromString $ "selfTestSuccesses_" <> show model, selfTestGivenProperties ([] :: [Property model]))
      ]
        <> [ ( fromString $ "selfTestSingleViolation_" <> show model <> "_" <> show property'
             , selfTestGivenProperties [property']
             )
           | property' <- ([minBound .. maxBound] :: [Property model])
           ]

  validatorTestAll ::
    IsProperty (Property model) =>
    Show (Model model) =>
    model ->
    Hedgehog.Property
  validatorTestAll m =
    property $ do
      properties' <- forAll $ genProperties m
      model <- forAll $ genModel properties'
      reifyTest model

  validatorTestGivenProperties ::
    IsProperty (Property model) =>
    Show (Model model) =>
    [Property model] ->
    Hedgehog.Property
  validatorTestGivenProperties properties' =
    property $ do
      model <- forAll $ genModel properties'
      reifyTest model

  validatorTestGroup ::
    IsProperty (Property model) =>
    Show (Model model) =>
    Show model =>
    model ->
    Group
  validatorTestGroup model =
    Group (fromString $ show model) $
      [ (fromString $ "validatorTestAll_" <> show model, validatorTestAll model)
      , (fromString $ "validatorTestSuccesses_" <> show model, validatorTestGivenProperties ([] :: [Property model]))
      ]
        <> [ ( fromString $ "validatorTestSingleViolation_" <> show model <> "_" <> show property'
             , validatorTestGivenProperties [property']
             )
           | property' <- ([minBound .. maxBound] :: [Property model])
           ]

-- helpers

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
