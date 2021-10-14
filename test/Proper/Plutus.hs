{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Proper.Plutus (
  Proper (..),
  IsProperty,
  toTestValidator,
  FOL (..),
) where

import Control.Monad.Reader
 ( Reader,
   ask,
   runReader,
 )
import Data.Kind (
  Type,
 )
import Data.Set (
  Set,
 )
import Data.Set qualified as Set
import Data.String (
  fromString,
 )
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Functor.Identity
 ( Identity
 )
import Hedgehog (
  Group (..),
  MonadGen,
  MonadTest,
  GenBase,
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
  (<*>),
  (<>),
  (==),
  (>>),
  (>>=),
  (&&),
  (||),
  pure,
  elem,
  not,
 )

data FOL a =
      Atom Bool
    | Proposition a
    | Negation (FOL a)
    | Conjunction (FOL a) (FOL a)
    | Disjunction (FOL a) (FOL a)
    | Implication (FOL a) (FOL a)
    | IfAndOnlyIf (FOL a) (FOL a)

genGivenFOL :: IsProperty a => MonadGen m => GenBase m ~ Identity => FOL a -> m (Set a)
genGivenFOL f =
  let g = Set.fromList <$> Gen.subsequence [minBound..maxBound]
   in Gen.filter (satisfiesFOL f) g

satisfiesFOL :: Eq a => FOL a -> Set a -> Bool
satisfiesFOL l = runReader $ go l
  where
    go :: Eq a => FOL a -> Reader (Set a) Bool
    go (Atom a) = pure a
    go (Proposition a) = do
      ctx <- ask
      pure $ a `elem` ctx
    go (Negation a) = not <$> go a
    go (Conjunction a b) = (&&) <$> go a <*> go b
    go (Disjunction a b) = do
      ia <- go a
      ib <- go b
      pure $ ia || ib
    go (Implication a b) = do
      ia <- go a
      if ia then go b else pure True
    go (IfAndOnlyIf a b) = do
      ia <- go a
      ib <- go b
      pure $ (not ia && not ib) || (ia && ib)

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
    Set (Property model)
  hasProperties x = Set.fromList $ filter (hasProperty x) [minBound .. maxBound]

  -- properties may be in a positive or negative context
  -- by default all properties are negative and should cause a failure
  shouldCauseFailure :: Property model -> Bool
  shouldCauseFailure _ = True

  expect :: Set (Property model) -> Result
  expect p = if or (shouldCauseFailure <$> Set.toList p) then Fail else Pass

  -- generate a model that satisfies specified properties
  genModel :: MonadGen m => Set (Property model) -> m (Model model)

  -- genProperties has a sensible default but some properties may be mutually exclusive
  -- so this can be overridden
  genProperties ::
    MonadGen m =>
    GenBase m ~ Identity =>
    IsProperty (Property model) =>
    model ->
    m (Set (Property model))
  genProperties _ = genGivenFOL logic

  -- some properties imply others
  logic :: FOL (Property model)
  logic = Atom True

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
          (Fail, Just "Fail") -> success
          (Pass, Just "Pass") -> success
          (Pass, Just t) ->
            if t == "Fail"
              then failWithFootnote unexpectedFailure
              else case Text.stripPrefix "Parse failed: " t of
                Nothing -> failWithFootnote $ internalError t
                Just t' -> failWithFootnote $ noParse t'
          (Fail, Just t) ->
            if t == "Pass"
              then failWithFootnote unexpectedSuccess
              else case Text.stripPrefix "Parse failed: " t of
                Nothing -> failWithFootnote $ internalError t
                Just _ -> success
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
    Set (Property model) ->
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
      [ (fromString "selfTestRandomProperties", selfTestAll model)
      , (fromString "selfTestNoProperties", selfTestGivenProperties (Set.empty :: Set (Property model)))
      ]
        <> [ ( fromString $ "selfTestSingleProperty" <> "_" <> show property'
             , selfTestGivenProperties $ Set.singleton property'
             )
           | property' <- ([minBound .. maxBound] :: [Property model])
           , satisfiesFOL logic (Set.singleton property')
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
    Set (Property model) ->
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
      [ (fromString "validatorTestRandomProperties", validatorTestAll model)
      , (fromString "validatorTestNullProperties", validatorTestGivenProperties (Set.empty :: Set (Property model)))
      ]
        <> [ ( fromString $ "validatorTestSingleProperty" <> "_" <> show property'
             , validatorTestGivenProperties $ Set.singleton property'
             )
           | property' <- ([minBound .. maxBound] :: [Property model])
           , satisfiesFOL logic (Set.singleton property')
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
