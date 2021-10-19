{-# LANGUAGE TypeFamilies #-}

--------------------------------------------------------------------------------
-- Proper.Plutus encourages you to define a model before writing any Plutus
--------------------------------------------------------------------------------
module Proper.Plutus (
  Proper (..),
  IsProperty,
  toTestValidator,
  toTestMintingPolicy,
  toTestStakeValidator,
  justDatumHash,
  PropLogic (..),
  (\/),
  (/\),
  (-->),
  (<->),
  noneOf,
  anyOf,
  allOf,
  oneOf,
) where

import Control.Monad.Reader (
  Reader,
  ask,
  runReader,
 )
import Data.Functor.Identity (Identity)
import Data.Kind (Type)
import Data.List (subsequences)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as Text
import Hedgehog (
  GenBase,
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
  evaluateScript,
 )
import Ledger.Address (scriptHashAddress)
import Plutus.V1.Ledger.Api (
  DCert,
  FromData (fromBuiltinData),
  POSIXTimeRange,
  StakingCredential,
 )
import Plutus.V1.Ledger.Contexts (
  ScriptContext (..),
  ScriptPurpose (Spending),
  TxInfo (..),
 )
import Plutus.V1.Ledger.Scripts (
  Context (..),
  Script,
  ValidatorHash,
 )
import PlutusCore.Evaluation.Machine.ExBudget (ExBudget (..))
import PlutusCore.Evaluation.Machine.ExMemory (ExCPU (..), ExMemory (..))
import PlutusTx (toBuiltinData)
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
  Functor,
  Int,
  Integer,
  Maybe (..),
  Ord,
  Show (..),
  String,
  elem,
  filter,
  fmap,
  foldr,
  length,
  mempty,
  not,
  pure,
  snd,
  zip,
  ($),
  (&&),
  (.),
  (/=),
  (<$>),
  (<*>),
  (<=),
  (<>),
  (==),
  (>>),
  (>>=),
  (||),
 )

--------------------------------------------------------------------------------
-- Propositional logic is used to define two aspects of a model.
-- The expected outcome of a test and the sets of properties which are valid in
-- conjunction.
--------------------------------------------------------------------------------

class (Enum c, Eq c, Ord c, Bounded c, Show c) => IsProperty c

data PropLogic a
  = Atom Bool
  | Prop a
  | Neg (PropLogic a)
  | Conjunction (PropLogic a) (PropLogic a)
  | Disjunction (PropLogic a) (PropLogic a)
  | Implication (PropLogic a) (PropLogic a)
  | IfAndOnlyIf (PropLogic a) (PropLogic a)
  deriving (Functor)

(/\) :: PropLogic a -> PropLogic a -> PropLogic a
(/\) = Conjunction

(\/) :: PropLogic a -> PropLogic a -> PropLogic a
(\/) = Disjunction

(-->) :: PropLogic a -> PropLogic a -> PropLogic a
(-->) = Implication

(<->) :: PropLogic a -> PropLogic a -> PropLogic a
(<->) = IfAndOnlyIf

anyOf :: [PropLogic a] -> PropLogic a
anyOf = foldr (\/) (Atom False)

allOf :: [PropLogic a] -> PropLogic a
allOf = foldr (/\) (Atom True)

noneOf :: [PropLogic a] -> PropLogic a
noneOf ps = allOf (Neg <$> ps)

oneOf :: (Eq a) => [a] -> PropLogic a
oneOf ps = anyOf (Prop <$> ps) /\ allOf [Prop p --> noneOf (Prop <$> filter (/= p) ps) | p <- ps]

genGivenPropLogic :: (IsProperty a, MonadGen m, GenBase m ~ Identity) => PropLogic a -> m (Set a)
genGivenPropLogic f =
  let g = Set.fromList <$> Gen.subsequence [minBound .. maxBound]
   in Gen.filter (satisfiesPropLogic f) g

satisfiesPropLogic :: Eq a => PropLogic a -> Set a -> Bool
satisfiesPropLogic l = runReader $ go l
  where
    go :: Eq a => PropLogic a -> Reader (Set a) Bool
    go (Atom a) = pure a
    go (Prop a) = do
      ctx <- ask
      pure $ a `elem` ctx
    go (Neg a) = not <$> go a
    go (Conjunction a b) = (&&) <$> go a <*> go b
    go (Disjunction a b) = (||) <$> go a <*> go b
    go (Implication a b) = do
      ia <- go a
      if ia then go b else pure True
    go (IfAndOnlyIf a b) = do
      ia <- go a
      ib <- go b
      pure $ (not ia && not ib) || (ia && ib)

-- Proper is a type family over a Model and its Properties
-- It encapsulates the model checking pattern shown in this diagram
--
-- PropLogic (Property model)        Set (Property model)
--            \                    /       ^
--             \                  /         \
--              \ genP         = /           \ satisfies
--               \              /      A      \
--                \            /               \
--                 v          /      genM       \
--          Set (Property model) -------------> Model model
--                 |                                    \
--                 |                                     \
--         expect  |                  B                   \ translate
--                 |                                       \
--                 v          =                 eval        v
--               Result ------------- Result <----------- Script
--
-- 'A' checks consistency between the model specification and its generator.
-- 'B' (which can be written after 'A' is complete) tests the compiled script.

class Proper model where
  -- a model encodes the data relevant to a specification
  data Model model :: Type

  -- properties are things that may be true of a model
  data Property model :: Type

  -- propositional logic over model properties defines sets of properties valid in conjunction
  logic :: PropLogic (Property model)
  logic = Atom True

  -- given a set of properties we expect a script to pass or fail
  expect :: PropLogic (Property model)
  expect = Atom True

  -- check whether a property is satisfied
  satisfiesProperty :: Model model -> Property model -> Bool

  -- generates a model that satisfies a set of properties
  genModel :: MonadGen m => Set (Property model) -> m (Model model)

  -- compute the properties of a model
  properties ::
    IsProperty (Property model) =>
    Model model ->
    Set (Property model)
  properties x = Set.fromList $ filter (satisfiesProperty x) [minBound .. maxBound]

  -- generates a set of properties (gen)
  genProperties ::
    MonadGen m =>
    GenBase m ~ Identity =>
    IsProperty (Property model) =>
    model ->
    m (Set (Property model))
  genProperties _ = genGivenPropLogic logic

  -- Context Api (translate)
  -- defaults are provided to enable up front construction and testing of a model
  -- these can be overridden to translate a model to a Plutus Context

  script :: Model model -> Maybe Script
  script _ = Nothing

  modelRedeemer :: Model model -> Redeemer
  modelRedeemer _ = Redeemer $ toBuiltinData ()

  modelDatum :: Model model -> Datum
  modelDatum model =
    case headMay (modelInputData model) of
      Nothing -> Datum $ toBuiltinData ()
      Just (_, so) -> Datum so

  modelCtx :: Model model -> Context
  modelCtx model = Context . toBuiltinData $ context
    where
      context :: ScriptContext
      context =
        ScriptContext go (modelScriptPurpose model)
      go :: TxInfo
      go =
        let baseInfo = modelBaseTxInfo model
            inInfo = modelScriptTxInInfo model
            inData = datumWithHash . snd <$> modelInputData model
         in baseInfo
              { txInfoInputs = inInfo <> txInfoInputs baseInfo
              , txInfoData = inData <> txInfoData baseInfo
              }

  modelScriptPurpose :: Model model -> ScriptPurpose
  modelScriptPurpose model = Spending . TxOutRef (modelTxId model) $ 0

  modelBaseTxInfo :: Model model -> TxInfo
  modelBaseTxInfo model =
    TxInfo
      { txInfoInputs = modelTxInputs model
      , txInfoOutputs = modelTxOutputs model
      , txInfoFee = modelTxFee model
      , txInfoMint = modelTxMint model
      , txInfoDCert = modelTxDCert model
      , txInfoWdrl = modelTxWdrl model
      , txInfoValidRange = modelTxValidRange model
      , txInfoSignatories = modelTxSignatories model
      , txInfoData = modelTxData model
      , txInfoId = modelTxId model
      }

  modelTxInputs :: Model model -> [TxInInfo]
  modelTxInputs model = modelScriptTxInInfo model <> modelSpenderTxInInfo model

  modelTxOutputs :: Model model -> [TxOut]
  modelTxOutputs model =
    (\(v, d) -> TxOut (scriptHashAddress $ modelValidatorHash model) v (justDatumHash d)) <$> modelOutputData model

  modelTxFee :: Model model -> Value
  modelTxFee _ = mempty

  modelTxMint :: Model model -> Value
  modelTxMint _ = mempty

  modelTxDCert :: Model model -> [DCert]
  modelTxDCert _ = []

  modelTxWdrl :: Model model -> [(StakingCredential, Integer)]
  modelTxWdrl _ = []

  modelTxValidRange :: Model model -> POSIXTimeRange
  modelTxValidRange _ = always

  modelTxSignatories :: Model model -> [PubKeyHash]
  modelTxSignatories _ = []

  modelTxData :: Model model -> [(DatumHash, Datum)]
  modelTxData model = datumWithHash <$> (snd <$> modelInputData model) <> (snd <$> modelOutputData model)

  modelTxId :: Model model -> TxId
  modelTxId _ = TxId "testTx"

  modelValidatorHash :: Model model -> ValidatorHash
  modelValidatorHash _ = "90ab"

  modelInputData :: Model model -> [(Value, BuiltinData)]
  modelInputData _ = []

  modelOutputData :: Model model -> [(Value, BuiltinData)]
  modelOutputData _ = []

  modelSpenderTxInInfo :: Model model -> [TxInInfo]
  modelSpenderTxInInfo _ = []

  modelScriptTxInInfo :: Model model -> [TxInInfo]
  modelScriptTxInInfo model =
    let sha = scriptHashAddress $ modelValidatorHash model
     in ( \((v, d),i) ->
            TxInInfo (TxOutRef (modelTxId model) i) $
              TxOut sha v $ justDatumHash d
        )
          <$> zip (modelInputData model) [0..]

  modelCPUBudget :: Model model -> ExCPU
  modelCPUBudget _ = ExCPU maxBound

  modelMemoryBudget :: Model model -> ExMemory
  modelMemoryBudget _ = ExMemory maxBound

  -- Plutus compiled code test (eval)
  -----------------------------------

  runScriptTest ::
    Show (Model model) =>
    IsProperty (Property model) =>
    MonadTest t =>
    Model model ->
    t ()
  runScriptTest model =
    case script model of
      Nothing -> footnote "script not defined" >> failure
      Just s -> do
        case evaluateScript s of
          Left err -> footnoteShow err >> failure
          Right res -> deliverResult model ctx res
    where
      ctx = modelCtx model

  deliverResult ::
    Show (Model model) =>
    IsProperty (Property model) =>
    MonadTest m =>
    Model model ->
    Context ->
    (ExBudget, [Text]) ->
    m ()
  deliverResult model ctx (cost, logs) =
    case (shouldPass, lastMay logs >>= Text.stripPrefix "proper-plutus: ") of
      (_, Nothing) -> failWithFootnote noOutcome
      (False, Just "Fail") -> success
      (True, Just "Pass") -> successWithBudgetCheck cost
      (True, Just t) ->
        if t == "Fail"
          then failWithFootnote unexpectedFailure
          else case Text.stripPrefix "Parse failed: " t of
            Nothing -> failWithFootnote $ internalError t
            Just t' -> failWithFootnote $ noParse t'
      (False, Just t) ->
        if t == "Pass"
          then failWithFootnote unexpectedSuccess
          else case Text.stripPrefix "Parse failed: " t of
            Nothing -> failWithFootnote $ internalError t
            Just _ -> success
    where
      shouldPass :: Bool
      shouldPass = satisfiesPropLogic expect $ properties model
      successWithBudgetCheck :: MonadTest m => ExBudget -> m ()
      successWithBudgetCheck (ExBudget cpu mem) =
        if cpu <= modelCPUBudget model && mem <= modelMemoryBudget model
          then success
          else failWithFootnote budgetCheckFailure
      failWithFootnote :: MonadTest m => String -> m ()
      failWithFootnote s = footnote s >> failure
      budgetCheckFailure :: String
      budgetCheckFailure =
        renderStyle ourStyle $
          "Success! But at what cost?"
            $+$ hang "Budget" 4 (ppDoc (ExBudget (modelCPUBudget model) (modelMemoryBudget model)))
            $+$ hang "Cost" 4 (ppDoc cost)
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
          $+$ hang "Expected " 4 (if shouldPass then "Pass" else "Fail")
          $+$ hang "Properties " 4 (ppDoc $ properties model)
      dumpInputs :: Doc
      dumpInputs =
        "Parameters"
          $+$ ppDoc model
      dumpLogs :: Doc
      dumpLogs = vcat . fmap go . zip [1 ..] $ logs
      go :: (Int, Text) -> Doc
      go (ix, line) = (int ix <> colon) <+> (text . show $ line)
      ourStyle :: Style
      ourStyle = style {lineLength = 80}

  -- HedgeHog properties and property groups

  modelTestGivenProperties ::
    IsProperty (Property model) =>
    Show (Model model) =>
    Set (Property model) ->
    Hedgehog.Property
  modelTestGivenProperties properties' =
    property $ do
      model <- forAll $ genModel properties'
      properties model === properties'

  plutusTestGivenProperties ::
    IsProperty (Property model) =>
    Show (Model model) =>
    Set (Property model) ->
    Hedgehog.Property
  plutusTestGivenProperties properties' =
    property $ do
      model <- forAll $ genModel properties'
      runScriptTest model

  quickCheckModelTest ::
    IsProperty (Property model) =>
    Show (Model model) =>
    model ->
    Hedgehog.Property
  quickCheckModelTest m =
    property $ do
      properties' <- forAll $ genProperties m
      model <- forAll $ genModel properties'
      properties model === properties'

  quickCheckPlutusTest ::
    IsProperty (Property model) =>
    Show (Model model) =>
    model ->
    Hedgehog.Property
  quickCheckPlutusTest m =
    property $ do
      properties' <- forAll $ genProperties m
      model <- forAll $ genModel properties'
      runScriptTest model

  testEnumeratedScenarios ::
    IsProperty (Property model) =>
    Show (Model model) =>
    Show model =>
    model ->
    String ->
    (Set (Property model) -> Hedgehog.Property) ->
    PropLogic (Property model) ->
    Group
  testEnumeratedScenarios _ groupname test cond =
    let allProps = ([minBound .. maxBound] :: [Property model])
     in Group (fromString groupname) $
          [ (fromString $ show $ Set.toList p, test p)
          | p <- Set.fromList <$> combinationsUpToLength (length allProps) allProps
          , satisfiesPropLogic (logic /\ cond) p
          ]

-- helpers

-- we are using this to brute force solutions to our propositional logic which is inefficient.
-- we could use a solver to find solutions to the propositional logic if it becomes a problem.
combinationsUpToLength :: Int -> [a] -> [[a]]
combinationsUpToLength l li = filter ((<= l) . length) $ subsequences li

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

{-# INLINEABLE toTestMintingPolicy #-}
toTestMintingPolicy ::
  forall (redeemer :: Type).
  (FromData redeemer) =>
  (redeemer -> ScriptContext -> Bool) ->
  (BuiltinData -> BuiltinData -> ())
toTestMintingPolicy f r p = case fromBuiltinData r of
  Nothing -> reportParseFailed "Redeemer"
  Just r' -> case fromBuiltinData p of
    Nothing -> reportParseFailed "ScriptContext"
    Just p' ->
      if f r' p'
        then reportPass
        else reportFail

{-# INLINEABLE toTestStakeValidator #-}
toTestStakeValidator ::
  forall (redeemer :: Type).
  (FromData redeemer) =>
  (redeemer -> ScriptContext -> Bool) ->
  (BuiltinData -> BuiltinData -> ())
toTestStakeValidator = toTestMintingPolicy

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
