{-# LANGUAGE TypeFamilies #-}

--------------------------------------------------------------------------------
-- Proper.Plutus encourages you to define a model before writing any Plutus
--------------------------------------------------------------------------------
module Proper.Plutus (
  Proper (..),
  IsProperty,
  justDatumHash,
  PropLogic (..),
  CompiledObject (..),
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
  Validator,
  MintingPolicy,
  StakeValidator,
  Datum (..),
  DatumHash,
  PubKeyHash,
  Redeemer (..),
  TxId (..),
  TxInInfo (..),
  TxOut (..),
  TxOutRef (..),
  Value,
  ScriptError (..),
  always,
  datumHash,
  evaluateScript,
  applyValidator,
  applyMintingPolicyScript,
  applyStakeValidatorScript,
 )
import Ledger.Address (scriptHashAddress)
import Plutus.V1.Ledger.Api (
  DCert,
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
 )
import Safe (
  headMay,
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
  (>>),
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

data CompiledObject = CompiledValidator Validator
                    | CompiledMintingPolicy MintingPolicy
                    | CompiledStakeValidator StakeValidator

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


  script :: Model model -> Maybe CompiledObject
  script _ = Nothing

  modelRedeemer :: Model model -> Redeemer
  modelRedeemer _ = Redeemer $ toBuiltinData ()

  modelDatum :: Model model -> Maybe Datum
  modelDatum model = Datum . snd <$> headMay (modelInputData model) 

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
     in ( \((v, d), i) ->
            TxInInfo (TxOutRef (modelTxId model) i) $
              TxOut sha v $ justDatumHash d
        )
          <$> zip (modelInputData model) [0 ..]

  modelCPUBudget :: Model model -> ExCPU
  modelCPUBudget _ = ExCPU maxBound

  modelMemoryBudget :: Model model -> ExMemory
  modelMemoryBudget _ = ExMemory maxBound


  -- Plutus compiled code test (eval)
  -----------------------------------
    --

  wrapObjectAsScript ::
    Show (Model model) =>
    IsProperty (Property model) =>
    MonadTest t =>
    Model model ->
    t Script
  wrapObjectAsScript model = do
    let ctx = modelCtx model
        dat = modelDatum model  --TODO this should be Maybe Datum since Minting and Staking don't require a datum
        red = modelRedeemer model
    case script model of
      Just (CompiledValidator v) ->
        case dat of
          Nothing -> footnote "modelDatum not defined for CompiledValidator" >> failure
          Just dat' -> pure $ applyValidator ctx v dat' red
      Just (CompiledMintingPolicy m) ->
        pure $ applyMintingPolicyScript ctx m red
      Just (CompiledStakeValidator s) ->
        pure $ applyStakeValidatorScript ctx s red
      _ -> footnote "script not defined" >> failure

  runScriptTest ::
    Show (Model model) =>
    IsProperty (Property model) =>
    MonadTest t =>
    Model model ->
    t ()
  runScriptTest model = do
    s <- wrapObjectAsScript model
    case evaluateScript s of
      Left (EvaluationError logs err) -> deliverResult model (Left (logs,err))
      Right res -> deliverResult model (Right res)
      Left err -> footnoteShow err >> failure

  deliverResult ::
    Show (Model model) =>
    IsProperty (Property model) =>
    MonadTest m =>
    Model model ->
    Either ([Text],String) (ExBudget, [Text]) ->
    m ()
  deliverResult model res =
    case (shouldPass, res) of
      (False, Left _) -> success
      (True, Right (cost,_)) -> successWithBudgetCheck cost
      (True, Left err) -> failWithFootnote $ unexpectedFailure err
      (False, Right (_,logs)) -> failWithFootnote $ unexpectedSuccess logs
    where
      ctx :: Context
      ctx = modelCtx model
      shouldPass :: Bool
      shouldPass = satisfiesPropLogic expect $ properties model
      successWithBudgetCheck :: MonadTest m => ExBudget -> m ()
      successWithBudgetCheck cost@(ExBudget cpu mem) =
        if cpu <= modelCPUBudget model && mem <= modelMemoryBudget model
          then success
          else failWithFootnote $ budgetCheckFailure cost
      failWithFootnote :: MonadTest m => String -> m ()
      failWithFootnote s = footnote s >> failure
      budgetCheckFailure :: ExBudget -> String
      budgetCheckFailure cost =
        renderStyle ourStyle $
          "Success! But at what cost?"
            $+$ hang "Budget" 4 (ppDoc (ExBudget (modelCPUBudget model) (modelMemoryBudget model)))
            $+$ hang "Cost" 4 (ppDoc cost)
      unexpectedSuccess :: [Text] -> String
      unexpectedSuccess logs =
        renderStyle ourStyle $
          "Unexpected success" $+$ dumpState logs
      unexpectedFailure :: ([Text],String) -> String
      unexpectedFailure (logs,reason) =
        renderStyle ourStyle $
          text ("Unexpected failure(" <> reason <> ")") $+$ dumpState logs
      dumpState :: [Text] -> Doc
      dumpState logs =
        ""
          $+$ hang "Context" 4 (ppDoc ctx)
          $+$ hang "Inputs" 4 dumpInputs
          $+$ hang "Logs" 4 (dumpLogs logs)
          $+$ hang "Expected " 4 (if shouldPass then "Pass" else "Fail")
          $+$ hang "Properties " 4 (ppDoc $ properties model)
      dumpInputs :: Doc
      dumpInputs =
        "Parameters"
          $+$ ppDoc model
      dumpLogs :: [Text] -> Doc
      dumpLogs logs = vcat . fmap go . zip [1 ..] $ logs
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

  combinedTestGivenProperties ::
    IsProperty (Property model) =>
    Show (Model model) =>
    Set (Property model) ->
    Hedgehog.Property
  combinedTestGivenProperties properties' =
    property $ do
      model <- forAll $ genModel properties'
      properties model === properties'
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

