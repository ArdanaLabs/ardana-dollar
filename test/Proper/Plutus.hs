{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Proper.Plutus (
  Proper (..),
  IsProperty,
  toTestValidator,
  PropLogic (..),
  (\/),
  (/\),
  (-->),
  (<->),
) where

import Control.Monad.Reader (
  Reader,
  ask,
  runReader,
 )
import Data.Functor.Identity (Identity)
import Data.Kind (Type)
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
  elem,
  filter,
  fmap,
  mempty,
  not,
  or,
  pure,
  snd,
  zip,
  length,
  (<=),
  ($),
  (&&),
  (.),
  (<$>),
  (<*>),
  (<>),
  (==),
  (>>),
  (>>=),
  (||),
 )
import Data.List (subsequences)

data PropLogic a
  = Atom Bool
  | Prop a
  | Neg (PropLogic a)
  | Conjunction (PropLogic a) (PropLogic a)
  | Disjunction (PropLogic a) (PropLogic a)
  | Implication (PropLogic a) (PropLogic a)
  | IfAndOnlyIf (PropLogic a) (PropLogic a)

(/\) :: PropLogic a -> PropLogic a -> PropLogic a
(/\) = Conjunction

(\/) :: PropLogic a -> PropLogic a -> PropLogic a
(\/) = Disjunction

(-->) :: PropLogic a -> PropLogic a -> PropLogic a
(-->) = Implication

(<->) :: PropLogic a -> PropLogic a -> PropLogic a
(<->) = IfAndOnlyIf

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
  satisfiesProperty :: Model model -> Property model -> Bool

  properties ::
    IsProperty (Property model) =>
    Model model ->
    Set (Property model)
  properties x = Set.fromList $ filter (satisfiesProperty x) [minBound .. maxBound]

  -- properties may be in a positive or negative context
  -- by default all properties are negative and should cause a failure
  shouldCauseFailure :: Property model -> Bool
  shouldCauseFailure _ = True

  expect :: Set (Property model) -> Result
  expect p = if or (shouldCauseFailure <$> Set.toList p) then Fail else Pass

  -- generate a model that satisfies specified properties
  genModel :: MonadGen m => Set (Property model) -> m (Model model)

  genProperties ::
    MonadGen m =>
    GenBase m ~ Identity =>
    IsProperty (Property model) =>
    model ->
    m (Set (Property model))
  genProperties _ = genGivenPropLogic logic

  -- some properties imply others
  logic :: PropLogic (Property model)
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

  modelTxId :: Model model -> TxId
  modelTxId _ = "abcd"

  modelValidatorHash :: Model model -> ValidatorHash
  modelValidatorHash _ = "90ab"

  modelSpendingValue :: Model model -> Value
  modelSpendingValue _ = mempty

  modelTxInfoId :: Model model -> TxId
  modelTxInfoId _ = TxId "testTx"

  modelFee :: Model model -> Value
  modelFee _ = mempty

  modelInputData :: Model model -> [(Value, BuiltinData)]
  modelInputData _ = []

  modelOutputData :: Model model -> [(Value, BuiltinData)]
  modelOutputData _ = []

  modelTxSignatories :: Model model -> [PubKeyHash]
  modelTxSignatories _ = []

  modelTimeRange :: Model model -> POSIXTimeRange
  modelTimeRange _ = always

  modelBaseTxInfo :: Model model -> TxInfo
  modelBaseTxInfo model =
    TxInfo
      { txInfoInputs = modelScriptTxInInfo model <> modelSpenderTxInInfo model
      , txInfoOutputs = modelTxOuts model
      , txInfoFee = modelFee model
      , txInfoMint = mempty
      , txInfoDCert = []
      , txInfoWdrl = []
      , txInfoValidRange = modelTimeRange model
      , txInfoSignatories = modelTxSignatories model
      , txInfoData = datumWithHash <$> (snd <$> modelInputData model) <> (snd <$> modelOutputData model)
      , txInfoId = modelTxInfoId model
      }

  modelTxOuts :: Model model -> [TxOut]
  modelTxOuts model =
    (\(v, d) -> TxOut (scriptHashAddress $ modelValidatorHash model) v (justDatumHash d)) <$> modelOutputData model

  modelSpenderTxInInfo :: Model model -> [TxInInfo]
  modelSpenderTxInInfo _ = []

  modelScriptTxInInfo :: Model model -> [TxInInfo]
  modelScriptTxInInfo model =
    let sha = scriptHashAddress $ modelValidatorHash model
     in ( \(v, d) ->
            TxInInfo (TxOutRef (modelTxId model) 0) $
              TxOut sha v $ justDatumHash d
        )
          <$> modelInputData model

  modelCtx :: Model model -> Context
  modelCtx model = Context . toBuiltinData $ context
    where
      context :: ScriptContext
      context =
        ScriptContext go
          . Spending
          . TxOutRef (modelTxId model)
          $ 0
      go :: TxInfo
      go =
        let baseInfo = modelBaseTxInfo model
            inInfo = modelScriptTxInInfo model
            inData = datumWithHash . snd <$> modelInputData model
         in baseInfo
              { txInfoInputs = inInfo <> txInfoInputs baseInfo
              , txInfoData = inData <> txInfoData baseInfo
              }

  modelRedeemer :: Model model -> Redeemer
  modelRedeemer _ = Redeemer $ toBuiltinData ()

  modelDatum :: Model model -> Datum
  modelDatum model =
    case headMay (modelInputData model) of
      Nothing -> Datum $ toBuiltinData ()
      Just (_, so) -> Datum so

  reifyTest :: Show (Model model) => IsProperty (Property model) => MonadTest t => Model model -> t ()
  reifyTest model =
    case runScript ctx val dat red of
      Left err -> footnoteShow err >> failure
      Right (_, logs) -> deliverResult model ctx logs
    where
      val = validator model
      ctx = modelCtx model
      dat = modelDatum model
      red = modelRedeemer model

  deliverResult :: Show (Model model) => IsProperty (Property model) => MonadTest m => Model model -> Context -> [Text] -> m ()
  deliverResult p ctx logs =
    let shouldPass = expect $ properties p
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
          $+$ hang "Expected " 4 (ppDoc $ expect $ properties p)
          $+$ hang "Properties " 4 (ppDoc $ properties p)
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
      properties model === properties'

  selfTestGivenProperties ::
    IsProperty (Property model) =>
    Show (Model model) =>
    Set (Property model) ->
    Hedgehog.Property
  selfTestGivenProperties properties' =
    property $ do
      model <- forAll $ genModel properties'
      properties model === properties'

  selfTestGroup ::
    IsProperty (Property model) =>
    Show (Model model) =>
    Show model =>
    model ->
    Int ->
    Group
  selfTestGroup model l =
    Group (fromString $ show model) $
        (fromString "selfTestRandomProperties", selfTestAll model)
        :
        [ (fromString $ "selfTestProperties " <> show p, selfTestGivenProperties p)
        | p <- Set.fromList <$> combinationsUpToLength l ([minBound .. maxBound] :: [Property model])
        , satisfiesPropLogic logic p
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
    Int ->
    Group
  validatorTestGroup model l =
    Group (fromString $ show model) $
        (fromString "validatorTestRandomProperties", validatorTestAll model)
        :
        [ (fromString $ "validatorTestProperties " <> show p, validatorTestGivenProperties p)
        | p <- Set.fromList <$> combinationsUpToLength l ([minBound .. maxBound] :: [Property model])
        , satisfiesPropLogic logic p
        ]


-- helpers

combinationsUpToLength :: Int -> [a] -> [[a]]
combinationsUpToLength l li = filter ((<=l) . length) $ subsequences li

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
