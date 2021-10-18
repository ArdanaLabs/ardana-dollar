{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Test.ArdanaDollar.PriceOracle.OnChain.Model.Proper (
  priceOracleTest,
) where

import ArdanaDollar.PriceOracle.OnChain
import Control.Monad (void)
import Control.Monad.Trans.Reader (
  ReaderT (runReaderT),
  ask,
 )
import Data.Kind (Type)
import Data.Set qualified as Set
import Hedgehog (
  MonadGen,
  checkParallel,
 )
import Hedgehog.Gen qualified as Gen
import Hedgehog.Gen.Plutus qualified as HP
import Hedgehog.Range qualified as Range
import Ledger (
  CurrencySymbol,
  Extended (Finite),
  Interval (..),
  LowerBound (..),
  POSIXTime (..),
  PrivateKey,
  PubKey,
  PubKeyHash,
  TokenName,
  UpperBound (..),
  Value,
  always,
  knownPrivateKeys,
  pubKeyHash,
 )
import Ledger.Oracle (
  SignedMessage,
  signMessage,
 )
import Plutus.V1.Ledger.Api (getValue)
import Plutus.V1.Ledger.Contexts (ScriptContext (..))
import Plutus.V1.Ledger.Scripts (
  Context,
  Datum,
  MintingPolicy,
  Redeemer,
  Script,
  Validator,
  ValidatorHash,
  applyMintingPolicyScript,
  applyValidator,
  mkMintingPolicyScript,
  mkValidatorScript,
 )
import Plutus.V1.Ledger.Value (singleton)
import PlutusCore.Evaluation.Machine.ExMemory (ExCPU (..), ExMemory (..))
import PlutusTx (
  applyCode,
  compile,
  toBuiltinData,
 )
import PlutusTx.AssocMap qualified as AssocMap
import PlutusTx.Builtins (BuiltinData)
import PlutusTx.Prelude (
  BuiltinByteString,
  isJust,
 )
import PlutusTx.UniqueMap qualified as UniqueMap
import Proper.Plutus
import Wallet.Emulator.Wallet (
  knownWallet,
  walletPubKey,
 )
import Prelude (
  Bool (..),
  Bounded (..),
  Enum,
  Eq,
  IO,
  Integer,
  Maybe (..),
  Ord,
  Show,
  elem,
  filter,
  flip,
  fromInteger,
  fst,
  pure,
  snd,
  uncurry,
  (!!),
  ($),
  (&&),
  (+),
  (-),
  (.),
  (/=),
  (<),
  (<$>),
  (<=),
  (==),
 )

import System.Exit (exitSuccess)

mkTestValidator :: OracleValidatorParams -> Validator
mkTestValidator params =
  mkValidatorScript $
    $$(compile [||go||])
      `applyCode` oracleCompiledTypedValidator params
  where
    {-# INLINEABLE go #-}
    go ::
      (SignedMessage PriceTracking -> () -> ScriptContext -> Bool) ->
      (BuiltinData -> BuiltinData -> BuiltinData -> ())
    go = toTestValidator

mkTestValidatorScript :: OracleValidatorParams -> Datum -> Redeemer -> Context -> Script
mkTestValidatorScript params d r c = applyValidator c (mkTestValidator params) d r

mkTestMintingPolicy :: ValidatorHash -> OracleMintingParams -> MintingPolicy
mkTestMintingPolicy oracle params =
  mkMintingPolicyScript $
    $$(compile [||go||])
      `applyCode` oracleCompiledTypedMintingPolicy oracle params
  where
    {-# INLINEABLE go #-}
    go ::
      (() -> ScriptContext -> Bool) ->
      (BuiltinData -> BuiltinData -> ())
    go = toTestMintingPolicy

mkTestMintingPolicyScript :: ValidatorHash -> OracleMintingParams -> Redeemer -> Context -> Script
mkTestMintingPolicyScript oracle params r c = applyMintingPolicyScript c (mkTestMintingPolicy oracle params) r

priceOracleTest :: IO ()
priceOracleTest = do
  void $ checkParallel $ selfTestGroup Model 2
  void $ checkParallel $ scriptTestGroup Model 2
  void exitSuccess

data PriceOracleModel = Model deriving (Show)

data TransactorParams = NoSigner | JustSignedBy Integer | SignedByWithValue Integer Value
  deriving (Show)

data StateUTXOParams = StateUTXOParams
  { stateTokenValue :: Value
  , stateDatumValue :: Maybe TestDatumParameters
  }
  deriving (Show)

data TestDatumParameters = TestDatumParameters
  { signedByWallet :: Integer
  , timeStamp :: Integer
  }
  deriving (Show)

instance IsProperty (Property PriceOracleModel)

instance Proper PriceOracleModel where
  data Model PriceOracleModel
    = PriceOracleValidatorModel
        { stateNFTCurrency :: (CurrencySymbol, TokenName)
        , timeRangeLowerBound :: Integer
        , timeRangeUpperBound :: Integer
        , ownerWallet :: Integer
        , transactorParams :: TransactorParams
        , inputParams :: StateUTXOParams
        , outputParams :: StateUTXOParams
        , peggedCurrency :: BuiltinByteString
        }
    | PriceOracleMinterModel
        { stateNFTCurrency :: (CurrencySymbol, TokenName)
        , ownerWallet :: Integer
        , transactorParams :: TransactorParams
        , outputParams :: StateUTXOParams
        }
    deriving (Show)

  -- TODO this would be nicer if the properties were in a positive context
  data Property PriceOracleModel
    = PriceOracleMintingPolicyContext
    | PriceOracleValidatorContext
    | OutputDatumTimestampIsInRange
    | RangeWithinSizeLimit
    | OutputDatumSignedByOwner
    | TransactionSignedByOwner
    | StateTokenReturned
    | InputDatumIsCorrectType
    | OutputDatumIsCorrectType
    deriving stock (Enum, Eq, Ord, Bounded, Show)

  satisfiesProperty = flip satisfiesProperty'

  logic =
    allOf
      [ oneOf [PriceOracleMintingPolicyContext, PriceOracleValidatorContext]
      , anyOf
          ( Prop
              <$> [ OutputDatumTimestampIsInRange
                  , RangeWithinSizeLimit
                  , OutputDatumSignedByOwner
                  , InputDatumIsCorrectType
                  , OutputDatumIsCorrectType
                  ]
          )
          --> Prop PriceOracleValidatorContext
      , -- parsing will fail before we can check the signature hence these implications
        Prop OutputDatumSignedByOwner --> Prop OutputDatumIsCorrectType
      , Prop OutputDatumTimestampIsInRange --> Prop OutputDatumIsCorrectType
      ]

  expect =
    allOf
      [ Prop PriceOracleValidatorContext
          --> allOf
            ( Prop
                <$> [ OutputDatumTimestampIsInRange
                    , RangeWithinSizeLimit
                    , OutputDatumSignedByOwner
                    , TransactionSignedByOwner
                    , StateTokenReturned
                    , InputDatumIsCorrectType
                    , OutputDatumIsCorrectType
                    ]
            )
      , Prop PriceOracleMintingPolicyContext
          --> allOf
            ( Prop
                <$> [ TransactionSignedByOwner
                    , StateTokenReturned
                    ]
            )
      ]

  genModel = genModel' . Set.toList

  script m@PriceOracleValidatorModel {..} = Just $ mkTestValidatorScript params (modelDatum m) (modelRedeemer m) (modelCtx m)
    where
      ownerPubKey :: PubKey
      ownerPubKey = walletPubKey (knownWallet ownerWallet)
      ownerPubKeyHash :: PubKeyHash
      ownerPubKeyHash = pubKeyHash ownerPubKey
      params :: OracleValidatorParams
      params = OracleValidatorParams (fst stateNFTCurrency) ownerPubKey ownerPubKeyHash peggedCurrency
  script m@PriceOracleMinterModel {..} = Just $ mkTestMintingPolicyScript validatorHash' params (modelRedeemer m) (modelCtx m)
    where
      validatorHash' :: ValidatorHash
      validatorHash' = "09ab"
      ownerPubKey :: PubKey
      ownerPubKey = walletPubKey (knownWallet ownerWallet)
      ownerPubKeyHash :: PubKeyHash
      ownerPubKeyHash = pubKeyHash ownerPubKey
      params :: OracleMintingParams
      params = OracleMintingParams ownerPubKey ownerPubKeyHash


  modelCPUBudget _ = ExCPU 1_000_000_000
  modelMemoryBudget _ = ExMemory 1_000_000_000

  modelTxValidRange PriceOracleValidatorModel {..} =
    Interval
      (LowerBound (Finite (POSIXTime timeRangeLowerBound)) True)
      (UpperBound (Finite (POSIXTime timeRangeUpperBound)) True)
  modelTxValidRange PriceOracleMinterModel {} = always

  modelTxSignatories model =
    case transactorParams model of
      NoSigner -> []
      JustSignedBy signer -> [go signer]
      SignedByWithValue signer _ -> [go signer]
    where
      go = pubKeyHash . walletPubKey . knownWallet

  modelInputData PriceOracleValidatorModel {..} =
    [
      ( stateTokenValue inputParams
      , modelDatum' $ stateDatumValue inputParams
      )
    ]
  modelInputData PriceOracleMinterModel {} = []

  modelOutputData model =
    [
      ( stateTokenValue (outputParams model)
      , modelDatum' $ stateDatumValue $ outputParams model
      )
    ]

modelDatum' :: Maybe TestDatumParameters -> BuiltinData
modelDatum' Nothing = toBuiltinData ()
modelDatum' (Just TestDatumParameters {..}) =
  toBuiltinData $ signMessage (PriceTracking UniqueMap.empty UniqueMap.empty (POSIXTime timeStamp)) signedByPrivK
  where
    signedByPrivK :: PrivateKey
    signedByPrivK = lookupPrivateKey signedByWallet
    lookupPrivateKey :: Integer -> PrivateKey
    lookupPrivateKey i = knownPrivateKeys !! fromInteger (i - 1)

---------------------------------------------------------------------------------
-- TODO use the real currency symbol
mockCurrencySymbol :: CurrencySymbol
mockCurrencySymbol = "123456789012345678901234567890ef"

correctNFTCurrency :: (CurrencySymbol, TokenName)
correctNFTCurrency = (mockCurrencySymbol, "PriceTracking")

--correctStateTokenValue :: Value
--correctStateTokenValue = uncurry singleton correctNFTCurrency 1
---------------------------------------------------------------------------------

-- Property
---------------------------------------------------------------------------------

satisfiesProperty' :: Property PriceOracleModel -> Model PriceOracleModel -> Bool
satisfiesProperty' OutputDatumTimestampIsInRange = outputDatumTimestampIsInRange
satisfiesProperty' RangeWithinSizeLimit = rangeWithinSizeLimit
satisfiesProperty' OutputDatumSignedByOwner = outputDatumSignedByOwner
satisfiesProperty' TransactionSignedByOwner = transactionSignedByOwner
satisfiesProperty' StateTokenReturned = stateTokenReturned
satisfiesProperty' InputDatumIsCorrectType = hasIncorrectInputDatum
satisfiesProperty' OutputDatumIsCorrectType = hasIncorrectOutputDatum
satisfiesProperty' PriceOracleMintingPolicyContext = isMinterModel
satisfiesProperty' PriceOracleValidatorContext = isScriptModel

type ModelProperty = Model PriceOracleModel -> Bool

isMinterModel :: ModelProperty
isMinterModel PriceOracleMinterModel {} = True
isMinterModel _ = False

isScriptModel :: ModelProperty
isScriptModel PriceOracleValidatorModel {} = True
isScriptModel _ = False

outputDatumTimestampIsInRange :: ModelProperty
outputDatumTimestampIsInRange PriceOracleValidatorModel {..} =
  case stateDatumValue outputParams of
    Nothing -> False
    Just so -> timeRangeLowerBound <= timeStamp so && timeStamp so <= timeRangeUpperBound
outputDatumTimestampIsInRange _ = False

rangeWithinSizeLimit :: ModelProperty
rangeWithinSizeLimit PriceOracleValidatorModel {..} =
  let rangeLen = timeRangeUpperBound - timeRangeLowerBound
   in 0 < rangeLen && rangeLen <= 10000
rangeWithinSizeLimit _ = False

outputDatumSignedByOwner :: ModelProperty
outputDatumSignedByOwner PriceOracleValidatorModel {..} =
  case stateDatumValue outputParams of
    Nothing -> False
    Just so -> signedByWallet so == ownerWallet
outputDatumSignedByOwner _ = False

transactionSignedByOwner :: ModelProperty
transactionSignedByOwner model =
  case transactorParams model of
    NoSigner -> False
    JustSignedBy signer -> signer == ownerWallet model
    SignedByWithValue signer _ -> signer == ownerWallet model

stateTokenReturned :: ModelProperty
stateTokenReturned model =
  case AssocMap.lookup mockCurrencySymbol $ getValue $ stateTokenValue $ outputParams model of
    Nothing -> False
    Just so -> case AssocMap.lookup (snd $ stateNFTCurrency model) so of
      Just 1 -> True
      _ -> False

hasIncorrectInputDatum :: ModelProperty
hasIncorrectInputDatum PriceOracleValidatorModel {..} = isJust $ stateDatumValue inputParams
hasIncorrectInputDatum _ = False

hasIncorrectOutputDatum :: ModelProperty
hasIncorrectOutputDatum PriceOracleValidatorModel {..} = isJust $ stateDatumValue outputParams
hasIncorrectOutputDatum _ = False

-- generators
---------------------------------------------------------------------------------

genModel' :: MonadGen m => [Property PriceOracleModel] -> m (Model PriceOracleModel)
genModel' props =
  if PriceOracleValidatorContext `elem` props
    then runReaderT genPriceOracleValidatorModel props
    else runReaderT genPriceOracleMinterModel props

genPriceOracleMinterModel ::
  forall (m :: Type -> Type).
  MonadGen m =>
  ReaderT [Property PriceOracleModel] m (Model PriceOracleModel)
genPriceOracleMinterModel = do
  (tlb, tub) <- genTimeRange
  w <- genKnownWalletIdx
  sp <- genTransactorParams w
  op <- genOutputUTXOParams w (tlb, tub)
  pure $
    PriceOracleMinterModel
      { stateNFTCurrency = correctNFTCurrency --TODO compute properly
      , ownerWallet = w
      , transactorParams = sp
      , outputParams = op
      }

genPriceOracleValidatorModel ::
  forall (m :: Type -> Type).
  MonadGen m =>
  ReaderT [Property PriceOracleModel] m (Model PriceOracleModel)
genPriceOracleValidatorModel = do
  (tlb, tub) <- genTimeRange
  w <- genKnownWalletIdx
  sp <- genTransactorParams w
  ip <- genInputUTXOParams
  op <- genOutputUTXOParams w (tlb, tub)
  pc <- HP.builtinByteString (Range.linear 0 6)
  pure $
    PriceOracleValidatorModel
      { stateNFTCurrency = correctNFTCurrency --TODO compute properly
      , timeRangeLowerBound = tlb
      , timeRangeUpperBound = tub
      , ownerWallet = w
      , transactorParams = sp
      , inputParams = ip
      , outputParams = op
      , peggedCurrency = pc
      }

-- TODO read some Hedgehog for a nicer generator ideas
-- these nested ifs are not great but they do the job
genTransactorParams ::
  forall (m :: Type -> Type).
  MonadGen m =>
  Integer ->
  ReaderT [Property PriceOracleModel] m TransactorParams
genTransactorParams w = do
  properties' <- ask
  if TransactionSignedByOwner `elem` properties'
    then do
      b <- Gen.bool
      if b
        then pure $ JustSignedBy w
        else do
          v <- HP.value
          pure $ SignedByWithValue w v
    else do
      b <- Gen.bool
      if b
        then pure NoSigner
        else do
          b' <- Gen.bool
          if b'
            then do
              w' <- genWalletIdxOtherThan w
              pure $ JustSignedBy w'
            else do
              w' <- genWalletIdxOtherThan w
              v <- HP.value
              pure $ SignedByWithValue w' v

genInputUTXOParams ::
  forall (m :: Type -> Type).
  MonadGen m =>
  ReaderT [Property PriceOracleModel] m StateUTXOParams
genInputUTXOParams = do
  stok <- genStateToken
  d <- genInputDatumParameters
  pure $ StateUTXOParams stok d

genOutputUTXOParams ::
  forall (m :: Type -> Type).
  MonadGen m =>
  Integer ->
  (Integer, Integer) ->
  ReaderT [Property PriceOracleModel] m StateUTXOParams
genOutputUTXOParams w ts = do
  stok <- genStateToken
  d <- genOutputDatumParameters w ts
  pure $ StateUTXOParams stok d

genKnownWalletIdx ::
  forall (m :: Type -> Type).
  MonadGen m =>
  m Integer
genKnownWalletIdx = Gen.integral (Range.linear 1 9)

genWalletIdxOtherThan ::
  forall (m :: Type -> Type).
  MonadGen m =>
  Integer ->
  m Integer
genWalletIdxOtherThan w = Gen.element $ filter (/= w) [1 .. 9]

genOutputTimeStamp ::
  forall (m :: Type -> Type).
  MonadGen m =>
  (Integer, Integer) ->
  ReaderT [Property PriceOracleModel] m Integer
genOutputTimeStamp ts = do
  properties' <- ask
  if OutputDatumTimestampIsInRange `elem` properties'
    then do
      Gen.integral (uncurry Range.linear ts)
    else do
      b <- Gen.bool
      if b
        then Gen.integral (Range.linear 0 (fst ts - 1))
        else Gen.integral (Range.linear (snd ts + 1) 300_000)

genInputTimeStamp ::
  forall (m :: Type -> Type).
  MonadGen m =>
  ReaderT [Property PriceOracleModel] m Integer
genInputTimeStamp = Gen.integral (Range.linear 0 100_000)

genTimeRange ::
  forall (m :: Type -> Type).
  MonadGen m =>
  ReaderT [Property PriceOracleModel] m (Integer, Integer)
genTimeRange = do
  properties' <- ask
  if RangeWithinSizeLimit `elem` properties'
    then do
      t1 <- Gen.integral (Range.linear 1 100_000)
      t2 <- Gen.integral (Range.linear (t1 + 1) (t1 + 10_000))
      pure (t1, t2)
    else do
      t1 <- Gen.integral (Range.linear 1 100_000)
      t2 <- Gen.integral (Range.linear (t1 + 10_1000) (t1 + 200_000))
      pure (t1, t2)

genOutputDatumParameters ::
  forall (m :: Type -> Type).
  MonadGen m =>
  Integer ->
  (Integer, Integer) ->
  ReaderT [Property PriceOracleModel] m (Maybe TestDatumParameters)
genOutputDatumParameters w ts = do
  properties' <- ask
  if OutputDatumIsCorrectType `elem` properties'
    then do
      walletIdx <-
        if OutputDatumSignedByOwner `elem` properties'
          then pure w
          else genWalletIdxOtherThan w
      t <- genOutputTimeStamp ts
      pure $ Just $ TestDatumParameters walletIdx t
    else pure Nothing

genInputDatumParameters ::
  forall (m :: Type -> Type).
  MonadGen m =>
  ReaderT [Property PriceOracleModel] m (Maybe TestDatumParameters)
genInputDatumParameters = do
  properties' <- ask
  if InputDatumIsCorrectType `elem` properties'
    then do
      walletIdx <- genKnownWalletIdx
      t <- genInputTimeStamp
      pure $ Just $ TestDatumParameters walletIdx t
    else pure Nothing

genStateTokenCurrencySymbol ::
  forall (m :: Type -> Type).
  MonadGen m =>
  ReaderT [Property PriceOracleModel] m CurrencySymbol
genStateTokenCurrencySymbol = do
  properties' <- ask
  if StateTokenReturned `elem` properties'
    then pure $ fst correctNFTCurrency
    else HP.currencySymbol

genStateTokenTokenName ::
  forall (m :: Type -> Type).
  MonadGen m =>
  ReaderT [Property PriceOracleModel] m TokenName
genStateTokenTokenName = do
  properties' <- ask
  --TODO disambiguate. StateTokenReturned should be distinct from StateTokenCurrencyCorrect
  if StateTokenReturned `elem` properties'
    then pure $ snd correctNFTCurrency
    else HP.tokenName

genStateTokenAmount ::
  forall (m :: Type -> Type).
  MonadGen m =>
  ReaderT [Property PriceOracleModel] m Integer
genStateTokenAmount = do
  properties' <- ask
  if StateTokenReturned `elem` properties'
    then pure 1
    else do
      b <- Gen.bool
      if b
        then pure 0
        else Gen.integral (Range.linear 2 5)

genStateToken ::
  forall (m :: Type -> Type).
  MonadGen m =>
  ReaderT [Property PriceOracleModel] m Value
genStateToken = do
  s <- genStateTokenCurrencySymbol
  n <- genStateTokenTokenName
  v <- genStateTokenAmount
  pure $ singleton s n v
