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
  Validator,
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
import PlutusTx.Prelude (BuiltinByteString)
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
  (+),
  (-),
  (.),
  (/=),
  (<),
  (||),
 )

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

priceOracleTest :: IO ()
priceOracleTest = do
  void $ checkParallel $ selfTestGroup Model 1
  void $ checkParallel $ validatorTestGroup Model 1

data PriceOracleModel = Model deriving (Show)

data SpenderParams = NoSigner | JustSignedBy Integer | SignedByWithValue Integer Value
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
  data Model PriceOracleModel = PriceOracleModel
    { stateNFTCurrency :: (CurrencySymbol, TokenName)
    , timeRangeLowerBound :: Integer
    , timeRangeUpperBound :: Integer
    , ownerWallet :: Integer
    , transactorParams :: SpenderParams
    , inputParams :: StateUTXOParams
    , outputParams :: StateUTXOParams
    , peggedCurrency :: BuiltinByteString
    }
    deriving (Show)

  data Property PriceOracleModel
    = OutputDatumTimestampNotInRange
    | RangeNotWithinSizeLimit
    | OutputDatumNotSignedByOwner
    | TransactionNotSignedByOwner
    | StateTokenNotReturned
    | HasIncorrectInputDatum
    | HasIncorrectOutputDatum
    deriving stock (Enum, Eq, Ord, Bounded, Show)

  satisfiesProperty = flip satisfiesProperty'

  logic =
    Neg (Prop HasIncorrectOutputDatum)
      \/ ( Prop HasIncorrectOutputDatum
            --> ( Prop OutputDatumTimestampNotInRange
                    /\ Prop OutputDatumNotSignedByOwner
                )
         )

  genModel = genModel' . Set.toList

  validator PriceOracleModel {..} = mkTestValidator params
    where
      ownerPubKey :: PubKey
      ownerPubKey = walletPubKey (knownWallet ownerWallet)

      ownerPubKeyHash :: PubKeyHash
      ownerPubKeyHash = pubKeyHash ownerPubKey

      params :: OracleValidatorParams
      params = OracleValidatorParams (fst stateNFTCurrency) ownerPubKey ownerPubKeyHash peggedCurrency

  modelCPUBudget _ = ExCPU 1_000_000_000
  modelMemoryBudget _ = ExMemory 1_000_000_000

  modelTxValidRange PriceOracleModel {..} =
    Interval
      (LowerBound (Finite (POSIXTime timeRangeLowerBound)) True)
      (UpperBound (Finite (POSIXTime timeRangeUpperBound)) True)

  modelTxSignatories PriceOracleModel {..} =
    case transactorParams of
      NoSigner -> []
      JustSignedBy signer -> [go signer]
      SignedByWithValue signer _ -> [go signer]
    where
      go = pubKeyHash . walletPubKey . knownWallet

  modelInputData PriceOracleModel {..} =
    [
      ( stateTokenValue inputParams
      , modelDatum' $ stateDatumValue inputParams
      )
    ]

  modelOutputData PriceOracleModel {..} =
    [
      ( stateTokenValue inputParams
      , modelDatum' $ stateDatumValue outputParams
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
satisfiesProperty' OutputDatumTimestampNotInRange = outputDatumTimestampNotInRange
satisfiesProperty' RangeNotWithinSizeLimit = rangeNotWithinSizeLimit
satisfiesProperty' OutputDatumNotSignedByOwner = outputDatumNotSignedByOwner
satisfiesProperty' TransactionNotSignedByOwner = txNotSignedByOwner
satisfiesProperty' StateTokenNotReturned = stateTokenNotReturned
satisfiesProperty' HasIncorrectInputDatum = hasIncorrectInputDatum
satisfiesProperty' HasIncorrectOutputDatum = hasIncorrectOutputDatum

type ModelProperty = Model PriceOracleModel -> Bool

outputDatumTimestampNotInRange :: ModelProperty
outputDatumTimestampNotInRange PriceOracleModel {..} =
  case stateDatumValue outputParams of
    Nothing -> True
    Just so -> timeStamp so < timeRangeLowerBound || timeRangeUpperBound < timeStamp so

rangeNotWithinSizeLimit :: ModelProperty
rangeNotWithinSizeLimit PriceOracleModel {..} =
  let rangeLen = timeRangeUpperBound - timeRangeLowerBound
   in rangeLen < 0 || 10000 < rangeLen

outputDatumNotSignedByOwner :: ModelProperty
outputDatumNotSignedByOwner PriceOracleModel {..} =
  case stateDatumValue outputParams of
    Nothing -> True
    Just so -> signedByWallet so /= ownerWallet

txNotSignedByOwner :: ModelProperty
txNotSignedByOwner PriceOracleModel {..} = case transactorParams of
  NoSigner -> True
  JustSignedBy signer -> signer /= ownerWallet
  SignedByWithValue signer _ -> signer /= ownerWallet

stateTokenNotReturned :: ModelProperty
stateTokenNotReturned PriceOracleModel {..} =
  case AssocMap.lookup mockCurrencySymbol $ getValue $ stateTokenValue outputParams of
    Nothing -> True
    Just so -> case AssocMap.lookup (snd stateNFTCurrency) so of
      Just 1 -> False
      _ -> True

hasIncorrectInputDatum :: ModelProperty
hasIncorrectInputDatum PriceOracleModel {..} =
  case stateDatumValue inputParams of
    Nothing -> True
    _ -> False

hasIncorrectOutputDatum :: ModelProperty
hasIncorrectOutputDatum PriceOracleModel {..} =
  case stateDatumValue outputParams of
    Nothing -> True
    _ -> False

-- Gen TODO move anything that is not model specific to a common lib
---------------------------------------------------------------------------------

genModel' :: MonadGen m => [Property PriceOracleModel] -> m (Model PriceOracleModel)
genModel' = runReaderT genPriceOracleModel

genPriceOracleModel ::
  forall (m :: Type -> Type).
  MonadGen m =>
  ReaderT [Property PriceOracleModel] m (Model PriceOracleModel)
genPriceOracleModel = do
  (tlb, tub) <- genTimeRange
  w <- genKnownWalletIdx
  sp <- genSpenderParams w
  ip <- genInputUTXOParams
  op <- genOutputUTXOParams w (tlb, tub)
  pc <- HP.builtinByteString (Range.linear 0 6)
  pure $
    PriceOracleModel
      { stateNFTCurrency = correctNFTCurrency --TODO include in model
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
genSpenderParams ::
  forall (m :: Type -> Type).
  MonadGen m =>
  Integer ->
  ReaderT [Property PriceOracleModel] m SpenderParams
genSpenderParams w = do
  properties' <- ask
  if TransactionNotSignedByOwner `elem` properties'
    then do
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
    else do
      b <- Gen.bool
      if b
        then pure $ JustSignedBy w
        else do
          v <- HP.value
          pure $ SignedByWithValue w v

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
  if OutputDatumTimestampNotInRange `elem` properties'
    then do
      b <- Gen.bool
      if b
        then Gen.integral (Range.linear 0 (fst ts - 1))
        else Gen.integral (Range.linear (snd ts + 1) 300_000)
    else do
      Gen.integral (uncurry Range.linear ts)

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
  if RangeNotWithinSizeLimit `elem` properties'
    then do
      t1 <- Gen.integral (Range.linear 1 100_000)
      t2 <- Gen.integral (Range.linear (t1 + 10_1000) (t1 + 200_000))
      pure (t1, t2)
    else do
      t1 <- Gen.integral (Range.linear 1 100_000)
      t2 <- Gen.integral (Range.linear (t1 + 1) (t1 + 10_000))
      pure (t1, t2)

genOutputDatumParameters ::
  forall (m :: Type -> Type).
  MonadGen m =>
  Integer ->
  (Integer, Integer) ->
  ReaderT [Property PriceOracleModel] m (Maybe TestDatumParameters)
genOutputDatumParameters w ts = do
  properties' <- ask
  if HasIncorrectOutputDatum `elem` properties'
    then pure Nothing
    else do
      walletIdx <-
        if OutputDatumNotSignedByOwner `elem` properties'
          then genWalletIdxOtherThan w
          else pure w
      t <- genOutputTimeStamp ts
      pure $ Just $ TestDatumParameters walletIdx t

genInputDatumParameters ::
  forall (m :: Type -> Type).
  MonadGen m =>
  ReaderT [Property PriceOracleModel] m (Maybe TestDatumParameters)
genInputDatumParameters = do
  properties' <- ask
  if HasIncorrectInputDatum `elem` properties'
    then pure Nothing
    else do
      walletIdx <- genKnownWalletIdx
      t <- genInputTimeStamp
      pure $ Just $ TestDatumParameters walletIdx t

genStateTokenCurrencySymbol ::
  forall (m :: Type -> Type).
  MonadGen m =>
  ReaderT [Property PriceOracleModel] m CurrencySymbol
genStateTokenCurrencySymbol = do
  properties' <- ask
  if StateTokenNotReturned `elem` properties'
    then HP.currencySymbol
    else pure $ fst correctNFTCurrency

genStateTokenTokenName ::
  forall (m :: Type -> Type).
  MonadGen m =>
  ReaderT [Property PriceOracleModel] m TokenName
genStateTokenTokenName = do
  properties' <- ask
  --TODO disambiguate. StateTokenNotReturned should be distinct from StateTokenCurrencyCorrect
  if StateTokenNotReturned `elem` properties'
    then HP.tokenName
    else pure $ snd correctNFTCurrency

genStateTokenAmount ::
  forall (m :: Type -> Type).
  MonadGen m =>
  ReaderT [Property PriceOracleModel] m Integer
genStateTokenAmount = do
  properties' <- ask
  if StateTokenNotReturned `elem` properties'
    then do
      b <- Gen.bool
      if b
        then pure 0
        else Gen.integral (Range.linear 2 5)
    else pure 1

genStateToken ::
  forall (m :: Type -> Type).
  MonadGen m =>
  ReaderT [Property PriceOracleModel] m Value
genStateToken = do
  s <- genStateTokenCurrencySymbol
  n <- genStateTokenTokenName
  v <- genStateTokenAmount
  pure $ singleton s n v
