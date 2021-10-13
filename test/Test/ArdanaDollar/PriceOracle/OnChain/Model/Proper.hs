{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE RecordWildCards #-}
module Test.ArdanaDollar.PriceOracle.OnChain.Model.Proper (
  priceOracleTest
   ) where
import Proper.Plutus
import Control.Monad.Trans.Reader
  ( ReaderT (runReaderT),
    ask,
  )
import Data.Kind
  ( Type,
  )
import Hedgehog
  ( MonadGen,
    checkParallel,
  )
import Hedgehog.Gen qualified as Gen
import Hedgehog.Gen.Plutus qualified as HP
import Hedgehog.Range qualified as Range
import Ledger
  ( CurrencySymbol,
    TokenName,
    Value,
  )
import PlutusTx.Prelude ( BuiltinByteString )
import Plutus.V1.Ledger.Api
  ( getValue
  )
import Plutus.V1.Ledger.Value
  ( singleton
  )
import PlutusTx.AssocMap qualified as AssocMap
import Prelude (
    IO,
    Enum,
    Eq,
    Ord,
    Bounded,
    Show,
    Integer,
    Bool(..),
    Maybe(..),
    (==),
    (/=),
    (&&),
    (<=),
    (>=),
    (>),
    (-),
    (+),
    ($),
    flip,
    uncurry,
    fst,
    snd,
    pure,
    elem,
    filter,
  )
import Control.Monad
  ( void,
  )

priceOracleTest :: IO ()
priceOracleTest = do
  void $ checkParallel $ selfTestGroup PriceOracleModel
  void $ checkParallel $ validatorTestGroup PriceOracleModel

data PriceOracleModel = PriceOracleModel deriving (Show)

data SpenderParams = NoSigner | JustSignedBy Integer | SignedByWithValue Integer Value
  deriving (Show)

data StateUTXOParams = StateUTXOParams
  { stateTokenValue :: Value
  , stateDatumValue :: TestDatumParameters
  }
  deriving (Show)

data TestDatumParameters = TestDatumParameters
  { signedByWallet :: Integer
  , timeStamp :: Integer
  }
  deriving (Show)

instance Proper PriceOracleModel where

  data Model PriceOracleModel =
    TestParameters
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

  data Check PriceOracleModel
    = OutputDatumTimestampInRange
    | RangeWithinSizeLimit
    | OutputDatumSignedByOwner
    | TransactionSignedByOwner
    | StateTokenReturned
    deriving stock (Enum, Eq, Ord, Bounded, Show)

  check = flip doCheck
  genModel = genModel'
  reify _ = pure () -- TODO make this do the test

instance IsCheck (Check PriceOracleModel)

---------------------------------------------------------------------------------
-- TODO use the real currency symbol
mockCurrencySymbol :: CurrencySymbol
mockCurrencySymbol = "123456789012345678901234567890ef"

correctNFTCurrency :: (CurrencySymbol, TokenName)
correctNFTCurrency = (mockCurrencySymbol, "PriceTracking")

--correctStateTokenValue :: Value
--correctStateTokenValue = uncurry singleton correctNFTCurrency 1
---------------------------------------------------------------------------------

-- Check
---------------------------------------------------------------------------------

doCheck :: Check PriceOracleModel -> Model PriceOracleModel -> Bool
doCheck OutputDatumTimestampInRange = outputDatumInRange
doCheck RangeWithinSizeLimit = rangeWithinSizeLimit
doCheck OutputDatumSignedByOwner = outputDatumSignedByOwner
doCheck TransactionSignedByOwner = txSignedByOwner
doCheck StateTokenReturned = stateTokenReturned

type ModelCheck = Model PriceOracleModel -> Bool

outputDatumInRange :: ModelCheck
outputDatumInRange TestParameters {..} =
  let so = stateDatumValue outputParams
   in timeStamp so >= timeRangeLowerBound && timeStamp so <= timeRangeUpperBound

rangeWithinSizeLimit :: ModelCheck
rangeWithinSizeLimit TestParameters {..} =
  let rangeLen = timeRangeUpperBound - timeRangeLowerBound
   in rangeLen > 0 && rangeLen <= 10000

outputDatumSignedByOwner :: ModelCheck
outputDatumSignedByOwner TestParameters {..} =
  let d = stateDatumValue outputParams
   in signedByWallet d == ownerWallet

txSignedByOwner :: ModelCheck
txSignedByOwner TestParameters {..} = case transactorParams of
  NoSigner -> False
  JustSignedBy signer -> signer == ownerWallet
  SignedByWithValue signer _ -> signer == ownerWallet

stateTokenReturned :: ModelCheck
stateTokenReturned TestParameters {..} =
  case AssocMap.lookup mockCurrencySymbol $ getValue $ stateTokenValue outputParams of
    Nothing -> False
    Just so -> case AssocMap.lookup (snd stateNFTCurrency) so of
      Just 1 -> True
      _ -> False

-- Gen
---------------------------------------------------------------------------------

genModel' :: MonadGen m => [Check PriceOracleModel] -> m (Model PriceOracleModel)
genModel' = runReaderT genTestParameters


genTestParameters ::
  forall (m :: Type -> Type).
  MonadGen m =>
  ReaderT [Check PriceOracleModel] m (Model PriceOracleModel)
genTestParameters = do
  (tlb, tub) <- genTimeRange
  w <- genKnownWalletIdx
  sp <- genSpenderParams w
  ip <- genInputUTXOParams
  op <- genOutputUTXOParams w (tlb, tub)
  pc <- HP.builtinByteString (Range.linear 0 6)
  pure $
    TestParameters
      { stateNFTCurrency = correctNFTCurrency --TODO generate
      , timeRangeLowerBound = tlb
      , timeRangeUpperBound = tub
      , ownerWallet = w
      , transactorParams = sp
      , inputParams = ip
      , outputParams = op
      , peggedCurrency = pc
      }

-- if if if if if lol TODO read some Hedgehog for a nicer generator ideas
genSpenderParams ::
  forall (m :: Type -> Type).
  MonadGen m =>
  Integer ->
  ReaderT [Check PriceOracleModel] m SpenderParams
genSpenderParams w = do
  violations <- ask
  if TransactionSignedByOwner `elem` violations
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
  ReaderT [Check PriceOracleModel] m StateUTXOParams
genInputUTXOParams = do
  stok <- genStateToken
  d <- genInputDatumParameters
  pure $ StateUTXOParams stok d

genOutputUTXOParams ::
  forall (m :: Type -> Type).
  MonadGen m =>
  Integer ->
  (Integer, Integer) ->
  ReaderT [Check PriceOracleModel] m StateUTXOParams
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
  ReaderT [Check PriceOracleModel] m Integer
genOutputTimeStamp ts = do
  violations <- ask
  if OutputDatumTimestampInRange `elem` violations
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
  ReaderT [Check PriceOracleModel] m Integer
genInputTimeStamp = Gen.integral (Range.linear 0 100_000)

genTimeRange ::
  forall (m :: Type -> Type).
  MonadGen m =>
  ReaderT [Check PriceOracleModel] m (Integer, Integer)
genTimeRange = do
  violations <- ask
  if RangeWithinSizeLimit `elem` violations
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
  ReaderT [Check PriceOracleModel] m TestDatumParameters
genOutputDatumParameters w ts = do
  violations <- ask
  walletIdx <-
    if OutputDatumSignedByOwner `elem` violations
      then genWalletIdxOtherThan w
      else pure w
  t <- genOutputTimeStamp ts
  pure $ TestDatumParameters walletIdx t

genInputDatumParameters ::
  forall (m :: Type -> Type).
  MonadGen m =>
  ReaderT [Check PriceOracleModel] m TestDatumParameters
genInputDatumParameters = do
  walletIdx <- genKnownWalletIdx
  t <- genInputTimeStamp
  pure $ TestDatumParameters walletIdx t

genStateTokenCurrencySymbol ::
  forall (m :: Type -> Type).
  MonadGen m =>
  ReaderT [Check PriceOracleModel] m CurrencySymbol
genStateTokenCurrencySymbol = do
  violations <- ask
  if StateTokenReturned `elem` violations
    then HP.currencySymbol
    else pure $ fst correctNFTCurrency

genStateTokenTokenName ::
  forall (m :: Type -> Type).
  MonadGen m =>
  ReaderT [Check PriceOracleModel] m TokenName
genStateTokenTokenName = do
  violations <- ask
  if StateTokenReturned `elem` violations
    then HP.tokenName
    else pure $ snd correctNFTCurrency

genStateTokenAmount ::
  forall (m :: Type -> Type).
  MonadGen m =>
  ReaderT [Check PriceOracleModel] m Integer
genStateTokenAmount = do
  violations <- ask
  if StateTokenReturned `elem` violations
    then do
      b <- Gen.bool
      if b
        then pure 0
        else Gen.integral (Range.linear 2 5)
    else pure 1

genStateToken ::
  forall (m :: Type -> Type).
  MonadGen m =>
  ReaderT [Check PriceOracleModel] m Value
genStateToken = do
  s <- genStateTokenCurrencySymbol
  n <- genStateTokenTokenName
  v <- genStateTokenAmount
  pure $ singleton s n v
