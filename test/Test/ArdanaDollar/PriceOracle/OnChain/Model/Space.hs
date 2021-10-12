module Test.ArdanaDollar.PriceOracle.OnChain.Model.Space (
  genTestParameters,
  genConstraintList,
) where

import Test.ArdanaDollar.PriceOracle.OnChain.Model.Constraints
import Test.ArdanaDollar.PriceOracle.OnChain.Model.Parameters

import Ledger qualified
import Ledger.Value qualified as Value
import Plutus.V1.Ledger.Api (singleton)
import PlutusTx.Prelude hiding (elem)
import Prelude (Bounded (..), elem)

import Control.Monad.Trans.Reader
import Data.Kind (Type)
import Hedgehog (MonadGen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Gen.Plutus qualified as HP
import Hedgehog.Range qualified as Range

-- we should be able to generate parameters that violate a given set of constraints
-- this brings some benefits
--    1. more efficient test case generation
--       - since we are not rejection sampling
--    2. we can test that the model generated indeed violates the constraints we expected it to
--       - this is more powerful than it seems at first
--       - doing this means we are expressing the constraints in terms of generators
--       - this means we have a third modelling perspective on the behaviour of the validator
--       - this increases the confidence we have in the model
--    3. we can more easily structure our tests in terms of HedgeHog properties
--       - e.g. property $ do
--                params <- forAll $ runReaderT genTestParameters [OutputDatumTimestampInRange]
--                b <- tastyPlutusTestRunner params
--                b === False
--       - e.g. property $ do
--                violations <- forAll constraintViolationLists
--                params <- runReaderT genTestParameters violations
--                violations === constraintViolations params
--    4. I speculate that the property checking nature of HedgeHog will make this style of test more compositional
--       - e.g. property $ do
--                v <- forall validOutputOfContractA
--                params <- forall $ validContractBWithInput v
--                b <- tastyPlutusTestRunnerForB params
--                b == True

genConstraintList ::
  forall (m :: Type -> Type).
  MonadGen m =>
  m [Constraint]
genConstraintList = Gen.subsequence [minBound .. maxBound]

genTestParameters ::
  forall (m :: Type -> Type).
  MonadGen m =>
  ReaderT [Constraint] m TestParameters
genTestParameters = do
  (tlb, tub) <- genTimeRange
  w <- genKnownWalletIdx
  sp <- genSpenderParams w
  ip <- genInputUTXOParams
  op <- genOutputUTXOParams w (tlb, tub)
  pc <- HP.builtinByteString (Range.linear 0 6)
  return $
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

-- if if if if if lol
genSpenderParams ::
  forall (m :: Type -> Type).
  MonadGen m =>
  Integer ->
  ReaderT [Constraint] m SpenderParams
genSpenderParams w = do
  violations <- ask
  if TransactionSignedByOwner `elem` violations
    then do
      b <- Gen.bool
      if b
        then return NoSigner
        else do
          b' <- Gen.bool
          if b'
            then do
              w' <- genWalletIdxOtherThan w
              return $ JustSignedBy w'
            else do
              w' <- genWalletIdxOtherThan w
              v <- HP.value
              return $ SignedByWithValue w' v
    else do
      b <- Gen.bool
      if b
        then return $ JustSignedBy w
        else do
          v <- HP.value
          return $ SignedByWithValue w v

genInputUTXOParams ::
  forall (m :: Type -> Type).
  MonadGen m =>
  ReaderT [Constraint] m StateUTXOParams
genInputUTXOParams = do
  stok <- genStateToken
  d <- genInputDatumParameters
  return $ StateUTXOParams stok d

genOutputUTXOParams ::
  forall (m :: Type -> Type).
  MonadGen m =>
  Integer ->
  (Integer, Integer) ->
  ReaderT [Constraint] m StateUTXOParams
genOutputUTXOParams w ts = do
  stok <- genStateToken
  d <- genOutputDatumParameters w ts
  return $ StateUTXOParams stok d

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
  ReaderT [Constraint] m Integer
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
  ReaderT [Constraint] m Integer
genInputTimeStamp = Gen.integral (Range.linear 0 100_000)

genTimeRange ::
  forall (m :: Type -> Type).
  MonadGen m =>
  ReaderT [Constraint] m (Integer, Integer)
genTimeRange = do
  violations <- ask
  if RangeWithinSizeLimit `elem` violations
    then do
      t1 <- Gen.integral (Range.linear 1 100_000)
      t2 <- Gen.integral (Range.linear (t1 + 10_1000) (t1 + 200_000))
      return (t1, t2)
    else do
      t1 <- Gen.integral (Range.linear 1 100_000)
      t2 <- Gen.integral (Range.linear (t1 + 1) (t1 + 10_000))
      return (t1, t2)

genOutputDatumParameters ::
  forall (m :: Type -> Type).
  MonadGen m =>
  Integer ->
  (Integer, Integer) ->
  ReaderT [Constraint] m TestDatumParameters
genOutputDatumParameters w ts = do
  violations <- ask
  walletIdx <-
    if OutputDatumSignedByOwner `elem` violations
      then genWalletIdxOtherThan w
      else return w
  t <- genOutputTimeStamp ts
  return $ TestDatumParameters walletIdx t

genInputDatumParameters ::
  forall (m :: Type -> Type).
  MonadGen m =>
  ReaderT [Constraint] m TestDatumParameters
genInputDatumParameters = do
  walletIdx <- genKnownWalletIdx
  t <- genInputTimeStamp
  return $ TestDatumParameters walletIdx t

genStateTokenCurrencySymbol ::
  forall (m :: Type -> Type).
  MonadGen m =>
  ReaderT [Constraint] m Value.CurrencySymbol
genStateTokenCurrencySymbol = do
  violations <- ask
  if StateTokenReturned `elem` violations
    then HP.currencySymbol
    else return $ fst correctNFTCurrency

genStateTokenTokenName ::
  forall (m :: Type -> Type).
  MonadGen m =>
  ReaderT [Constraint] m Value.TokenName
genStateTokenTokenName = do
  violations <- ask
  if StateTokenReturned `elem` violations
    then HP.tokenName
    else return $ snd correctNFTCurrency

genStateTokenAmount ::
  forall (m :: Type -> Type).
  MonadGen m =>
  ReaderT [Constraint] m Integer
genStateTokenAmount = do
  violations <- ask
  if StateTokenReturned `elem` violations
    then do
      b <- Gen.bool
      if b
        then return 0
        else Gen.integral (Range.linear 2 5)
    else return 1

genStateToken ::
  forall (m :: Type -> Type).
  MonadGen m =>
  ReaderT [Constraint] m Ledger.Value
genStateToken = do
  s <- genStateTokenCurrencySymbol
  n <- genStateTokenTokenName
  v <- genStateTokenAmount
  return $ singleton s n v
