module Test.ArdanaDollar.PriceOracle.OnChain.Model.Gen
  ( genTestParameters
  ) where

import Test.ArdanaDollar.PriceOracle.OnChain.Model.Parameters

import Ledger qualified
import Ledger.Value qualified as Value
import Plutus.V1.Ledger.Api (singleton)
import PlutusTx.Prelude

import Data.Kind (Type)
import Hedgehog (MonadGen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Gen.Plutus qualified as HP
import Hedgehog.Range qualified as Range


genTestParameters :: forall (m :: Type -> Type). MonadGen m => m TestParameters
genTestParameters = do
  (tlb, tub) <- genTimeRange
  w <- genKnownWalletIdx
  sp <- genSpenderParams
  ip <- genStateUTXOParams
  op <- genStateUTXOParams
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

genSpenderParams :: forall (m :: Type -> Type). MonadGen m => m SpenderParams
genSpenderParams = do
  b <- Gen.bool
  if b
    then do
      w <- genKnownWalletIdx
      return $ JustSignedBy w
    else do
      -- hmmm janky ifology
      b' <- Gen.bool
      if b'
        then return NoSigner
        else do
          w <- genKnownWalletIdx
          v <- HP.value
          return $ SignedByWithValue w v

genStateUTXOParams :: forall (m :: Type -> Type). MonadGen m => m StateUTXOParams
genStateUTXOParams = do
  stok <- genStateToken
  d <- genTestDatumParameters
  return $ StateUTXOParams stok d

genKnownWalletIdx :: forall (m :: Type -> Type). MonadGen m => m Integer
genKnownWalletIdx = Gen.integral (Range.linear 1 5)

genTimeStamp :: forall (m :: Type -> Type). MonadGen m => m Integer
genTimeStamp = Gen.integral (Range.linear 0 100_000)

genTimeRange :: forall (m :: Type -> Type). MonadGen m => m (Integer, Integer)
genTimeRange = do
  t1 <- Gen.integral (Range.linear 0 99_000)
  t2 <- Gen.integral (Range.linear t1 100_000)
  return (t1, t2)

genTestDatumParameters :: forall (m :: Type -> Type). MonadGen m => m TestDatumParameters
genTestDatumParameters = do
  walletIdx <- genKnownWalletIdx
  timestamped <- genTimeStamp
  return $ TestDatumParameters walletIdx timestamped

genStateTokenCurrencySymbol :: forall (m :: Type -> Type). MonadGen m => m Value.CurrencySymbol
genStateTokenCurrencySymbol = do
  b <- Gen.bool
  if b
    then return $ fst correctNFTCurrency
    else HP.currencySymbol

genStateTokenTokenName :: forall (m :: Type -> Type). MonadGen m => m Value.TokenName
genStateTokenTokenName = do
  b <- Gen.bool
  if b
    then return $ snd correctNFTCurrency
    else HP.tokenName

genStateTokenAmount :: forall (m :: Type -> Type). MonadGen m => m Integer
genStateTokenAmount = Gen.integral (Range.linear 0 3)

genStateToken :: forall (m :: Type -> Type). MonadGen m => m Ledger.Value
genStateToken = do
  s <- genStateTokenCurrencySymbol
  n <- genStateTokenTokenName
  v <- genStateTokenAmount
  return $ singleton s n v

