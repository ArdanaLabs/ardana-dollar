{-# LANGUAGE RecordWildCards #-}

module Test.ArdanaDollar.PriceOracle.OnChain.Model.Constraints (
  Constraint (..),
  ModelCheck,
  constraintViolations,
) where

import Test.ArdanaDollar.PriceOracle.OnChain.Model.Parameters

import Plutus.V1.Ledger.Api (getValue)
import PlutusTx.AssocMap qualified as AssocMap
import PlutusTx.Prelude hiding (Enum, Eq, Ord)
import Prelude (Bounded (..), Enum, Eq, Ord, Show)

data Constraint
  = OutputDatumTimestampInRange
  | RangeWithinSizeLimit
  | OutputDatumSignedByOwner
  | TransactionSignedByOwner
  | StateTokenReturned
  deriving stock (Enum, Eq, Ord, Bounded, Show)

-- are we not missing StateTokenPresent?

type ModelCheck = TestParameters -> Bool

checkConstraint :: Constraint -> ModelCheck
checkConstraint OutputDatumTimestampInRange = outputDatumInRange
checkConstraint RangeWithinSizeLimit = rangeWithinSizeLimit
checkConstraint OutputDatumSignedByOwner = outputDatumSignedByOwner
checkConstraint TransactionSignedByOwner = txSignedByOwner
checkConstraint StateTokenReturned = stateTokenReturned

constraintViolations :: TestParameters -> [Constraint]
constraintViolations p = filter (not . flip checkConstraint p) [minBound .. maxBound]

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
