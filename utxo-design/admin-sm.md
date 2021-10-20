# Admin (State Machine)

Purpose: Admin holds the global state for a `(peggedCurrency, collateralCurrency)` pair.

Parameterized by:
```haskell
data AdminParams = AdminParams
  { targetCurrency :: ByteString -- fiat currency identifier
  , collateralCurrency :: AssetClass
  , treasuryStateToken :: AssetClass
  , ownerAddress :: PubKey
  , oneShotUtxo :: TxOutRef
  }
```

## Datum

```haskell
data AdminState = AdminState
  { active :: Bool
  , stabilityFeeMultiplier :: Rational
  , liquidationBenefitCap :: Rational
  , liquidationBenefitFloor :: Rational
  , minCollateralRatio :: Rational
  , lastUpdated :: PosixTime
  , oracleStateToken :: AssetClass,
  , certTokenStart :: Natural
  , certTokenBase :: Natural
  , certTokenExpiration :: POSIXTime
  , validRangeSize :: POSIXTime
  }
```

## Initial

- `oneShotUtxo` must be consumed by the transaction.

```haskell
outputDatum ≡ outputDatum
  { active = True
  , lastUpdated = currentTime
  }
  where currentTime = ivFrom $ txInfoValidRange _
```

## Certification Tokens

- Each act will create `certTokenStart` UTXOs with certification tokens.
- The validator for the UTXOs will require that `certTokenBase` UTXOs with the same
  token are made in the consuming transaction.
- The tokens are only valid for `certTokenExpiration` slots.

## Acts

- `ivFrom (txInfoValidRange _) ≡ ivTo (txInfoValidRange _) - validRangeSize _`

### UpdateAdminStateAct
Purpose: Allows `ownerAddress` to do anything.

- Transaction must be signed by `ownerAddress`.

### InitiateUpgradeAct

FIXME: Is this needed when we have `UpdateAdminStateAct`?
FIXME: Will likely need to change.

Purpose: Triggers Upgrade procedure by securely sending an UpgradeContractToken to the Treasury.

- `InitiateUpgradeAct` for Treasury state machine also in transaction.
- Must be signed by `ownerAddress`.
- Must consume UTXO containing `UpgradeContractToken` locked by `treasuryAddress`.
- Must set `active` to `False`.
- Must create UTXO with `UpgradeContractToken` locked with `treasuryAddress`,
  with datum set to `UpgradeContract { newContract = Just x }` for some `x`.
  datum must be  (set `UpgradeContract.newContract` to `Just InitiateUpgradeAct.newContract`)

FIXME: - UpgradeContract token -> `InitiateUpgradeAct.newContract` address (set `newContract` to `Nothing`)
