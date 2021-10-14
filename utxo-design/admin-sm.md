# Admin (State Machine)

Purpose: Admin holds the global state for a `(peggedCurrency, collateralCurrency)` pair.

Parameterized by:
```haskell
data AdminParams = AdminParams
  { targetCurrency :: ByteString -- fiat currency identifier
  , collateralCurrency :: AssetClass
  , treasuryAddress :: ValidatorHash
  , treasuryStateTokenSymbol :: CurrencySymbol
  , adminStateCurrencySymbol :: CurrencySymbol
  , adminOperator :: PubKey
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
  , oracleToken :: AssetClass,
  , oracleCertification :: CurrencySymbol
  , oracleAddress :: Credential
  , certTokenStart :: Natural
  , certTokenBase :: Natural
  , certTokenExpiration :: Natural 
  }
```

## Token

Minting rules:
- `oneShotUtxo` must be consumed by the transaction.
- The state must be initialized to the following.

Can only mint if `oneShotUtxo` is consumed,
and if the state is the following:
```haskell
data AdminState = AdminState
  { active :: Bool
  , stabilityFeeMultiplier :: Rational
  , liquidationBenefitCap :: Rational
  , liquidationBenefitFloor :: Rational
  , minCollateralRatio :: Rational
  , lastUpdated :: PosixTime
  }
AdminState
  { active = False
  , stabilityFeeMultiplier = 1
  , liquidationBenefitCap = 1 % 5 -- 20%
  , liquidationBenefitFloor = 1 % 20 -- 5%
  , minCollateralRatio = 3 % 2 -- 150%
  , lastUpdated = currentTime
  }
```

## Acts

### UpdateAdminStateAct
Purpose: Allows `ownerAddress` to do anything.

- Transaction must be signed by `ownerAddress`.

### RefreshAdminStateAct
Purpose: allows a trusted automated bot to refresh the admin state without updating it so it does not go stale without the `ownerAddress`

- Must be signed by the `adminOperator`.

### InitiateUpgradeAct

FIXME: Is this needed when we have `UpdateAdminStateAct`?

Purpose: Triggers Upgrade procedure by securely sending an UpgradeContractToken to the Treasury.

- `InitiateUpgradeAct` for Treasury state machine also in transaction.
- Must be signed by `ownerAddress`.
- Must consume UTXO containing `UpgradeContractToken` locked by `treasuryAddress`.
- Must set `active` to `False`.
- Must create UTXO with `UpgradeContractToken` locked with `treasuryAddress`,
  with datum set to `UpgradeContract { newContract = Just x }` for some `x`.
  datum must be  (set `UpgradeContract.newContract` to `Just InitiateUpgradeAct.newContract`)

FIXME: - UpgradeContract token -> `InitiateUpgradeAct.newContract` address (set `newContract` to `Nothing`)

## Certification Tokens

- Each act will create `certTokenStart` UTXOs with the certification token.
- The validator for the UTXOs will require that `certTokenBase` UTXOs with the same
  token are made in the consuming transaction.
- The tokens are only valid for `certTokenExpiration` slots.
