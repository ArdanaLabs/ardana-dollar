# Admin (State Machine)

Purpose: Admin holds the global state for a `(peggedCurrency, collateralCurrency)` pair.

Parameterized by:
```haskell
data AdminParams = AdminParams
  { treasuryStateToken :: AssetClass
  }
```

## Datum

```haskell
data AdminState = AdminState
  { active :: Bool
  , collateralCurrency :: AssetClass
  , interestRate :: Rational
  , liquidationBenefitCap :: Rational
  , liquidationBenefitFloor :: Rational
  , minCollateralRatio :: Rational
  , oracleToken :: AssetClass,
  , certTokenStart :: Natural
  , certTokenBase :: Natural
  , certTokenExpiration :: POSIXTime
  , validRangeSize :: POSIXTime
  , refreshInterval :: POSIXTime
  , timestamp :: POSIXTime
  }
```

## Initial

Redeemer for minting policy:
```haskell
data AdminMPRedeemer = AdminMPRedeemer
  { treasuryStateHash :: TokenName
  }
```

- There must be a `treasuryState :: TreasuryState` certified with
  a token with the name `treasuryStateHash`.
- `ivTo (txInfoValidRange _) < treasuryState.certTokenExpiration + treasuryState.timestamp`.
- `treasuryState.ownerAuthToken` must be minted or burned.
- `timestamp ≡ ivFrom $ txInfoValidRange _`

## Certification Tokens

- Each act will create `certTokenStart` UTXOs with certification tokens.
- The validator for the UTXOs will require that `certTokenBase` UTXOs with the same
  token are made in the consuming transaction.
- The tokens are only valid for `certTokenExpiration` slots.

## Acts

**NB:** The output datum has to be available in `txInfoData`.

- `ivFrom (txInfoValidRange _) ≡ ivTo (txInfoValidRange _) - validRangeSize _`.
- `new.timestamp ≡ ivFrom $ txInfoValidRange info`.
- `new.collateralCurrency ≡ old.collateralCurrency`. 

### UpdateAct

```haskell
treasuryStateHash :: TokenName
```

- There must be a `treasuryState :: TreasuryState` certified with
  a token with the name `treasuryStateHash`.
- `ivTo (txInfoValidRange _) < treasuryState.certTokenExpiration + treasuryState.timestamp`.
- `treasuryState.ownerAuthToken` must be minted or burned.

### RefreshAct

- There must only be one output and only one input.
- Nothing must be minted.
- `new ≡ old`.
- `old.timestamp - ivTo (txInfoValidRange _) >= old.refreshInterval`
