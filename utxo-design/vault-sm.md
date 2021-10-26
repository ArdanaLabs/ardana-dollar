# Vault (State Machine)

TODO: Handle liquidation.

Purpose: Holds the collateral and records the borrowed dUSD for a user.

Parameterized by:
```haskell
data VaultParams = VaultParams
  { dUSD :: AssetClass
  , adminStateToken :: CurrencySymbol
  , treasuryStateToken :: CurrencySymbol
  }
```

## Datum

```haskell
data VaultState = VaultState
  { borrowPrincipal :: Natural
  , interest :: Natural
  , interestTimestamp :: POSIXTime
  , userAuthToken :: AssetClass
  , collateralCurrency :: AssetClass
  }
```

## Initial

- `borrowPrincipal` must be 0.
- `interestTimestamp` must be equal to `ivFrom (txInfoValidRange _)`.
- `userAuthToken` must be minted or burned.
- There must be no values in the output other than
  `collateralCurrency`, ADA, and the state token.
- `assetClassValueOf txInfoMint dUSD ≡ 0`.

## Interest algorithm

- `new.interestTimestamp >= old.interestTimestamp`.
- `new.interestTimestamp = ivTo (txInfoValidRange _)`.
- `new ≡ old { interest = newInterest }`,
  where
  ```haskell
  -- FIXME: This might be too complex for Plutus.
  newInterest :: Natural
  newInterest =
    e^(
      (new.interestTimestamp - old.interestTimestamp)
      * adminState.interestRate
    )
    * (old.interest + old.borrowPrincipal)
    - old.borrowPrincipal
  ```

## Acts

**NB:** The output datum has to be available in `txInfoData`.

### AddCollateralAct

Purpose: Allow the user to add collateral.

- `old ≡ new`
- For each token, there must be just as much in the new UTXO as
  there is in the old UTXO.
- `assetClassValueOf txInfoMint dUSD ≡ 0`.

### RemoveCollateralAct

Purpose: Allow the user to withdraw collateral, if doing so does not put the Vault under the `minimumCollateralRatio`.

Arguments:
```haskell
adminStateHash :: TokenName
oracleHash :: TokenName
```

- `assetClassValueOf txInfoMint dUSD ≡ 0`.
- There must be an `adminState :: AdminState` certified with a certification
  token for `adminStateToken` with the name `adminStateHash`.
- `adminState.active`.
- `ivTo (txInfoValidRange _) < adminState.certTokenExpiration + adminState.timestamp``
- `adminState.collateralCurrency` must be equal to `collateralCurrency`.
- There must be an `oracleDatum :: OracleDatum` ceritifed with a certification
  token for `adminState.oracleToken` with the name `oracleHash`.
- `ivTo (txInfoValidRange _) < oracleDatum.certTokenExpiration + oracleDatum.timestamp`.
- `assetClassValueOf newValue collateralCurrency > oracleDatum.ratio * (borrowPrincipal + new.interest) * adminState.minCollateralRatio`.
- Interest algorithm must be applied.

### AddBorrowAct

Purpose: Allow the user to repay a borrow

Arguments:
```haskell
adminStateHash :: TokenName
oracleHash :: TokenName
```

- There must be an `adminState :: AdminState` certified with a certification
  token for `adminStateToken` with the name `adminStateHash`.
- `adminState.active`.
- `ivTo (txInfoValidRange _) < adminState.certTokenExpiration + adminState.timestamp``
- `adminState.collateralCurrency` must be equal to `collateralCurrency`.
- There must be an `oracleDatum :: OracleDatum` ceritifed with a certification
  token for `adminState.oracleToken` with the name `oracleHash`.
- `ivTo (txInfoValidRange _) < oracleDatum.certTokenExpiration + oracleDatum.timestamp`.
- `new.borrowPrincipal = old.borrowPrincipal + assetClassValueOf txInfoMint dUSD`.
- `assetClassValueOf txInfoMint dUSD > 0`.
- `assetClassValueOf newValue collateralCurrency ≡ assetClassValueOf oldValue collateralCurrency`.
- `assetClassValueOf newValue collateralCurrency > oracleDatum.ratio * (new.borrowPrincipal + new.interest) * adminState.minCollateralRatio`.
- Interest algorithm must be applied.

### RepayBorrowAct

Purpose: Allow the user to repay a borrow

Arguments:
```haskell
adminStateHash :: TokenName
oracleHash :: TokenName
```

- There must be an `adminState :: AdminState` certified with a certification
  token for `adminStateToken` with the name `adminStateHash`.
- `adminState.active`.
- `ivTo (txInfoValidRange _) < adminState.certTokenExpiration + adminState.timestamp``
- `adminState.collateralCurrency` must be equal to `collateralCurrency`.
- There must be an `oracleDatum :: OracleDatum` ceritifed with a certification
  token for `adminState.oracleToken` with the name `oracleHash`.
- `ivTo (txInfoValidRange _) < oracleDatum.certTokenExpiration + oracleDatum.timestamp`.
- `new.interest = max(old.interest + assetClassValueOf txInfoMint dUSD, 0)`.
- `new.borrowPrincipal = max(min(old.borrowPrincipal + old.interest + assetClassValueOf txInfoMint dUSD, old.borrowPrincipal), 0)`.
- `assetClassValueOf txInfoMint dUSD < 0`.
- `assetClassValueOf newValue collateralCurrency ≡ assetClassValueOf oldValue collateralCurrency`.
- `assetClassValueOf newValue collateralCurrency > oracleDatum.ratio * (new.borrowPrincipal + new.interest) * adminState.minCollateralRatio`.
- Interest algorithm must be applied.