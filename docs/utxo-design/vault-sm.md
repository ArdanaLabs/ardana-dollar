# Vault (State Machine)

TODO: Handle liquidation.

Purpose: Holds the collateral and records the borrowed dUSD for a user.

Parameterized by:
```haskell
data VaultParams = VaultParams
  { dUSD :: AssetClass
  , adminStateToken :: CurrencySymbol
  , treasuryStateToken :: CurrencySymbol
  , costCenterHash :: ValidatorHash
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
  where
  ```haskell
  interest' :: Natural
  interest' =
    2^(
      (new.interestTimestamp - old.interestTimestamp)
      * adminState.interestRate
    )
    * (old.interest + old.borrowPrincipal)
    - old.borrowPrincipal
  ```

### Calculating exponent

Though from what is written above, you might think that on-chain you need
to calculate a non-integral exponent, no such calculation is necessary.
You can go the other way around, and check the following:
```haskell
((newInterest + old.borrowPrincipal) / (old.interest + old.borrowPrincipal))^d
==
2^(
  (new.interestTimestamp - old.interestTimestamp)
  * adminState.interestRate
  * d
)
```
such that `d :: Integer` makes the exponent integral.
How do we find this `d`? We already have it, because we're not working with floats,
but rather rationals. `d` is equivalent to the divisor of the rational
`(new.interestTimestamp - old.interestTimestamp) * adminState.interestRate :: Rational`.

In practice, this means you remove the divisor `d` from this `Rational`, making it an
`Integer`, and multiply the other side by `d`.
Now the problem is reduced to calculating `2^x` for some `x :: Integer`, which is trivial.
Unfortunately, it seems that there is no primitive for doing bit shifts (https://github.com/input-output-hk/plutus/issues/4168),
but repeated multiplication is a viable alternative.

## Acts

**NB:** The output datum has to be available in `txInfoData`.

### AddCollateralAct

Purpose: Allow the user to add collateral.

- `new ≡ old { userAuthToken = new.userAuthToken }`
- For each token, there must be just as much in the new UTXO as
  there is in the old UTXO.
- `assetClassValueOf txInfoMint dUSD ≡ 0`.
- `userAuthToken` must be minted or burned.

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
- There must be an `oracleDatum :: OracleDatum` certified with a certification
  token for `adminState.oracleToken` with the name `oracleHash`.
- `ivTo (txInfoValidRange _) < oracleDatum.certTokenExpiration + oracleDatum.timestamp`.
- `assetClassValueOf newValue collateralCurrency > oracleDatum.ratio * (borrowPrincipal + new.interest) * adminState.minCollateralRatio`.
- Interest algorithm must be applied.
- ```haskell
  new ≡ old
    { interest = interest'
    , interestTimestamp = new.interestTimestamp
    , userAuthToken = new.userAuthToken
    }
  ```
- `userAuthToken` must be minted or burned.

### AddBorrowAct

Purpose: Allow the user to borrow dUSD.

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
- There must be an `oracleDatum :: OracleDatum` certified with a certification
  token for `adminState.oracleToken` with the name `oracleHash`.
- `ivTo (txInfoValidRange _) < oracleDatum.certTokenExpiration + oracleDatum.timestamp`.
- `assetClassValueOf txInfoMint dUSD > 0`.
- `assetClassValueOf newValue collateralCurrency ≡ assetClassValueOf oldValue collateralCurrency`.
- `assetClassValueOf newValue collateralCurrency > oracleDatum.ratio * (new.borrowPrincipal + new.interest) * adminState.minCollateralRatio`.
- Interest algorithm must be applied.
- ```haskell
  new ≡ old
    { interest = interest'
    , borrowPrincipal = old.borrowPrincipal + assetClassValueOf txInfoMint dUSD
    , interestTimestamp = new.interestTimestamp
    , userAuthToken = new.userAuthToken
    }
  ```
- `userAuthToken` must be minted or burned.

### RepayBorrowAct

Purpose: Allow the user to repay a borrow

Arguments:
```haskell
adminStateHash :: TokenName
```

- There must be an `adminState :: AdminState` certified with a certification
  token for `adminStateToken` with the name `adminStateHash`.
- `adminState.active`.
- `ivTo (txInfoValidRange _) < adminState.certTokenExpiration + adminState.timestamp``
- `adminState.collateralCurrency` must be equal to `collateralCurrency`.
- `assetClassValueOf txInfoMint dUSD <= 0`.
- `assetClassValueOf newValue collateralCurrency ≡ assetClassValueOf oldValue collateralCurrency`.
- Interest algorithm must be applied.
- `new.interest ≡ 0 || new.borrowPrincipal == old.borrowPrincipal`.
- ```haskell
  let reward = flip assetClassValueOf dUSD . foldMap txOutValue . filter (\o -> o.txOutAddress == costCenterHash && findDatum _ o.txOutDatumHash == CostCenterDatum "vault") $ txInfoOutputs in
  new ≡ old
    { interest = max(interest' - reward, 0)
    , borrowPrincipal = max(old.borrowPrincipal + assetClassValueOf (txInfoMint _) dUSD, 0)
    , interestTimestamp = new.interestTimestamp
    , userAuthToken = new.userAuthToken
    }
  ```
- `userAuthToken` must be minted or burned.

### LiquidationAct

FIXME: What do we do when the ratio is below 100%?

Purpose: When the collateral ratio `r` is below the minimum, but above 100%, we allow
liquidations, where a buyer can buy `(r - 1) * k + 1` of the collateral per dUSD, until
the minimum ratio is attained again.
This is an auction in MakerDAO, but we went for this approach for the sake of simplicity.

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
- There must be an `oracleDatum :: OracleDatum` certified with a certification
  token for `adminState.oracleToken` with the name `oracleHash`.
- `ivTo (txInfoValidRange _) < oracleDatum.certTokenExpiration + oracleDatum.timestamp`.
- `assetClassValueOf txInfoMint dUSD <= 0`.
- `assetClassValueOf newValue collateralCurrency * oracleDatum.ratio ≡ collateral'`.
- `assetClassValueOf oldValue collateralCurrency * oracleDatum.ratio ≡ collateral`.
- Interest algorithm must be applied.
- `borrow = old.borrowPrincipal + interest'`.
- `new.interest ≡ 0 || new.borrowPrincipal == old.borrowPrincipal`.
- `let reward = flip assetClassValueOf dUSD . foldMap txOutValue . filter (\o -> o.txOutAddress == costCenterHash && findDatum _ o.txOutDatumHash == CostCenterDatum "vault") $ txInfoOutputs`
- `reward >= old.interest - new.interest`
- `assetClassValueOf (txInfoMint _) dUSD <= negate $ max((borrow - borrow') - (old.interest - new.interest), 0)`
- ```haskell
  new ≡ old
    { interest = max(interest' - (borrow - borrow'), 0)
    , borrowPrincipal = max(old.borrowPrincipal + assetClassValueOf (txInfoMint _) dUSD, 0)
    , interestTimestamp = new.interestTimestamp
    }
  ```

Math:
```haskell
-- TODO: Formalise

liquidationTarget > 0
1 > liquidationRate > 0

dUSDToPay = borrow - borrow'
-- `collateral` has already been divided by the oracle ratio
collateralToReceive = collateral - collateral'

x = collateral
x' = collateral'
y = borrow
y' = borrow'
k = liquidationRate
n = minCollateralRatio + liquidationTarget

x / y < minCollateralRatio

x' / y' = n

(x / y) < (x' / y')

(x - x') / (y - y') = m

m = ((x / y) - 1) * k + 1

-- derived from the above

x' = n * y'

(x - y' * n) / (y - y') = m

x / (y - y') - y' * n / (y - y') = m

x - y' * n = m * (y - y')

x - y' * n = m * y - m * y'

x - m * y = y' * n - m * y'

x - m * y = y' * n + y' * -m

x - m * y = y' * (n - m)

(x - m * y) / (n - m) = y'

(x - (((x / y) - 1) * k + 1) * y) / (n - (((x / y) - 1) * k + 1)) = y'

(x - ((x / y) - 1) * k * y - y) / (n - (((x / y) - 1) * k + 1)) = y'

(x - ((x / y) - 1) * k * y - y) / (n - ((x / y) - 1) * k - 1) = y'

(x - (x - y) * k - y) / (n - ((x / y) - 1) * k - 1) = y'

(x - k * x + k * y - y) / (n - ((x / y) - 1) * k - 1) = y'

(x - k * x + k * y - y) / (n - (x / y * k - k) - 1) = y'

(x - k * x + k * y - y) / (n - x / y * k + k - 1) = y'
```
