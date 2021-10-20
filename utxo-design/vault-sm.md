# Vault (State Machine)

Purpose: Holds the collateral and records the borrowed dUSD for a user.

Parameterized by:
```haskell
data VaultParams = VaultParams
  { targetCurrency :: ByteString -- fiat currency identifier
  , collateralCurrency :: AssetClass
  , adminCert :: CurrencySymbol,
  , treasuryCert :: CurrencySymbol
  }
```

## Datum

```haskell
data VaultState = VaultState
  { borrowPrincipal :: Integer
  , lastStabilityFeeTime :: PosixTime
  }
```

## Token

This token is not one-shot and is interchangeable with
every Vault State Machine state token with the same `VaultParams`.

Minting policy:
- There must be exactly one output with the Vault validator, with datum
  `VaultState { borrowPrincipal = 0, lastStabilityFeeTime = 0 }`.
  There must only be Ada (minimum Ada requirement) and `collateralCurrency` in the UTXO.
