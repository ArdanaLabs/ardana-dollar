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
  { collateralCurrency :: AssetClass
  , borrowPrincipal :: Integer
  , lastStabilityFeeTime :: PosixTime
  }
```

## Token

This token is not one-shot and is interchangeable with
every Vault State Machine state token with the same `VaultParams`.

FIXME
