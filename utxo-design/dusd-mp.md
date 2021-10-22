# dUSD minting policy

Parameters:
```haskell
data DUSDParameters = DUSDParameters
  { treasuryStateToken :: AssetClass
  }
```

Redeemer:
```haskell
data DUSDRedeemer = DUSDRedeemer
  { treasuryState :: TokenName
  }
```

`dUSD` itself as an empty token name.

Rules when minting >= 0:
- There must be a certification token with `treasuryState` as name.
- The token `dUSDPermissionToken` must be available in the transaction,
  it will be read from the datum which hash is `treasuryState`.

You can always burn.
