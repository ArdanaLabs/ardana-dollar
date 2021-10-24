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
  { treasuryStateHash :: TokenName
  }
```

`dUSD` itself as an empty token name.

Rules when minting >= 0:
- There must be a `treasuryState :: TreasuryState` certified with
  a token with the name `treasuryStateHash`.
- `ivTo (txInfoValidRange _) < treasuryState.certTokenExpiration + treasuryState.timestamp`.
- The token `treasuryState.dUSDPermissionToken` must be available in the transaction.

You can always burn.
