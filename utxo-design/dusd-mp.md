# dUSD minting policy

```haskell
dUSD :: AssetClass
dUSD = AssetClass (dUSDMP, "dUSD")
```

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

Rules when minting >= 0:
- There must be a `treasuryState :: TreasuryState` with a corresponding certification
  token for `treasuryStateToken` with the name `treasuryStateHash`.
- `ivTo (txInfoValidRange _) < treasuryState.certTokenExpiration + treasuryState.timestamp`.
- The token `treasuryState.dUSDPermissionToken` must be available in the transaction.

You can always burn.
