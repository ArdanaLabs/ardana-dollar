# Cost Centers (Validator)

Parameters:
```haskell
data CostCenterParams = CostCenterParams
  { treasuryStateToken :: AssetClass
  }
```

Datum:
```haskell
data CostCenterDatum = CostCenterDatum
  { identifier :: BuiltinByteString
  }
```

Redeemer:
```haskell
data CostCenterRedeemer = CostCenterRedeemer
  { treasuryStateHash :: TokenName
  }
```

To unlock a UTXO locked with this datum:

- There must be a `treasuryState :: TreasuryState` with a corresponding certification
  token for `treasuryStateToken` with the name `treasuryStateHash`.
- `ivTo (txInfoValidRange _) < treasuryState.certTokenExpiration + treasuryState.timestamp`.
- `treasuryState.ownerAuthToken` must be minted or burned.
