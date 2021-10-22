# dUSD minting permission token (minting policy)

Parameters:
```
data DUSDPermissionParameters = DUSDPermissionParameter
  { vaultStateToken :: AssetClass
  }
```

This is the minting policy for v1 of ardana-dollar.

You can always burn.

Rules for minting:
- `n` of `vaultStateToken` is being minted where `n > 0`.
