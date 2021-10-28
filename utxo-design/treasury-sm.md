# Treasury (State Machine)

Purpose: The Treasury is a structure that allows for a store of funds across multiple versions of the protocol, totals of these funds are stored in `CostCenterState` datum.

Note: the treasury scripts will not be updated as part of version 2 or any future version of the protocol, Instead, the treasury will integrate with those future versions of the protocol without any code changes.

Parameterized by:
```haskell
data TreasuryParams = TreasuryParams
  { targetCurrency :: ByteString -- fiat currency identifier
  , danaAssetClass :: AssetClass
  , oneShotUtxo :: TxOutRef
  }
```

## Datum

```haskell
data TreasuryState = TreasuryState
  { certTokenStart :: Natural
  , certTokenBase :: Natural
  , certTokenExpiration :: POSIXTime
  , refreshInterval :: POSIXTime
  , validRangeSize :: POSIXTime
  , timestamp :: POSIXTime
  , ownerAuthToken :: AssetClass
  , dUSDPermissionToken :: AssetClass
  -- This is useful if the minting policy for `ownerAuthToken`
  -- wants to store extra data in the state.
  , extra :: BuiltinData
  }
```

## Initial

- `oneShotUtxo` must be consumed by the transaction.

## Certification Tokens

- Each act will create `old.certTokenStart` UTXOs with certification tokens.
- The validator for the UTXOs will require that `old.certTokenBase` UTXOs with the same
  token are made in the consuming transaction.
- The tokens are only valid for `old.certTokenExpiration` slots.

## Acts

**NB:** The output datum has to be available in `txInfoData`.

- `ivFrom (txInfoValidRange _) ≡ ivTo (txInfoValidRange _) - old.validRangeSize _`
- `new.timestamp ≡ ivFrom $ txInfoValidRange info`

### RefreshAct

- There must only one input, one output for this the treasury, and the outputs
  for the certification tokens.
- Only the certification tokens must be minted.
- `new ≡ old`.
- `old.timestamp - ivTo (txInfoValidRange _) >= old.refreshInterval`

### UpdateAct

- `ownerAuthToken` must be minted or burned.
