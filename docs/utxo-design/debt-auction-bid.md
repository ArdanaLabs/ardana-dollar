# Debt Auction Bid Validator (WIP)

Purpose: When there is unbacked dUSD on the more, a debt auction must happen.
The design chosen here is quite simple, and essentially delegates the task
of calculating the ratio of collateral and dUSD to the owner of the treasury.
The bidders simply lock their bid with this validator, and then the treasury
owner must consider the bid.

Parameters:
```haskell
data DebtAuctionBidRedeemer = DebtAuctionBidRedeemer
  { treasuryStateHash :: TokenName
  , dana :: AssetClass
  , dUSD :: AssetClass
  }
```


Datum:
```haskell
data DebtAuctionBidDatum = DebtAuctionBidDatum
  { owner :: Address
  , danaCount :: Natural
  , expiration :: POSIXTime
  }
```

Redeemer:
```haskell
data DebtAuctionBidRedeemer = DebtAuctionBidRedeemer
  { treasuryStateHash :: Maybe TokenName
  }
```

Rules for consumption:
- If `ivTo (txInfoValidRange _) < expiration` then
  + There must be a `treasuryState :: TreasuryState` with a corresponding certification
  token for `treasuryStateToken` with the name `treasuryStateHash`.
  + `ivTo (txInfoValidRange _) < treasuryState.certTokenExpiration + treasuryState.timestamp`.
  + `treasuryState.ownerAuthToken` must be minted or burned.
  + At least `danaCount` of `dana` must go to `owner`.
  + An amount of `dUSD` equivalent to the amount in this UTXO must be burned.
  + Any value in this UTXO that isn't burned must go to `owner`.
- Else
  + All the value in the UTXO must go back to `owner`.

## Off-chain

Auctions will be announced off-chain.
The selection of bids will also happen off-chain.
It makes most economical sense for the owner of the treasury
to pick the bids that are cheapest.

The treasury owner should announce the end date, then people
should make bids using this validator. Before the end, the owner
should collect the dUSD from the bids that are the cheapest.
