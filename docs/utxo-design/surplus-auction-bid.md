# Surplus Auction Bid Validator

Purpose:
When there is too little dUSD on the market compared to the total collateral,
a surplus auction can happen in order to balance the ratio.

The design chosen here is quite simple, and essentially delegates the task
of calculating the ratio of collateral and dUSD to the owner of the treasury.
The bidders simply lock their bid with this validator, and then the treasury
owner must consider the bid.

The same design is used for debt auctions.

Essentially, the bids trade DANA for dUSD.

Parameters:
```haskell
data SurplusAuctionBidRedeemer = SurplusAuctionBidRedeemer
  { treasuryStateHash :: TokenName
  , dana :: AssetClass
  , dUSD :: AssetClass
  }
```


Datum:
```haskell
data SurplusAuctionBidDatum = SurplusAuctionBidDatum
  { owner :: Address
  , dUSDCount :: Natural
  , expiration :: POSIXTime
  }
```

Redeemer:
```haskell
data SurplusAuctionBidRedeemer = SurplusAuctionBidRedeemer
  { treasuryStateHash :: Maybe TokenName
  }
```

Rules for consumption:
- If `ivTo (txInfoValidRange _) < expiration` then
  + There must be a `treasuryState :: TreasuryState` with a corresponding certification
  token for `treasuryStateToken` with the name `treasuryStateHash`.
  + `ivTo (txInfoValidRange _) < treasuryState.certTokenExpiration + treasuryState.timestamp`.
  + `treasuryState.ownerAuthToken` must be minted or burned.
  + At least `dUSDCount` of `dUSD` must go to `owner`.
  + An amount of `dana` equivalent to the amount in this UTXO must be burned.
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
should collect the DANA from the bids that are the cheapest.
