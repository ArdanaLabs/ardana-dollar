# Oracle (UTXO)

```haskell
data OracleDatum
  { ratio :: Rational -- collateral value / dUSD value
  , certTokenBase :: Natural
  -- The reason these two aren't merged, is that an implementation of the oracle
  -- will likely need both.
  , certTokenExpiration :: POSIXTime
  , timestamp :: POSIXTime
    extra :: Data -- Can be anything
  }
```
