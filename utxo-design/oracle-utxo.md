# Oracle (UTXO)

```haskell
data OracleDatum
  { ratio :: Rational -- collateral value / dUSD value
  , certTokenBase :: Natural
  , certTokenExpiration :: POSIXTime
  , timestamp :: POSIXTime
    extra :: Data -- Can be anything
  }
```
