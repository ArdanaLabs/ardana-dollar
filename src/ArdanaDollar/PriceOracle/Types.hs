{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-specialise #-}

module ArdanaDollar.PriceOracle.Types (
  OracleMintingRedeemer(..),
  PriceTracking(..),
  PriceTrackingCopy(..),
  OracleMintingParams(..),
  OracleValidatorParams(..),
 ) where
import Data.Aeson qualified as JSON
import PlutusTx qualified
import PlutusTx.Prelude (
   BuiltinByteString,
   Integer,
   Eq(..),
   (&&),
 )
import Ledger (
   PubKey,
   PubKeyHash,
   POSIXTime,
   Address,
 )
import Ledger.Value (
   CurrencySymbol,
   AssetClass,
 )
import Prelude qualified as Haskell
import GHC.Generics (Generic)
import PlutusTx.UniqueMap qualified as UniqueMap


data OracleValidatorParams = OracleValidatorParams
  { oracleValidatorParams'oracleMintingCurrencySymbol :: !CurrencySymbol
  , oracleValidatorParams'operator :: !PubKey
  , oracleValidatorParams'operatorPkh :: !PubKeyHash
  , oracleValidatorParams'peggedCurrency :: !BuiltinByteString
  }
  deriving stock (Haskell.Eq, Haskell.Show, Generic)
  deriving anyclass (JSON.FromJSON, JSON.ToJSON)
PlutusTx.makeLift ''OracleValidatorParams

data PriceTracking = PriceTracking
  { priceTracking'fiatPriceFeed :: UniqueMap.Map BuiltinByteString Integer
  , priceTracking'cryptoPriceFeed :: UniqueMap.Map AssetClass Integer
  , priceTracking'lastUpdate :: POSIXTime
  }
  deriving stock (Haskell.Eq,Haskell.Show, Generic)
  deriving anyclass (JSON.FromJSON, JSON.ToJSON)
PlutusTx.makeIsDataIndexed ''PriceTracking [('PriceTracking, 0)]

instance Eq PriceTracking where
  {-# INLINEABLE (==) #-}
  (==) a b = priceTracking'fiatPriceFeed a == priceTracking'fiatPriceFeed b
          && priceTracking'cryptoPriceFeed a == priceTracking'cryptoPriceFeed b
          && priceTracking'lastUpdate a == priceTracking'lastUpdate b


data PriceTrackingCopy = PriceTrackingCopy
  {
    priceTrackingCopy'Copier :: Address
  , priceTrackingCopy'Expiry :: POSIXTime
  , priceTrackingCopy'Replications :: Integer
  }
  deriving stock (Haskell.Eq,Haskell.Show, Generic)
  deriving anyclass (JSON.FromJSON, JSON.ToJSON)
PlutusTx.makeIsDataIndexed ''PriceTrackingCopy [('PriceTrackingCopy, 0)]


instance Eq PriceTrackingCopy where
  {-# INLINEABLE (==) #-}
  (==) a b = priceTrackingCopy'Copier a == priceTrackingCopy'Copier b
          && priceTrackingCopy'Expiry a == priceTrackingCopy'Expiry b

data OracleMintingParams = OracleMintingParams
  { oracleMintingParams'operator :: !Ledger.PubKey
  , oracleMintingParams'operatorPkh :: !Ledger.PubKeyHash
  , oracleMintingParams'operatorChangeAddress :: !Address
  }
  deriving stock (Haskell.Eq, Haskell.Show, Generic)
  deriving anyclass (JSON.FromJSON, JSON.ToJSON)
PlutusTx.makeLift ''OracleMintingParams

data OracleMintingRedeemer = Initialise | Update PriceTracking POSIXTime Integer | CreateCopy Address | DestroyCopy
  deriving stock (Haskell.Eq, Haskell.Show, Generic)
  deriving anyclass (JSON.FromJSON, JSON.ToJSON)
PlutusTx.makeIsDataIndexed ''OracleMintingRedeemer [('Initialise, 0),('Update,1),('CreateCopy,2),('DestroyCopy,3)]


