{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-specialise #-}

module ArdanaDollar.PriceOracle.Types (
  OracleMintingRedeemer (..),
  PriceTracking (..),
  PriceTrackingCertification (..),
  OracleMintingParams (..),
) where

import Data.Aeson qualified as JSON
import GHC.Generics (Generic)
import Ledger (
  Address,
  POSIXTime,
  TxOutRef,
  ValidatorHash,
 )
import Ledger.Value (
  AssetClass,
 )
import PlutusTx qualified
import PlutusTx.Prelude (
  BuiltinByteString,
  Eq (..),
  Integer,
  (&&),
 )
import PlutusTx.UniqueMap qualified as UniqueMap
import Prelude qualified as Haskell

data PriceTracking = PriceTracking
  { fiatPriceFeed :: UniqueMap.Map BuiltinByteString Integer
  , cryptoPriceFeed :: UniqueMap.Map AssetClass Integer
  , lastUpdate :: POSIXTime
  , controllingScript :: ValidatorHash
  , requiredCertificationReplications :: Integer
  , certificationExpiry :: POSIXTime
  , narrowIntervalWidth :: Integer
  }
  deriving stock (Haskell.Eq, Haskell.Show, Generic)
  deriving anyclass (JSON.FromJSON, JSON.ToJSON)
PlutusTx.makeIsDataIndexed ''PriceTracking [('PriceTracking, 0)]

instance Eq PriceTracking where
  {-# INLINEABLE (==) #-}
  (==) a b =
    fiatPriceFeed a == fiatPriceFeed b
      && cryptoPriceFeed a == cryptoPriceFeed b
      && lastUpdate a == lastUpdate b
      && controllingScript a == controllingScript b
      && requiredCertificationReplications a == requiredCertificationReplications b
      && certificationExpiry a == certificationExpiry b
      && narrowIntervalWidth a == narrowIntervalWidth b

data PriceTrackingCertification = PriceTrackingCertification
  { priceTrackingCertification'copier :: Address
  , priceTrackingCertification'expiry :: POSIXTime
  , priceTrackingCertification'replications :: Integer
  , priceTrackingCertification'narrowIntervalWidth :: Integer
  }
  deriving stock (Haskell.Eq, Haskell.Show, Generic)
  deriving anyclass (JSON.FromJSON, JSON.ToJSON)
PlutusTx.makeIsDataIndexed ''PriceTrackingCertification [('PriceTrackingCertification, 0)]

instance Eq PriceTrackingCertification where
  {-# INLINEABLE (==) #-}
  (==) a b =
    priceTrackingCertification'copier a == priceTrackingCertification'copier b
      && priceTrackingCertification'expiry a == priceTrackingCertification'expiry b
      && priceTrackingCertification'replications a == priceTrackingCertification'replications b
      && priceTrackingCertification'narrowIntervalWidth a == priceTrackingCertification'narrowIntervalWidth b

data OracleMintingParams = OracleMintingParams
  { stateTokenTxOutRef :: !TxOutRef
  , initialControllingValidator :: !ValidatorHash
  }
  deriving stock (Haskell.Eq, Haskell.Show, Generic)
  deriving anyclass (JSON.FromJSON, JSON.ToJSON)
PlutusTx.makeLift ''OracleMintingParams

data OracleMintingRedeemer = Initialise | Update | CopyCertification Address | DestroyCertification
  deriving stock (Haskell.Eq, Haskell.Show, Generic)
  deriving anyclass (JSON.FromJSON, JSON.ToJSON)
PlutusTx.makeIsDataIndexed ''OracleMintingRedeemer [('Initialise, 0), ('Update, 1), ('CopyCertification, 2), ('DestroyCertification, 3)]
