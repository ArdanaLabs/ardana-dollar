{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-specialise #-}

module ArdanaDollar.Certification.Types (
  CertificationMintingRedeemer (..),
  CertificationCopyingParameters (..),
  CertifiedDatum (..),
  CertificationMintingParams (..),
) where

import Data.Aeson qualified as JSON
import GHC.Generics (Generic)
import Ledger (
  Address,
  Datum,
  POSIXTime,
  TxOutRef,
  ValidatorHash,
 )
import PlutusTx qualified
import PlutusTx.Prelude (
  Eq (..),
  Integer,
  (&&),
 )
import Prelude qualified as Haskell

data CertifiedDatum = CertifiedDatum
  { unCertificateCopyingParameters :: Datum
  , lastUpdate :: POSIXTime
  , certificationAuthority :: ValidatorHash
  , requiredCertificationReplications :: Integer
  , certificationExpiry :: POSIXTime
  , narrowIntervalWidth :: Integer
  }
  deriving stock (Haskell.Eq, Haskell.Show, Generic)
  deriving anyclass (JSON.FromJSON, JSON.ToJSON)
PlutusTx.makeIsDataIndexed ''CertifiedDatum [('CertifiedDatum, 0)]

instance Eq CertifiedDatum where
  {-# INLINEABLE (==) #-}
  (==) a b =
    unCertificateCopyingParameters a == unCertificateCopyingParameters b
      && lastUpdate a == lastUpdate b
      && certificationAuthority a == certificationAuthority b
      && requiredCertificationReplications a == requiredCertificationReplications b
      && certificationExpiry a == certificationExpiry b
      && narrowIntervalWidth a == narrowIntervalWidth b

data CertificationCopyingParameters = CertificationCopyingParameters
  { copier :: Address
  , expiry :: POSIXTime
  , replications :: Integer
  , narrowIntervalWidth' :: Integer
  }
  deriving stock (Haskell.Eq, Haskell.Show, Generic)
  deriving anyclass (JSON.FromJSON, JSON.ToJSON)
PlutusTx.makeIsDataIndexed ''CertificationCopyingParameters [('CertificationCopyingParameters, 0)]

instance Eq CertificationCopyingParameters where
  {-# INLINEABLE (==) #-}
  (==) a b =
    copier a == copier b
      && expiry a == expiry b
      && replications a == replications b
      && narrowIntervalWidth' a == narrowIntervalWidth' b

data CertificationMintingParams = CertificationMintingParams
  { stateTokenTxOutRef :: !TxOutRef
  , initialControllingValidator :: !ValidatorHash
  }
  deriving stock (Haskell.Eq, Haskell.Show, Generic)
  deriving anyclass (JSON.FromJSON, JSON.ToJSON)
PlutusTx.makeLift ''CertificationMintingParams

data CertificationMintingRedeemer = Initialise | Update | CopyCertificationToken Address | DestroyCertificationToken
  deriving stock (Haskell.Eq, Haskell.Show, Generic)
  deriving anyclass (JSON.FromJSON, JSON.ToJSON)
PlutusTx.makeIsDataIndexed ''CertificationMintingRedeemer [('Initialise, 0), ('Update, 1), ('CopyCertificationToken, 2), ('DestroyCertificationToken, 3)]
