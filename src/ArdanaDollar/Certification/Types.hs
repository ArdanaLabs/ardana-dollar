{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-specialise #-}

module ArdanaDollar.Certification.Types (
  ProliferationParameters (..),
  CertificationMintingRedeemer (..),
  Certification (..),
) where

import Data.Aeson qualified as JSON
import GHC.Generics (Generic)
import Ledger (
  Address,
  DatumHash,
  POSIXTime,
 )
import PlutusTx qualified
import PlutusTx.Prelude (
  Eq (..),
  Integer,
  (&&),
 )
import Prelude qualified as Haskell

data ProliferationParameters = ProliferationParameters
  { adaReturnAddress' :: Address
  , minReplications' :: Integer
  , certificationExpiry' :: POSIXTime
  , narrowIntervalWidth' :: Integer
  }
  deriving stock (Haskell.Eq, Haskell.Show, Generic)
  deriving anyclass (JSON.FromJSON, JSON.ToJSON)
PlutusTx.makeIsDataIndexed ''ProliferationParameters [('ProliferationParameters, 0)]

instance Eq ProliferationParameters where
  {-# INLINEABLE (==) #-}
  (==) a b =
    adaReturnAddress' a == adaReturnAddress' b
      && certificationExpiry' a == certificationExpiry' b
      && minReplications' a == minReplications' b
      && narrowIntervalWidth' a == narrowIntervalWidth' b

data Certification = Certification
  { minReplications :: Integer
  , certificationExpiry :: POSIXTime
  , narrowIntervalWidth :: Integer
  , certifiedDatumHash :: DatumHash
  }
  deriving stock (Haskell.Eq, Haskell.Show, Generic)
  deriving anyclass (JSON.FromJSON, JSON.ToJSON)
PlutusTx.makeIsDataIndexed ''Certification [('Certification, 0)]

data CertificationMintingRedeemer
  = Initialise
  | Certify Address Certification
  | CopyCertificationToken Address DatumHash
  | DestroyCertificationToken DatumHash
  deriving stock (Haskell.Eq, Haskell.Show, Generic)
  deriving anyclass (JSON.FromJSON, JSON.ToJSON)
PlutusTx.makeIsDataIndexed ''CertificationMintingRedeemer [('Initialise, 0), ('Certify, 1), ('CopyCertificationToken, 2), ('DestroyCertificationToken, 3)]
