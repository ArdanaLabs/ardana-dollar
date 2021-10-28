{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-specialise #-}

module ArdanaDollar.ExponentialMint.Types (
  ProliferationParameters (..),
  ExponentialMintingRedeemer (..),
  ExponentialMint (..),
) where

import Data.Aeson qualified as JSON
import GHC.Generics (Generic)
import Ledger (
  Address,
  POSIXTime,
 )
import Ledger.Value (
  TokenName,
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
  , canBeDestroyedAtTime' :: POSIXTime
  , narrowIntervalWidth' :: Integer
  }
  deriving stock (Haskell.Eq, Haskell.Show, Generic)
  deriving anyclass (JSON.FromJSON, JSON.ToJSON)
PlutusTx.makeIsDataIndexed ''ProliferationParameters [('ProliferationParameters, 0)]

instance Eq ProliferationParameters where
  {-# INLINEABLE (==) #-}
  (==) a b =
    adaReturnAddress' a == adaReturnAddress' b
      && canBeDestroyedAtTime' a == canBeDestroyedAtTime' b
      && minReplications' a == minReplications' b
      && narrowIntervalWidth' a == narrowIntervalWidth' b

data ExponentialMint = ExponentialMint
  { minReplications :: Integer
  , canBeDestroyedAtTime :: POSIXTime
  , narrowIntervalWidth :: Integer
  , seedTokenName :: TokenName
  }
  deriving stock (Haskell.Eq, Haskell.Show, Generic)
  deriving anyclass (JSON.FromJSON, JSON.ToJSON)
PlutusTx.makeIsDataIndexed ''ExponentialMint [('ExponentialMint, 0)]

data ExponentialMintingRedeemer
  = Initialise
  | SeedExponentiallyMintableToken Address ExponentialMint
  | CopyExponentiallyMintableToken Address TokenName
  | DestroyExponentiallyMintableToken TokenName
  deriving stock (Haskell.Eq, Haskell.Show, Generic)
  deriving anyclass (JSON.FromJSON, JSON.ToJSON)
PlutusTx.makeIsDataIndexed ''ExponentialMintingRedeemer [('Initialise, 0), ('SeedExponentiallyMintableToken, 1), ('CopyExponentiallyMintableToken, 2), ('DestroyExponentiallyMintableToken, 3)]
