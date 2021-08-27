{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-specialize #-}

module ArdanaDollar.DanaStakePool.Types (
  NFTAssetClass (..),
  DanaAssetClass (..),
  UserInitProofAssetClass (..),
  Balance (..),
  Datum (..),
  TraversalState (..),
  Redeemer (..),
  GlobalData (..),
  UserData (..),
) where

import PlutusTx.Prelude

import Ledger qualified
import Ledger.Value qualified as Value
import PlutusTx qualified
import PlutusTx.Monoid qualified
import PlutusTx.Semigroup qualified

import Prelude qualified as Haskell

import GHC.Generics (Generic)

import Data.Aeson qualified as JSON

newtype NFTAssetClass = NFTAssetClass
  {unNFTAssetClass :: Value.AssetClass}
  deriving stock (Haskell.Show, Generic, Haskell.Eq)
  deriving anyclass (JSON.FromJSON, JSON.ToJSON)

newtype DanaAssetClass = DanaAssetClass
  {unDanaAssetClass :: Value.AssetClass}

newtype UserInitProofAssetClass = UserInitProofAssetClass
  {unUserInitProofAssetClass :: Value.AssetClass}

data Balance = Balance
  { balance'stake :: Value.Value
  , balance'reward :: Value.Value
  }
  deriving stock (Haskell.Show, Generic, Haskell.Eq)
  deriving anyclass (JSON.FromJSON, JSON.ToJSON)

data UserData = UserData
  { userData'pkh :: Ledger.PubKeyHash
  , userData'balance :: Balance
  , userData'id :: Integer
  }
  deriving stock (Haskell.Show, Generic)
  deriving anyclass (JSON.FromJSON, JSON.ToJSON)

data TraversalState = TraversalInactive | TraversalActive Value.Value Integer
  deriving stock (Haskell.Show, Generic, Haskell.Eq)
  deriving anyclass (JSON.FromJSON, JSON.ToJSON)

data GlobalData = GlobalData
  { globalData'totalStake :: Value.Value
  , globalData'count :: Integer
  , globalData'locked :: Bool
  , globaldata'traversal :: TraversalState
  }
  deriving stock (Haskell.Show, Generic, Haskell.Eq)
  deriving anyclass (JSON.FromJSON, JSON.ToJSON)

data Datum = UserDatum UserData | GlobalDatum GlobalData
  deriving stock (Haskell.Show, Generic)
  deriving anyclass (JSON.FromJSON, JSON.ToJSON)

data Redeemer
  = DepositOrWithdraw
  | ProvideRewards
  | DistributeRewards
  | WithdrawRewards
  | InitializeUser
  deriving stock (Haskell.Show, Generic)
  deriving anyclass (JSON.FromJSON, JSON.ToJSON)

instance PlutusTx.Semigroup.Semigroup Balance where
  (<>) x y = Balance (balance'stake x + balance'stake y) (balance'reward x + balance'reward y)

instance Haskell.Semigroup Balance where
  (<>) x y = x PlutusTx.Semigroup.<> y

instance PlutusTx.Monoid.Monoid Balance where
  mempty = Balance mempty mempty

instance Haskell.Monoid Balance where
  mempty = PlutusTx.Monoid.mempty

PlutusTx.makeLift ''DanaAssetClass
PlutusTx.makeLift ''NFTAssetClass
PlutusTx.makeLift ''UserInitProofAssetClass

PlutusTx.makeIsDataIndexed ''Balance [('Balance, 0)]
PlutusTx.makeIsDataIndexed ''UserData [('UserData, 0)]
PlutusTx.makeIsDataIndexed ''TraversalState [('TraversalInactive, 0), ('TraversalActive, 1)]

PlutusTx.makeIsDataIndexed ''GlobalData [('GlobalData, 0)]
PlutusTx.makeIsDataIndexed
  ''Datum
  [ ('UserDatum, 0)
  , ('GlobalDatum, 1)
  ]

PlutusTx.makeIsDataIndexed
  ''Redeemer
  [ ('DepositOrWithdraw, 0)
  , ('ProvideRewards, 1)
  , ('DistributeRewards, 2)
  , ('WithdrawRewards, 3)
  , ('InitializeUser, 4)
  ]
