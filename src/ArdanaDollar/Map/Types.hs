{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module ArdanaDollar.Map.Types (
  MapInstance (..),
  Pointer (..),
  PointerCS (..),
  Map (..),
  Node (..),
  Datum (..),
  Redeemer (..),
  TokenRedeemer (..),
  Key,
  Value,
) where

import PlutusTx qualified
import PlutusTx.Prelude

import Data.Aeson qualified as JSON
import GHC.Generics (Generic)
import Ledger qualified
import Prelude qualified as Haskell

type Key = Integer
type Value = Integer

newtype MapInstance = MapInstance {unMapInstance :: Ledger.AssetClass}
  deriving stock (Haskell.Show, Generic)
  deriving anyclass (JSON.FromJSON, JSON.ToJSON)

newtype Pointer = Pointer {unPointer :: Ledger.AssetClass}
  deriving stock (Haskell.Show, Generic)
  deriving anyclass (JSON.FromJSON, JSON.ToJSON)

newtype PointerCS = PointerCS {unPointerCS :: Ledger.CurrencySymbol}

newtype Map = Map
  { map'head :: Maybe Pointer
  }

data Node = Node
  { node'key :: Key
  , node'value :: Value
  , node'next :: Maybe Pointer
  }

instance Eq Pointer where
  {-# INLINEABLE (==) #-}
  x == y = unPointer x == unPointer y

instance Eq Node where
  {-# INLINEABLE (==) #-}
  x == y =
    node'key x == node'key y
      && node'value x == node'value y
      && node'next x == node'next y

instance Eq Map where
  {-# INLINEABLE (==) #-}
  x == y = map'head x == map'head y

data Datum = MapDatum Map | NodeDatum Node
data Redeemer = Use | ListOp

data TokenRedeemer
  = AddToEmptyMap Key
  | AddSmallest Key Ledger.TxOutRef
  | AddInTheMiddle Key Ledger.TxOutRef Ledger.TxOutRef
  | AddGreatest Key Ledger.TxOutRef
  | RemoveFromOneElementMap Ledger.TxOutRef
  | RemoveSmallest Ledger.TxOutRef Ledger.TxOutRef
  | RemoveInTheMiddle Ledger.TxOutRef Ledger.TxOutRef Ledger.TxOutRef
  | RemoveGreatest Ledger.TxOutRef Ledger.TxOutRef

PlutusTx.makeLift ''PointerCS
PlutusTx.makeLift ''MapInstance

PlutusTx.makeIsDataIndexed ''Pointer [('Pointer, 0)]
PlutusTx.makeIsDataIndexed ''Map [('Map, 0)]
PlutusTx.makeIsDataIndexed ''Node [('Node, 0)]
PlutusTx.makeIsDataIndexed
  ''Datum
  [ ('MapDatum, 0)
  , ('NodeDatum, 1)
  ]
PlutusTx.makeIsDataIndexed
  ''Redeemer
  [ ('Use, 0)
  , ('ListOp, 1)
  ]

PlutusTx.makeIsDataIndexed
  ''TokenRedeemer
  [ ('AddToEmptyMap, 0)
  , ('AddSmallest, 1)
  , ('AddInTheMiddle, 2)
  , ('AddGreatest, 3)
  , ('RemoveFromOneElementMap, 4)
  , ('RemoveSmallest, 5)
  , ('RemoveInTheMiddle, 6)
  , ('RemoveGreatest, 7)
  ]
