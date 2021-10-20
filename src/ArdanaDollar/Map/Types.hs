{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GADTs #-}
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
) where

import PlutusTx qualified
import PlutusTx.Prelude

import Data.Aeson qualified as JSON
import GHC.Generics (Generic)
import Ledger qualified
import Prelude qualified as Haskell

newtype MapInstance = MapInstance {unMapInstance :: Ledger.AssetClass}
  deriving newtype (Haskell.Show, Haskell.Eq, Generic, JSON.FromJSON, JSON.ToJSON)

newtype Pointer = Pointer {unPointer :: Ledger.AssetClass}
  deriving newtype (Haskell.Show, Generic)

newtype PointerCS = PointerCS {unPointerCS :: Ledger.CurrencySymbol}

newtype Map = Map
  { map'head :: Maybe Pointer
  }

data Node k v = Node
  { node'key :: k
  , node'value :: v
  , node'next :: Maybe Pointer
  }

instance Eq Pointer where
  {-# INLINEABLE (==) #-}
  x == y = unPointer x == unPointer y

instance (Eq k, Eq v) => Eq (Node k v) where
  {-# INLINEABLE (==) #-}
  x == y =
    node'key x == node'key y
      && node'value x == node'value y
      && node'next x == node'next y

instance Eq Map where
  {-# INLINEABLE (==) #-}
  x == y = map'head x == map'head y

data Datum k v = MapDatum Map | NodeDatum (Node k v)
data Redeemer = Use | ListOp

data TokenRedeemer k
  = AddToEmptyMap k
  | AddSmallest k Ledger.TxOutRef
  | AddInTheMiddle k Ledger.TxOutRef Ledger.TxOutRef
  | AddGreatest k Ledger.TxOutRef
  | RemoveFromOneElementMap Ledger.TxOutRef
  | RemoveSmallest Ledger.TxOutRef Ledger.TxOutRef
  | RemoveInTheMiddle Ledger.TxOutRef Ledger.TxOutRef Ledger.TxOutRef
  | RemoveGreatest Ledger.TxOutRef Ledger.TxOutRef

PlutusTx.makeLift ''PointerCS
PlutusTx.makeLift ''MapInstance

PlutusTx.makeIsDataIndexed ''MapInstance [('MapInstance, 0)]
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
