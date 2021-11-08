{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module ArdanaDollar.Map.Types (
  MapInstance (..),
  Pointer (..),
  PointerCS (..),
  SnapshotCS (..),
  Map (..),
  Node (..),
  Datum (..),
  Redeemer (..),
  NodeValidTokenRedeemer (..),
  SnapshotTokenRedeemer (..),
  MapSnapshot (..),
  NodeSnapshot (..),
  SnapshotVersion (..),
  SnapshotPerm (..),
  SnapshotPointer (..),
  LockState (..),
  UnlockCS (..),
  UnlockTokenRedeemer (..),
) where

import PlutusTx qualified
import PlutusTx.Prelude

import Data.Aeson qualified as JSON
import GHC.Generics (Generic)
import Ledger qualified
import Ledger.Value (Value)
import Prelude qualified as Haskell

newtype MapInstance = MapInstance {unMapInstance :: Ledger.AssetClass}
  deriving newtype (Haskell.Show, Haskell.Eq, Generic, JSON.FromJSON, JSON.ToJSON)

newtype Pointer = Pointer {unPointer :: Ledger.AssetClass}
  deriving newtype (Haskell.Show, Haskell.Eq, Generic)

newtype SnapshotPointer = SnapshotPointer {unSnapshotPointer :: Ledger.AssetClass}
  deriving newtype (Haskell.Show, Haskell.Eq, Generic)

newtype PointerCS = PointerCS {unPointerCS :: Ledger.CurrencySymbol}

newtype SnapshotCS = SnapshotCS {unSnapshotCS :: Ledger.CurrencySymbol}

newtype UnlockCS = UnlockCS {unUnlockCS :: Ledger.CurrencySymbol}

newtype SnapshotVersion = SnapshotVersion
  {unSnapshotVersion :: Integer}
  deriving newtype (Haskell.Eq, Haskell.Show)

data LockState = Unlocked | LockedFor !SnapshotVersion
  deriving stock (Haskell.Eq, Haskell.Show, Generic)

data Map = Map
  { map'head :: !(Maybe Pointer)
  , map'lockState :: !LockState
  , map'nextVersion :: !SnapshotVersion
  }
  deriving stock (Haskell.Eq, Haskell.Show, Generic)

data Node k v = Node
  { node'key :: !k
  , node'value :: !v
  , node'next :: !(Maybe Pointer)
  , node'lockState :: !LockState
  }
  deriving stock (Haskell.Eq, Haskell.Show, Generic)

data MapSnapshot = MapSnapshot
  { mapSnapshot'assets :: Value
  , mapSnapshot'head :: Maybe SnapshotPointer
  , mapSnapshot'version :: SnapshotVersion
  }
  deriving stock (Haskell.Eq, Haskell.Show, Generic)

data NodeSnapshot k v = NodeSnapshot
  { nodeSnapshot'assets :: Value
  , nodeSnapshot'key :: k
  , nodeSnapshot'value :: v
  , nodeSnapshot'next :: Maybe SnapshotPointer
  , nodeSnapshot'version :: SnapshotVersion
  }
  deriving stock (Haskell.Eq, Haskell.Show, Generic)

data SnapshotPerm = SnapshotPerm
  { snapshotPerm'start :: Pointer
  , snapshotPerm'end :: Pointer
  , snapshotPerm'version :: SnapshotVersion
  }
  deriving stock (Haskell.Eq, Haskell.Show, Generic)

data Datum k v
  = MapDatum !Map
  | NodeDatum !(Node k v)
  | MapSnapshotDatum !MapSnapshot
  | NodeSnapshotDatum !(NodeSnapshot k v)
  | SnapshotPermDatum !SnapshotPerm
  deriving stock (Haskell.Eq, Haskell.Show, Generic)

data Redeemer
  = Use
  | ListOp
  | SnapshotOp
  | UnlockOp
  deriving stock (Haskell.Show, Haskell.Eq, Generic)

data NodeValidTokenRedeemer
  = AddToEmptyMap
  | AddSmallest
  | AddInTheMiddle !Ledger.TxOutRef
  | AddGreatest !Ledger.TxOutRef
  | RemoveFromOneElementMap
  | RemoveSmallest
  | RemoveInTheMiddle !Ledger.TxOutRef
  | RemoveGreatest !Ledger.TxOutRef
  deriving stock (Haskell.Eq, Haskell.Show, Generic)

data SnapshotTokenRedeemer
  = InitiateSnapshot !Ledger.TxOutRef
  | SplitSnapshot !Ledger.TxOutRef !Ledger.TxOutRef !Ledger.TxOutRef
  | MakeSnapshot !Ledger.TxOutRef !Ledger.TxOutRef
  deriving stock (Haskell.Eq, Haskell.Show, Generic)

data UnlockTokenRedeemer
  = InitiateUnlock
  | MergeUnlock
  | MakeUnlock

-- instances

instance Eq Pointer where
  {-# INLINEABLE (==) #-}
  x == y = unPointer x == unPointer y

instance Eq SnapshotPointer where
  {-# INLINEABLE (==) #-}
  x == y = unSnapshotPointer x == unSnapshotPointer y

instance Eq SnapshotVersion where
  {-# INLINEABLE (==) #-}
  x == y = unSnapshotVersion x == unSnapshotVersion y

instance Eq LockState where
  {-# INLINEABLE (==) #-}
  x == y = case (x, y) of
    (Unlocked, Unlocked) -> True
    (LockedFor v1, LockedFor v2) -> v1 == v2
    (_, _) -> False

instance Eq Map where
  {-# INLINEABLE (==) #-}
  x == y =
    map'head x == map'head y
      && map'lockState x == map'lockState y
      && map'nextVersion x == map'nextVersion y

instance (Eq k, Eq v) => Eq (Node k v) where
  {-# INLINEABLE (==) #-}
  x == y =
    node'key x == node'key y
      && node'value x == node'value y
      && node'next x == node'next y
      && node'lockState x == node'lockState y

instance Eq MapSnapshot where
  {-# INLINEABLE (==) #-}
  x == y =
    mapSnapshot'assets x == mapSnapshot'assets y
      && mapSnapshot'head x == mapSnapshot'head y
      && mapSnapshot'version x == mapSnapshot'version y

instance (Eq k, Eq v) => Eq (NodeSnapshot k v) where
  {-# INLINEABLE (==) #-}
  x == y =
    nodeSnapshot'assets x == nodeSnapshot'assets y
      && nodeSnapshot'key x == nodeSnapshot'key y
      && nodeSnapshot'value x == nodeSnapshot'value y
      && nodeSnapshot'next x == nodeSnapshot'next y
      && nodeSnapshot'version x == nodeSnapshot'version y

instance Eq SnapshotPerm where
  {-# INLINEABLE (==) #-}
  x == y =
    snapshotPerm'start x == snapshotPerm'start y
      && snapshotPerm'end x == snapshotPerm'end y
      && snapshotPerm'version x == snapshotPerm'version y

PlutusTx.makeLift ''PointerCS
PlutusTx.makeLift ''SnapshotCS
PlutusTx.makeLift ''UnlockCS
PlutusTx.makeLift ''MapInstance

PlutusTx.makeIsDataIndexed ''MapInstance [('MapInstance, 0)]
PlutusTx.makeIsDataIndexed ''Pointer [('Pointer, 0)]
PlutusTx.makeIsDataIndexed ''SnapshotPointer [('SnapshotPointer, 0)]
PlutusTx.makeIsDataIndexed ''SnapshotVersion [('SnapshotVersion, 0)]
PlutusTx.makeIsDataIndexed
  ''LockState
  [ ('Unlocked, 0)
  , ('LockedFor, 1)
  ]

PlutusTx.makeIsDataIndexed ''Map [('Map, 0)]
PlutusTx.makeIsDataIndexed ''Node [('Node, 0)]
PlutusTx.makeIsDataIndexed ''MapSnapshot [('MapSnapshot, 0)]
PlutusTx.makeIsDataIndexed ''NodeSnapshot [('NodeSnapshot, 0)]
PlutusTx.makeIsDataIndexed ''SnapshotPerm [('SnapshotPerm, 0)]

PlutusTx.makeIsDataIndexed
  ''Datum
  [ ('MapDatum, 0)
  , ('NodeDatum, 1)
  , ('MapSnapshotDatum, 2)
  , ('NodeSnapshotDatum, 3)
  , ('SnapshotPermDatum, 4)
  ]
PlutusTx.makeIsDataIndexed
  ''Redeemer
  [ ('Use, 0)
  , ('ListOp, 1)
  , ('SnapshotOp, 2)
  , ('UnlockOp, 3)
  ]

PlutusTx.makeIsDataIndexed
  ''NodeValidTokenRedeemer
  [ ('AddToEmptyMap, 0)
  , ('AddSmallest, 1)
  , ('AddInTheMiddle, 2)
  , ('AddGreatest, 3)
  , ('RemoveFromOneElementMap, 4)
  , ('RemoveSmallest, 5)
  , ('RemoveInTheMiddle, 6)
  , ('RemoveGreatest, 7)
  ]

PlutusTx.makeIsDataIndexed
  ''SnapshotTokenRedeemer
  [ ('InitiateSnapshot, 0)
  , ('SplitSnapshot, 1)
  , ('MakeSnapshot, 2)
  ]

PlutusTx.makeIsDataIndexed
  ''UnlockTokenRedeemer
  [ ('InitiateUnlock, 0)
  , ('MergeUnlock, 1)
  , ('MakeUnlock, 2)
  ]
