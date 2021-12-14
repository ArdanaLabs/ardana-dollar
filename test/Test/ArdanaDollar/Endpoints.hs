{-# LANGUAGE TypeFamilies #-}

module Test.ArdanaDollar.Endpoints (
  endpoints,
  createEndpoint,
  Schema,
  CreateSchema,
) where

import Plutus.Contract (
  Contract,
  Endpoint,
  endpoint,
  selectList,
  type (.\/),
 )
import Plutus.V1.Ledger.Value qualified as Value
import PlutusTx.Prelude (
  AdditiveSemigroup ((+)),
  Integer,
  const,
  ($),
 )

import Control.Monad (forever)
import Data.Monoid (Last)
import Data.Text (Text)

import ArdanaDollar.Map.ListOpContracts qualified as Contracts
import ArdanaDollar.Map.SnapshotOpContracts qualified as Contracts
import ArdanaDollar.Map.Types (MapInstance)
import ArdanaDollar.Map.UseContracts qualified as Contracts
import Test.ArdanaDollar.ValidatorsTH (Integer2IntegerMap)

type Key = Integer
type Value = Integer

type Schema =
  Endpoint "insert" (Key, Value)
    .\/ Endpoint "remove" Key
    .\/ Endpoint "increment" Key
    .\/ Endpoint "createSnapshot" ()

type CreateSchema =
  Endpoint "create" ()
    .\/ Endpoint "createTest" Value.AssetClass

createEndpoint :: Contract (Last MapInstance) CreateSchema Text ()
createEndpoint = do
  forever $
    selectList
      [ endpoint @"create" $ const (Contracts.create @Integer2IntegerMap)
      , endpoint @"createTest" (Contracts.createTest @Integer2IntegerMap)
      ]

endpoints :: MapInstance -> Contract (Last ()) Schema Text ()
endpoints f = do
  forever $
    selectList
      [ endpoint @"insert" (Contracts.insert @Integer2IntegerMap f)
      , endpoint @"remove" (Contracts.remove @Integer2IntegerMap f)
      , endpoint @"increment" (\key -> Contracts.use @Integer2IntegerMap f key (+ 1))
      , endpoint @"createSnapshot" (const $ Contracts.createSnapshot @Integer2IntegerMap f)
      ]
