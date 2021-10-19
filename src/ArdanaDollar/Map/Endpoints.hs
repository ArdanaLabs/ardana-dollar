{-# LANGUAGE TypeFamilies #-}

module ArdanaDollar.Map.Endpoints (
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

import ArdanaDollar.Map.Contracts
import ArdanaDollar.Map.Types
import ArdanaDollar.Map.ValidatorsTH

type Key = Integer
type Value = Integer

type Schema =
  Endpoint "insert" (Key, Value)
    .\/ Endpoint "remove" Key
    .\/ Endpoint "increment" Key

type CreateSchema =
  Endpoint "create" ()
    .\/ Endpoint "createTest" Value.AssetClass

createEndpoint :: Contract (Last MapInstance) CreateSchema Text ()
createEndpoint = do
  forever $
    selectList
      [ endpoint @"create" $ const (create @Integer2IntegerMap)
      , endpoint @"createTest" (createTest @Integer2IntegerMap)
      ]

endpoints :: MapInstance -> Contract (Last ()) Schema Text ()
endpoints f = do
  forever $
    selectList
      [ endpoint @"insert" (insert @Integer2IntegerMap f)
      , endpoint @"remove" (remove @Integer2IntegerMap f)
      , endpoint @"increment" (\key -> use @Integer2IntegerMap f key (+ 1))
      ]
