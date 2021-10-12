{-# LANGUAGE TypeFamilies #-}

module ArdanaDollar.Map.Endpoints (
  endpoints,
  createEndpoint,
  Schema,
  CreateSchema,
) where

import Plutus.Contract
import Plutus.V1.Ledger.Value qualified as Value
import PlutusTx.Prelude

import Control.Monad (forever)
import Data.Monoid (Last)
import Data.Text (Text)

import ArdanaDollar.Map.Contracts
import ArdanaDollar.Map.Types

type Schema =
  Endpoint "insert" (Key, Value)
    .\/ Endpoint "remove" Key

type CreateSchema =
  Endpoint "create" ()
    .\/ Endpoint "createTest" Value.AssetClass

createEndpoint :: Contract (Last MapInstance) CreateSchema Text ()
createEndpoint = do
  forever $
    selectList
      [ endpoint @"create" $ const create
      , endpoint @"createTest" createTest
      ]

endpoints :: MapInstance -> Contract (Last ()) Schema Text ()
endpoints f = do
  forever $
    selectList
      [ endpoint @"insert" (insert f)
      , endpoint @"remove" (remove f)
      ]
