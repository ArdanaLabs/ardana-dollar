{-# LANGUAGE TypeFamilies #-}

module ArdanaDollar.Map.Endpoints (
  endpoints,
  createEndpoint,
  Schema,
  CreateSchema,
) where

import Plutus.Contract
import PlutusTx.Prelude

import Control.Monad (forever)
import Data.Monoid (Last)
import Data.Text (Text)

import ArdanaDollar.Map.Contracts
import ArdanaDollar.Map.Types

type Schema =
  Endpoint "insert" (Key, Value)

type CreateSchema =
  Endpoint "create" ()

createEndpoint :: Contract (Last MapInstance) CreateSchema Text ()
createEndpoint = do
  forever $ awaitPromise (endpoint @"create" $ const create)

endpoints :: MapInstance -> Contract (Last ()) Schema Text ()
endpoints f = do
  forever $
    selectList
      [ endpoint @"insert" (insert f)
      ]
