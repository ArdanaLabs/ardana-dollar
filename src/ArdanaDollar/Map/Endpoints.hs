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
import ArdanaDollar.Map.ValidatorsTH (Integer2IntegerMap)

type Schema =
  Endpoint "insert" (Integer, Integer)
    .\/ Endpoint "remove" Integer

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
      ]