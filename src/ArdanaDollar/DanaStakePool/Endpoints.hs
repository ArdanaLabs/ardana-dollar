{-# LANGUAGE TypeFamilies #-}

module ArdanaDollar.DanaStakePool.Endpoints (
  endpoints,
  initializeSystemEndpoint,
  queryEndpoints,
) where

import PlutusTx.Prelude

import Ledger (PubKeyHash)
import Ledger.Value qualified as Value
import Plutus.Contract

import Control.Monad (forever)
import Data.Monoid (Last)
import Data.Text (Text)

import ArdanaDollar.DanaStakePool.Contracts
import ArdanaDollar.DanaStakePool.Validators

type Schema =
  Endpoint "initializeUser" ()
    .\/ Endpoint "deposit" Integer
    .\/ Endpoint "withdraw" Integer
    .\/ Endpoint "provideRewards" Integer
    .\/ Endpoint "distributeRewards" ()
    .\/ Endpoint "withdrawRewards" ()

type QuerySchema =
  Endpoint "queryUser" Ledger.PubKeyHash
    .\/ Endpoint "querySelf" ()

type SystemInitializationSchema =
  Endpoint "initializeSystem" ()

initializeSystemEndpoint :: Contract (Last Value.AssetClass) SystemInitializationSchema Text ()
initializeSystemEndpoint =
  forever $ awaitPromise (endpoint @"initializeSystem" $ const initializeSystem)

endpoints :: Value.CurrencySymbol -> Contract (Last Datum) Schema Text ()
endpoints f = do
  forever $
    selectList
      [ endpoint @"initializeUser" (const $ initializeUser f)
      , endpoint @"deposit" $ deposit f
      , endpoint @"withdraw" $ withdraw f
      , endpoint @"provideRewards" $ provideRewards f
      , endpoint @"distributeRewards" $ distributeRewards f
      , endpoint @"withdrawRewards" $ withdrawRewards f
      ]

queryEndpoints :: Value.CurrencySymbol -> Contract (Last UserData) QuerySchema Text ()
queryEndpoints f = do
  forever $
    selectList
      [ endpoint @"queryUser" $ queryUser f
      , endpoint @"querySelf" (const $ querySelf f)
      ]
