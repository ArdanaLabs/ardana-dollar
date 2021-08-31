{-# LANGUAGE TypeFamilies #-}

module ArdanaDollar.DanaStakePool.Endpoints (
  endpoints,
  initializeSystemEndpoint,
  queryEndpoints,
) where

import PlutusTx.Prelude

import Ledger (PubKeyHash)
import Plutus.Contract

import Control.Monad (forever)
import Data.Monoid (Last)
import Data.Text (Text)

import ArdanaDollar.DanaStakePool.Contracts
import ArdanaDollar.DanaStakePool.Types

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

initializeSystemEndpoint :: Contract (Last NFTAssetClass) SystemInitializationSchema Text ()
initializeSystemEndpoint = do
  forever
    (endpoint @"initializeSystem" >> initializeSystem)

endpoints :: NFTAssetClass -> Contract (Last Datum) Schema Text ()
endpoints f = do
  forever $
    (endpoint @"initializeUser" >> initializeUser f)
      `select` (endpoint @"deposit" >>= deposit f)
      `select` (endpoint @"withdraw" >>= withdraw f)
      `select` (endpoint @"provideRewards" >>= provideRewards f)
      `select` (endpoint @"distributeRewards" >>= distributeRewards f)
      `select` (endpoint @"withdrawRewards" >>= withdrawRewards f)

queryEndpoints :: NFTAssetClass -> Contract (Last [UserData]) QuerySchema Text ()
queryEndpoints f = do
  forever $
    (endpoint @"queryUser" >>= queryUser f)
      `select` (endpoint @"querySelf" >> querySelf f)
