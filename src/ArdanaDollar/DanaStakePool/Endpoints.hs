{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module ArdanaDollar.DanaStakePool.Endpoints (
  endpoints,
  initializeSystemEndpoint,
) where

import PlutusTx.Prelude

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

type Schema2 =
  Endpoint "initializeSystem" ()

initializeSystemEndpoint :: Contract (Last Value.AssetClass) Schema2 Text ()
initializeSystemEndpoint = do
  forever
    (endpoint @"initializeSystem" >>= initializeSystem)

endpoints :: Value.CurrencySymbol -> Contract (Last Datum) Schema Text ()
endpoints f = do
  forever $
    (endpoint @"initializeUser" >>= initializeUser f)
      `select` (endpoint @"deposit" >>= deposit f)
      `select` (endpoint @"withdraw" >>= withdraw f)
      `select` (endpoint @"provideRewards" >>= provideRewards f)
      `select` (endpoint @"distributeRewards" >>= distributeRewards f)
      `select` (endpoint @"withdrawRewards" >>= withdrawRewards f)