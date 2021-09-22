module Test.ArdanaDollar.MapTest (
  testAddToEmptyMap,
  testAddSmallest,
  mapTests,
  runTrace,
  createMap,
) where

import Prelude

import Ledger.Ada as Ada
import Ledger.Value qualified as Value
import Plutus.Contract.Test
import Plutus.Trace.Emulator as Emulator

import Data.Default (Default (..))
import Data.Map qualified as Map
import Data.Monoid

import Control.Lens
import Control.Monad (void)

import Test.Tasty

import ArdanaDollar.Map.Endpoints
import ArdanaDollar.Map.Types

v :: Value.Value
v = Ada.lovelaceValueOf 100_000_000

emCfg :: EmulatorConfig
emCfg = EmulatorConfig (Left $ Map.fromList [(Wallet 1, v)]) def def

runTrace :: EmulatorTrace () -> IO ()
runTrace = runEmulatorTraceIO' def emCfg

createMap :: EmulatorTrace MapInstance
createMap = do
  h <- activateContractWallet (Wallet 1) createEndpoint
  callEndpoint @"create" h ()
  void $ Emulator.waitNSlots 5
  mapInstance <- getLast <$> observableState h
  case mapInstance of
    Just m -> return m
    Nothing -> throwError $ GenericError "Could not create map"

testAddToEmptyMap :: EmulatorTrace ()
testAddToEmptyMap = do
  mapInstance <- createMap

  h1 <- activateContractWallet (Wallet 1) (endpoints mapInstance)
  void $ Emulator.waitNSlots 1
  callEndpoint @"insert" h1 (5, 3)
  void $ Emulator.waitNSlots 1

testAddSmallest :: EmulatorTrace ()
testAddSmallest = do
  mapInstance <- createMap

  h1 <- activateContractWallet (Wallet 1) (endpoints mapInstance)
  void $ Emulator.waitNSlots 1
  callEndpoint @"insert" h1 (5, 3)
  void $ Emulator.waitNSlots 1
  callEndpoint @"insert" h1 (4, 2)
  void $ Emulator.waitNSlots 1

testAddToEmptyMap' :: TestTree
testAddToEmptyMap' =
  checkPredicateOptions
    (defaultCheckOptions & emulatorConfig .~ emCfg)
    "testAddToEmptyMap"
    ( walletFundsChange (Wallet 1) (Ada.lovelaceValueOf 100_000_000) -- TODO correct assertions
    )
    testAddToEmptyMap

testAddSmallest' :: TestTree
testAddSmallest' =
  checkPredicateOptions
    (defaultCheckOptions & emulatorConfig .~ emCfg)
    "testAddSmallest"
    ( walletFundsChange (Wallet 1) (Ada.lovelaceValueOf 100_000_000) -- TODO correct assertions
    )
    testAddSmallest

mapTests :: TestTree
mapTests =
  testGroup
    "map tests"
    [ testAddToEmptyMap'
    , testAddSmallest'
    ]
