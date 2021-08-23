module Test.ArdanaDollar.DanaStakePoolTest (
  testDeposit,
  testWithdraw,
  testProvide,
  testDistribute,
  danaStakePoolTests,
  runTrace,
) where

import ArdanaDollar.DanaStakePool.DanaCurrency
import ArdanaDollar.DanaStakePool.Endpoints as PEndpoints
import ArdanaDollar.Vault as Vault

import Prelude

import Ledger.Value qualified as Value

import Data.Map qualified as Map
import Data.Maybe
import Data.Monoid
import Ledger.Ada as Ada
import Plutus.Contract.Test
import Plutus.Trace.Emulator as Emulator

import Control.Monad (void)

import Data.Default (Default (..))

import Control.Lens

import Test.Tasty

v :: Value.Value
v =
  Ada.lovelaceValueOf 100_000_000
    <> Value.assetClassValue danaAsset 100_000_000
    <> Value.assetClassValue Vault.dUSDAsset 100_000_000

emCfg :: EmulatorConfig
emCfg = EmulatorConfig $ Left $ Map.fromList [(Wallet 1, v), (Wallet 2, v), (Wallet 3, v)]

runTrace :: EmulatorTrace () -> IO ()
runTrace = runEmulatorTraceIO' def emCfg def

initializeSystem :: EmulatorTrace Value.CurrencySymbol
initializeSystem = do
  h <- activateContractWallet (Wallet 1) PEndpoints.initializeSystemEndpoint
  callEndpoint @"initializeSystem" h ()
  void $ Emulator.waitNSlots 5
  fst . Value.unAssetClass . fromJust . getLast <$> observableState h

testDeposit :: EmulatorTrace ()
testDeposit = do
  ac <- initializeSystem

  h1 <- activateContractWallet (Wallet 1) (PEndpoints.endpoints ac)
  void $ Emulator.waitNSlots 5
  callEndpoint @"initializeUser" h1 ()
  void $ Emulator.waitNSlots 5
  callEndpoint @"deposit" h1 5
  void $ Emulator.waitNSlots 5
  callEndpoint @"deposit" h1 4
  void $ Emulator.waitNSlots 5

testWithdraw :: EmulatorTrace ()
testWithdraw = do
  ac <- initializeSystem

  h1 <- activateContractWallet (Wallet 1) (PEndpoints.endpoints ac)
  void $ Emulator.waitNSlots 5
  callEndpoint @"initializeUser" h1 ()
  void $ Emulator.waitNSlots 5
  callEndpoint @"deposit" h1 5
  void $ Emulator.waitNSlots 5
  callEndpoint @"withdraw" h1 2
  void $ Emulator.waitNSlots 5

testProvide :: EmulatorTrace ()
testProvide = do
  ac <- initializeSystem

  h1 <- activateContractWallet (Wallet 1) (PEndpoints.endpoints ac)
  void $ Emulator.waitNSlots 10
  callEndpoint @"provideRewards" h1 5
  void $ Emulator.waitNSlots 5
  callEndpoint @"provideRewards" h1 7
  void $ Emulator.waitNSlots 5

testDistribute :: EmulatorTrace ()
testDistribute = do
  ac <- initializeSystem

  h1 <- activateContractWallet (Wallet 1) (PEndpoints.endpoints ac)
  h2 <- activateContractWallet (Wallet 2) (PEndpoints.endpoints ac)
  h3 <- activateContractWallet (Wallet 3) (PEndpoints.endpoints ac)

  callEndpoint @"initializeUser" h1 ()
  void $ Emulator.waitNSlots 5
  callEndpoint @"initializeUser" h2 ()
  void $ Emulator.waitNSlots 5

  callEndpoint @"provideRewards" h3 503
  void $ Emulator.waitNSlots 5

  callEndpoint @"deposit" h1 60
  void $ Emulator.waitNSlots 5
  callEndpoint @"deposit" h2 40
  void $ Emulator.waitNSlots 5

  callEndpoint @"distributeRewards" h3 ()
  void $ Emulator.waitNSlots 5

  callEndpoint @"withdrawRewards" h1 ()
  void $ Emulator.waitNSlots 5
  callEndpoint @"withdrawRewards" h2 ()
  void $ Emulator.waitNSlots 5

testDeposit' :: TestTree
testDeposit' =
  checkPredicateOptions
    (defaultCheckOptions & emulatorConfig .~ emCfg)
    "deposit to stake pool"
    ( walletFundsChange (Wallet 1) (Value.assetClassValue danaAsset (-9))
    )
    testDeposit

testWithdraw' :: TestTree
testWithdraw' =
  checkPredicateOptions
    (defaultCheckOptions & emulatorConfig .~ emCfg)
    "deposit & withdraw from stake pool"
    ( walletFundsChange (Wallet 1) (Value.assetClassValue danaAsset (-3))
    )
    testWithdraw

testDistribute' :: TestTree
testDistribute' =
  checkPredicateOptions
    (defaultCheckOptions & emulatorConfig .~ emCfg)
    "two users collect rewards"
    ( walletFundsChange (Wallet 1) (Value.assetClassValue Vault.dUSDAsset 301 <> Value.assetClassValue danaAsset (-60))
        .&&. walletFundsChange (Wallet 2) (Value.assetClassValue Vault.dUSDAsset 201 <> Value.assetClassValue danaAsset (-40))
    )
    testDistribute

danaStakePoolTests :: TestTree
danaStakePoolTests =
  testGroup
    "dana stake pool tests"
    [ testDeposit'
    , testWithdraw'
    , testDistribute'
    ]