module Test.ArdanaDollar.DanaStakePoolTest (
  testDeposit,
  testWithdraw,
  testProvide,
  testDistribute1,
  testDistribute2,
  danaStakePoolTests,
  runTrace,
) where

import ArdanaDollar.DanaStakePool.DanaCurrency
import ArdanaDollar.DanaStakePool.Endpoints as PEndpoints
import ArdanaDollar.DanaStakePool.Types (NFTAssetClass (..))
import ArdanaDollar.Vault as Vault

import Prelude

import Ledger.Value qualified as Value

import Data.Map qualified as Map
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
emCfg = EmulatorConfig (Left $ Map.fromList [(knownWallet 1, v), (knownWallet 2, v), (knownWallet 3, v)]) def def

runTrace :: EmulatorTrace () -> IO ()
runTrace = runEmulatorTraceIO' def emCfg

initializeSystem :: EmulatorTrace NFTAssetClass
initializeSystem = do
  h <- activateContractWallet (knownWallet 1) PEndpoints.initializeSystemEndpoint
  callEndpoint @"initializeSystem" h ()
  void $ Emulator.waitNSlots 5
  mAssetClass <- getLast <$> observableState h
  case mAssetClass of
    Just assetClass -> return assetClass
    Nothing -> throwError $ GenericError "Could not initialize system"

testDeposit :: EmulatorTrace ()
testDeposit = do
  ac <- initializeSystem

  h1 <- activateContractWallet (knownWallet 1) (PEndpoints.endpoints ac)
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

  h1 <- activateContractWallet (knownWallet 1) (PEndpoints.endpoints ac)
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

  h1 <- activateContractWallet (knownWallet 1) (PEndpoints.endpoints ac)
  void $ Emulator.waitNSlots 10
  callEndpoint @"provideRewards" h1 5
  void $ Emulator.waitNSlots 5
  callEndpoint @"provideRewards" h1 7
  void $ Emulator.waitNSlots 5

testDistribute1 :: EmulatorTrace ()
testDistribute1 = do
  ac <- initializeSystem

  h1 <- activateContractWallet (knownWallet 1) (PEndpoints.endpoints ac)
  h2 <- activateContractWallet (knownWallet 2) (PEndpoints.endpoints ac)
  h3 <- activateContractWallet (knownWallet 3) (PEndpoints.endpoints ac)

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
  void $ Emulator.waitNSlots 30

  callEndpoint @"withdrawRewards" h1 ()
  void $ Emulator.waitNSlots 5
  callEndpoint @"withdrawRewards" h2 ()
  void $ Emulator.waitNSlots 5

testDistribute2 :: EmulatorTrace ()
testDistribute2 = do
  ac <- initializeSystem

  h1 <- activateContractWallet (knownWallet 1) (PEndpoints.endpoints ac)
  h3 <- activateContractWallet (knownWallet 3) (PEndpoints.endpoints ac)

  callEndpoint @"initializeUser" h1 ()
  void $ Emulator.waitNSlots 5

  callEndpoint @"provideRewards" h3 503
  void $ Emulator.waitNSlots 5

  callEndpoint @"deposit" h1 60
  void $ Emulator.waitNSlots 5

  callEndpoint @"distributeRewards" h3 ()
  void $ Emulator.waitNSlots 30

  callEndpoint @"withdrawRewards" h1 ()
  void $ Emulator.waitNSlots 5

testDistribute3 :: EmulatorTrace ()
testDistribute3 = do
  ac <- initializeSystem

  h1 <- activateContractWallet (knownWallet 1) (PEndpoints.endpoints ac)
  h2 <- activateContractWallet (knownWallet 2) (PEndpoints.endpoints ac)
  h3 <- activateContractWallet (knownWallet 3) (PEndpoints.endpoints ac)

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
  void $ Emulator.waitNSlots 30

  callEndpoint @"withdrawRewards" h1 ()
  void $ Emulator.waitNSlots 5
  callEndpoint @"withdrawRewards" h2 ()
  void $ Emulator.waitNSlots 5

  -- the same as testDistribute1

  callEndpoint @"provideRewards" h3 11
  void $ Emulator.waitNSlots 5

  callEndpoint @"withdraw" h1 10
  void $ Emulator.waitNSlots 5
  callEndpoint @"deposit" h2 10
  void $ Emulator.waitNSlots 5

  callEndpoint @"distributeRewards" h3 ()
  void $ Emulator.waitNSlots 30

  callEndpoint @"withdrawRewards" h1 ()
  void $ Emulator.waitNSlots 5
  callEndpoint @"withdrawRewards" h2 ()
  void $ Emulator.waitNSlots 5

testDeposit' :: TestTree
testDeposit' =
  checkPredicateOptions
    (defaultCheckOptions & emulatorConfig .~ emCfg)
    "deposit to stake pool"
    ( walletFundsChange (knownWallet 1) (Value.assetClassValue danaAsset (-9))
    )
    testDeposit

testWithdraw' :: TestTree
testWithdraw' =
  checkPredicateOptions
    (defaultCheckOptions & emulatorConfig .~ emCfg)
    "deposit & withdraw from stake pool"
    ( walletFundsChange (knownWallet 1) (Value.assetClassValue danaAsset (-3))
    )
    testWithdraw

testDistribute1' :: TestTree
testDistribute1' =
  checkPredicateOptions
    (defaultCheckOptions & emulatorConfig .~ emCfg)
    "two users collect rewards"
    ( walletFundsChange (knownWallet 1) (Value.assetClassValue Vault.dUSDAsset 301 <> Value.assetClassValue danaAsset (-60))
        .&&. walletFundsChange (knownWallet 2) (Value.assetClassValue Vault.dUSDAsset 201 <> Value.assetClassValue danaAsset (-40))
    )
    testDistribute1

testDistribute2' :: TestTree
testDistribute2' =
  checkPredicateOptions
    (defaultCheckOptions & emulatorConfig .~ emCfg)
    "one user collects rewards"
    ( walletFundsChange (knownWallet 1) (Value.assetClassValue Vault.dUSDAsset 503 <> Value.assetClassValue danaAsset (-60))
    )
    testDistribute2

testDistribute3' :: TestTree
testDistribute3' =
  checkPredicateOptions
    (defaultCheckOptions & emulatorConfig .~ emCfg)
    "two reward distribution sessions"
    ( walletFundsChange (knownWallet 1) (Value.assetClassValue Vault.dUSDAsset 307 <> Value.assetClassValue danaAsset (-50))
        .&&. walletFundsChange (knownWallet 2) (Value.assetClassValue Vault.dUSDAsset 207 <> Value.assetClassValue danaAsset (-50))
    )
    testDistribute3

danaStakePoolTests :: TestTree
danaStakePoolTests =
  testGroup
    "dana stake pool tests"
    [ testDeposit'
    , testWithdraw'
    , testDistribute1'
    , testDistribute2'
    , testDistribute3'
    ]
