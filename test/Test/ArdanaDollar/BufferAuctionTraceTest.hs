module Test.ArdanaDollar.BufferAuctionTraceTest (bufferTraceTests) where

import Control.Lens
import Control.Monad (void)
import Data.Map qualified as Map
import Ledger qualified
import Ledger.Ada as Ada
import Ledger.Value as Value
import Plutus.Contract (ContractError, EmptySchema)
import Plutus.Contract qualified as Contract
import Plutus.Contract.Test
import Plutus.Trace.Emulator as Emulator
import Plutus.V1.Ledger.Contexts (TxOutRef)
import PlutusTx.Prelude
import Test.Tasty
import Prelude (String)

import ArdanaDollar.Buffer.Endpoints
import ArdanaDollar.Treasury.Types (
  Treasury (..),
  TreasuryStateTokenParams (..),
  TreasuryUpgradeContractTokenParams (..),
  danaAssetClass,
 )
import ArdanaDollar.Vault (dUSDAsset)
import Test.ArdanaDollar.TreasuryPrerun (
  emCfg,
  startAdminTrace,
  treasuryStartContract',
 )
import Test.AssertUtils (logChecker, logErrorAndThrow)
import Test.TraceUtils (getBus)

bufferTraceTests :: TestTree
bufferTraceTests =
  testGroup
    "Buffer Trace tests"
    [ noTreasuryTest
    , noBufferTest
    , debtTraceTest
    , debtAndSurplusTraceTest
    , tooSmallSurplusTraceTest
    , zeroDebtAuction
    , negativeDebtAuction
    , indivisibleSurplusAuction
    ]

-- Utils -----------------------------------------------------------------------
activateTreasuryDraftTrace :: EmulatorTrace Treasury
activateTreasuryDraftTrace = do
  _ <- startAdminTrace
  cTreasuryId <- activateContractWallet (knownWallet 1) treasuryStartContract'
  void $ waitNSlots 5
  treasury <- getBus cTreasuryId
  void $ waitNSlots 5
  return treasury

draftTrace :: EmulatorTrace Treasury
draftTrace = do
  treasury <- activateTreasuryDraftTrace
  _ <- activateContractWallet (knownWallet 1) (bufferStartContract @() @ContractError treasury (50, 50) <* Contract.waitNSlots 5)
  void $ waitNSlots 5
  return treasury

-- Tests -----------------------------------------------------------------------
noTreasuryTest :: TestTree
noTreasuryTest =
  checkPredicateOptions
    (defaultCheckOptions & emulatorConfig .~ emCfg)
    "no treasury"
    ( walletFundsChange (knownWallet 1) (assetClassValue dUSDAsset 0 <> assetClassValue danaAssetClass 0)
        .&&. assertInstanceLog t1 (logChecker "no treasury utxo at the script address")
    )
    bufferTrace
  where
    t1 :: ContractInstanceTag
    t1 = walletInstanceTag (knownWallet 1)

    bufferTrace :: EmulatorTrace ()
    bufferTrace = do
      walletPubKeyId <- activateContractWallet (knownWallet 2) $ do
        pkh <- Contract.ownPubKeyHash @[(TxOutRef, Ledger.PubKeyHash)] @EmptySchema @ContractError
        let pka = Ledger.pubKeyHashAddress pkh
        utxos <- Map.toList <$> Contract.utxosAt pka
        case utxos of
          [] -> Contract.logError @String "No UTXO found at public address"
          (oref, _) : _ -> Contract.tell [(oref, pkh)]
        void $ Contract.waitNSlots 1_000_000
      void $ waitNSlots 5
      orefs <- observableState walletPubKeyId

      case orefs of
        [] -> logErrorAndThrow "Could not find UTXO"
        ((oref, pkh) : _) -> do
          let mockTreasury =
                Treasury
                  { treasury'peggedCurrency = "USD"
                  , treasury'stateTokenSymbol = Value.assetClass Ada.adaSymbol (TokenName "Token1")
                  , treasury'stateTokenParams =
                      TreasuryStateTokenParams
                        { stateToken = TokenName "Token2"
                        , initialOutput = oref
                        }
                  , treasury'upgradeTokenSymbol = Value.assetClass Ada.adaSymbol (TokenName "Token3")
                  , treasury'upgradeTokenParams =
                      TreasuryUpgradeContractTokenParams
                        { upgradeToken'initialOwner = pkh
                        , upgradeToken'peggedCurrency = "USD"
                        , upgradeToken'initialOutput = oref
                        }
                  }
          cTreasuryUserId <- activateContractWallet (knownWallet 1) (bufferAuctionContract @() @ContractError mockTreasury)
          void $ Emulator.waitNSlots 2

          callEndpoint @"debtAuction" cTreasuryUserId 2
          void $ Emulator.waitNSlots 10

noBufferTest :: TestTree
noBufferTest =
  checkPredicateOptions
    (defaultCheckOptions & emulatorConfig .~ emCfg)
    "no buffer"
    ( walletFundsChange (knownWallet 1) (assetClassValue dUSDAsset 0 <> assetClassValue danaAssetClass 0)
        .&&. assertInstanceLog t1 (logChecker "no buffer utxo at the script address")
    )
    (activateTreasuryDraftTrace >>= bufferTrace)
  where
    t1 :: ContractInstanceTag
    t1 = walletInstanceTag (knownWallet 1)

    bufferTrace :: Treasury -> EmulatorTrace ()
    bufferTrace treasury = do
      cTreasuryUserId <- activateContractWallet (knownWallet 1) (bufferAuctionContract @() @ContractError treasury)
      void $ Emulator.waitNSlots 2

      callEndpoint @"debtAuction" cTreasuryUserId 2
      void $ Emulator.waitNSlots 10

debtTraceTest :: TestTree
debtTraceTest =
  checkPredicateOptions
    (defaultCheckOptions & emulatorConfig .~ emCfg)
    "debt auction trace"
    (walletFundsChange (knownWallet 2) (assetClassValue dUSDAsset (-100) <> assetClassValue danaAssetClass 2))
    (draftTrace >>= debtAuction)
  where
    debtAuction :: Treasury -> EmulatorTrace ()
    debtAuction treasury = do
      cTreasuryUserId <- activateContractWallet (knownWallet 2) (bufferAuctionContract @() @ContractError treasury)
      void $ Emulator.waitNSlots 2

      callEndpoint @"debtAuction" cTreasuryUserId 2
      void $ Emulator.waitNSlots 10

debtAndSurplusTraceTest :: TestTree
debtAndSurplusTraceTest =
  checkPredicateOptions
    (defaultCheckOptions & emulatorConfig .~ emCfg)
    "debt and surplus auction trace"
    (walletFundsChange (knownWallet 2) (assetClassValue dUSDAsset 0 <> assetClassValue danaAssetClass 0))
    (draftTrace >>= debtAndSurplusAuction)
  where
    debtAndSurplusAuction :: Treasury -> EmulatorTrace ()
    debtAndSurplusAuction treasury = do
      cTreasuryUserId <- activateContractWallet (knownWallet 2) (bufferAuctionContract @() @ContractError treasury)
      void $ Emulator.waitNSlots 2

      callEndpoint @"debtAuction" cTreasuryUserId 2
      void $ Emulator.waitNSlots 10
      callEndpoint @"surplusAuction" cTreasuryUserId 100
      void $ Emulator.waitNSlots 10

tooSmallSurplusTraceTest :: TestTree
tooSmallSurplusTraceTest =
  checkPredicateOptions
    (defaultCheckOptions & emulatorConfig .~ emCfg)
    "too small surplus auction trace"
    (walletFundsChange (knownWallet 2) (assetClassValue dUSDAsset (-100) <> assetClassValue danaAssetClass 2))
    (draftTrace >>= surplusAuction)
  where
    surplusAuction :: Treasury -> EmulatorTrace ()
    surplusAuction treasury = do
      cTreasuryUserId <- activateContractWallet (knownWallet 2) (bufferAuctionContract @() @ContractError treasury)
      void $ Emulator.waitNSlots 2

      callEndpoint @"debtAuction" cTreasuryUserId 2
      void $ Emulator.waitNSlots 10
      callEndpoint @"surplusAuction" cTreasuryUserId 15
      void $ Emulator.waitNSlots 10

zeroDebtAuction :: TestTree
zeroDebtAuction =
  checkPredicateOptions
    (defaultCheckOptions & emulatorConfig .~ emCfg)
    "zero debt auction trace"
    (walletFundsChange (knownWallet 2) (assetClassValue dUSDAsset 0 <> assetClassValue danaAssetClass 0))
    (draftTrace >>= debtAuction)
  where
    debtAuction :: Treasury -> EmulatorTrace ()
    debtAuction treasury = do
      cTreasuryUserId <- activateContractWallet (knownWallet 2) (bufferAuctionContract @() @ContractError treasury)
      void $ Emulator.waitNSlots 2

      callEndpoint @"debtAuction" cTreasuryUserId 0
      void $ Emulator.waitNSlots 10

negativeDebtAuction :: TestTree
negativeDebtAuction =
  checkPredicateOptions
    (defaultCheckOptions & emulatorConfig .~ emCfg)
    "negative debt auction trace"
    (walletFundsChange (knownWallet 2) (assetClassValue dUSDAsset 0 <> assetClassValue danaAssetClass 0))
    (draftTrace >>= debtAuction)
  where
    debtAuction :: Treasury -> EmulatorTrace ()
    debtAuction treasury = do
      cTreasuryUserId <- activateContractWallet (knownWallet 2) (bufferAuctionContract @() @ContractError treasury)
      void $ Emulator.waitNSlots 2

      callEndpoint @"debtAuction" cTreasuryUserId (-1)
      void $ Emulator.waitNSlots 10

indivisibleSurplusAuction :: TestTree
indivisibleSurplusAuction =
  checkPredicateOptions
    (defaultCheckOptions & emulatorConfig .~ emCfg)
    "indivisible surplus auction"
    (walletFundsChange (knownWallet 2) (assetClassValue dUSDAsset 0 <> assetClassValue danaAssetClass 0))
    (activateTreasuryDraftTrace >>= surplusAuction)
  where
    surplusAuction :: Treasury -> EmulatorTrace ()
    surplusAuction treasury = do
      _ <- activateContractWallet (knownWallet 1) (bufferStartContract @() @ContractError treasury (0, 0) <* Contract.waitNSlots 5)
      void $ waitNSlots 5

      cTreasuryUserId <- activateContractWallet (knownWallet 2) (bufferAuctionContract @() @ContractError treasury)
      void $ Emulator.waitNSlots 2

      callEndpoint @"surplusAuction" cTreasuryUserId 50
      void $ Emulator.waitNSlots 10
