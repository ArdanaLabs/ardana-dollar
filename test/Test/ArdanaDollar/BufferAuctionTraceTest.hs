module Test.ArdanaDollar.BufferAuctionTraceTest (bufferTraceTests) where

import Control.Lens
import Control.Monad (void)
import Control.Monad.Freer.Extras as Extras
import Data.Aeson
import Data.Default (Default (..))
import Data.Map qualified as Map
import Data.Row.Internal
import Data.Semigroup qualified as Semigroup
import Ledger.Ada as Ada
import Ledger.Value as Value
import Plutus.Contract (ContractError)
import Plutus.Contract qualified as Contract
import Plutus.Contract.Schema
import Plutus.Contract.Test
import Plutus.Trace.Emulator as Emulator
import PlutusTx.Prelude
import Test.Tasty
import Prelude (String, undefined)

import ArdanaDollar.Buffer.Endpoints
import ArdanaDollar.Treasury.Endpoints
import ArdanaDollar.Treasury.Types (Treasury, danaAssetClass)
import ArdanaDollar.Vault (dUSDAsset)
import Plutus.PAB.OutputBus

bufferTraceTests :: TestTree
bufferTraceTests =
  testGroup
    "Buffer Trace tests"
    [ debtTraceTest
    , debtAndSurplusTraceTest
    , tooSmallSurplusTraceTest
    , zeroDebtAuction
    , negativeDebtAuction
    ]

debtTraceTest :: TestTree
debtTraceTest =
  checkPredicateOptions
    (defaultCheckOptions & emulatorConfig .~ emCfg)
    "debt auction trace"
    (walletFundsChange (Wallet 2) (assetClassValue dUSDAsset (-100) <> assetClassValue danaAssetClass 2))
    (draftTrace debtAuction)
  where
    debtAuction :: Treasury -> EmulatorTrace ()
    debtAuction treasury = do
      cTreasuryUserId <- activateContractWallet (Wallet 2) (bufferAuctionContract @() @ContractError treasury)
      void $ Emulator.waitNSlots 2

      callEndpoint @"debtAuction" cTreasuryUserId 2
      void $ Emulator.waitNSlots 10

debtAndSurplusTraceTest :: TestTree
debtAndSurplusTraceTest =
  checkPredicateOptions
    (defaultCheckOptions & emulatorConfig .~ emCfg)
    "debt and surplus auction trace"
    (walletFundsChange (Wallet 2) (assetClassValue dUSDAsset 0 <> assetClassValue danaAssetClass 0))
    (draftTrace debtAndSurplusAuction)
  where
    debtAndSurplusAuction :: Treasury -> EmulatorTrace ()
    debtAndSurplusAuction treasury = do
      cTreasuryUserId <- activateContractWallet (Wallet 2) (bufferAuctionContract @() @ContractError treasury)
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
    (walletFundsChange (Wallet 2) (assetClassValue dUSDAsset (-100) <> assetClassValue danaAssetClass 2))
    (draftTrace surplusAuction)
  where
    surplusAuction :: Treasury -> EmulatorTrace ()
    surplusAuction treasury = do
      cTreasuryUserId <- activateContractWallet (Wallet 2) (bufferAuctionContract @() @ContractError treasury)
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
    (walletFundsChange (Wallet 2) (assetClassValue dUSDAsset 0 <> assetClassValue danaAssetClass 0))
    (draftTrace debtAuction)
  where
    debtAuction :: Treasury -> EmulatorTrace ()
    debtAuction treasury = do
      cTreasuryUserId <- activateContractWallet (Wallet 2) (bufferAuctionContract @() @ContractError treasury)
      void $ Emulator.waitNSlots 2

      callEndpoint @"debtAuction" cTreasuryUserId 0
      void $ Emulator.waitNSlots 10

negativeDebtAuction :: TestTree
negativeDebtAuction =
  checkPredicateOptions
    (defaultCheckOptions & emulatorConfig .~ emCfg)
    "negative debt auction trace"
    (walletFundsChange (Wallet 2) (assetClassValue dUSDAsset 0 <> assetClassValue danaAssetClass 0))
    (draftTrace debtAuction)
  where
    debtAuction :: Treasury -> EmulatorTrace ()
    debtAuction treasury = do
      cTreasuryUserId <- activateContractWallet (Wallet 2) (bufferAuctionContract @() @ContractError treasury)
      void $ Emulator.waitNSlots 2

      callEndpoint @"debtAuction" cTreasuryUserId (-1)
      void $ Emulator.waitNSlots 10

emCfg :: EmulatorConfig
emCfg = EmulatorConfig (Left $ Map.fromList [(Wallet w, v' w) | w <- [1 .. 3]]) def def
  where
    v :: Value.Value
    v = Ada.lovelaceValueOf 1_000_000_000

    v' :: Integer -> Value.Value
    v' w
      | w == 1 = v
      | otherwise = v <> assetClassValue dUSDAsset 1000 <> assetClassValue danaAssetClass 10

draftTrace :: (Treasury -> EmulatorTrace ()) -> EmulatorTrace ()
draftTrace cont = do
  cTreasuryId <- activateContractWallet (Wallet 1) (treasuryStartContract <* Contract.waitNSlots 5)
  void $ waitNSlots 5
  treasury <- getBus cTreasuryId
  void $ waitNSlots 5
  _ <- activateContractWallet (Wallet 1) (bufferStartContract @() @ContractError treasury <* Contract.waitNSlots 5)
  void $ waitNSlots 5
  cont treasury

getBus ::
  forall w s e.
  ( AllUniqueLabels (Input s)
  , AllUniqueLabels (Output s)
  , Forall (Input s) Unconstrained1
  , Forall (Input s) FromJSON
  , Forall (Input s) ToJSON
  , Forall (Output s) Unconstrained1
  , Forall (Output s) FromJSON
  , Forall (Output s) ToJSON
  , FromJSON e
  , FromJSON w
  , ToJSON w
  ) =>
  ContractHandle (OutputBus w) s e ->
  EmulatorTrace w
getBus cId = do
  ob <- observableState cId
  case getOutputBus ob of
    Just (Semigroup.Last s) -> return s
    Nothing -> Extras.logError @String "error in getOutputBus" >> undefined
