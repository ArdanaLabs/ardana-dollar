module Test.ArdanaDollar.TreasuryTraceTest (treasuryTraceTests) where

import Control.Lens
import Control.Monad (void)
import Data.Map qualified as Map
import Data.Semigroup qualified as Semigroup
import Data.Vector (Vector)
import Test.Tasty
import Prelude (String)

import Ledger qualified
import Ledger.Ada as Ada
import Ledger.Value as Value
import Plutus.Contract (ContractError, EmptySchema)
import Plutus.Contract qualified as Contract
import Plutus.Contract.Test hiding (not)
import Plutus.Contract.Test.Extra (
  dataAtComputedAddress,
  valueAtComputedAddress,
 )
import Plutus.Trace.Emulator as Emulator hiding (chainState)
import Plutus.V1.Ledger.Contexts (TxOutRef)
import PlutusTx.Prelude
import PlutusTx.UniqueMap qualified as UniqueMap

import ArdanaDollar.Treasury.Endpoints
import ArdanaDollar.Treasury.OffChain (treasuryAddress)
import ArdanaDollar.Treasury.Types (
  NewContract (..),
  Treasury (..),
  TreasuryDatum (..),
  TreasuryDepositParams (..),
  TreasuryStateTokenParams (..),
  TreasuryUpgradeContractTokenParams (..),
  danaAssetClass,
 )
import ArdanaDollar.Vault (dUSDAsset)
import Plutus.PAB.OutputBus
import Test.ArdanaDollar.TreasuryPrerun (
  emCfg,
  startAdminTrace,
  treasuryStartContract',
 )
import Test.AssertUtils (logChecker, logErrorAndThrow)
import Test.TraceUtils (getBus)

treasuryTraceTests :: TestTree
treasuryTraceTests =
  testGroup
    "Treasury Trace tests"
    [ noTreasury
    , depositTwiceCostCenter
    , depositIntoTwoCostCenters
    , sumDifferentDeposits
    , upgrade
    ]

-- Utils -----------------------------------------------------------------------
draftTrace ::
  EmulatorTrace
    ( Treasury
    , ContractHandle (OutputBus (Vector (BuiltinByteString, Value.Value))) TreasurySchema ContractError
    )
draftTrace = do
  _ <- startAdminTrace
  cTreasuryId <- activateContractWallet (knownWallet 1) treasuryStartContract'
  void $ waitNSlots 5
  treasury <- getBus cTreasuryId
  void $ waitNSlots 5
  treasuryUserId <- activateContractWallet (knownWallet 2) (treasuryContract @ContractError treasury <* Contract.waitNSlots 5)
  void $ waitNSlots 5
  return (treasury, treasuryUserId)

-- Tests -----------------------------------------------------------------------
noTreasury :: TestTree
noTreasury =
  checkPredicateOptions
    (defaultCheckOptions & emulatorConfig .~ emCfg)
    "no treasury"
    ( walletFundsChange (knownWallet 1) (assetClassValue dUSDAsset 0 <> assetClassValue danaAssetClass 0)
        .&&. walletFundsChange (knownWallet 2) (assetClassValue dUSDAsset 0 <> assetClassValue danaAssetClass 0)
        .&&. assertInstanceLog t2 (logChecker "no treasury utxo at the script address")
    )
    treasuryTrace
  where
    t2 :: ContractInstanceTag
    t2 = walletInstanceTag (knownWallet 2)

    treasuryTrace :: EmulatorTrace ()
    treasuryTrace = do
      walletPubKeyId <- activateContractWallet (knownWallet 2) $ do
        pk <- Contract.ownPubKey @[(TxOutRef, Ledger.PubKeyHash)] @EmptySchema @ContractError
        let pka = Ledger.pubKeyAddress pk
            pkh = Ledger.pubKeyHash pk
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
          cTreasuryUserId <-
            activateContractWallet
              (knownWallet 2)
              (treasuryContract @ContractError mockTreasury <* Contract.waitNSlots 5)
          void $ Emulator.waitNSlots 2

          callEndpoint @"depositFundsWithCostCenter" cTreasuryUserId $
            TreasuryDepositParams
              { treasuryDeposit'value = Value.assetClassValue dUSDAsset 50
              , treasuryDeposit'costCenter = "TestCenter1"
              }
          void $ Emulator.waitNSlots 10

depositTwiceCostCenter :: TestTree
depositTwiceCostCenter =
  checkPredicateOptions
    (defaultCheckOptions & emulatorConfig .~ emCfg)
    "deposit twice into the same cost center"
    ( walletFundsChange (knownWallet 1) (assetClassValue dUSDAsset 0 <> assetClassValue danaAssetClass 0)
        .&&. walletFundsChange (knownWallet 2) (assetClassValue dUSDAsset (-100) <> assetClassValue danaAssetClass 0)
        .&&. valueAtComputedAddress treasuryStartContract' t1 addressGetter valueChecker
        .&&. dataAtComputedAddress treasuryStartContract' t1 addressGetter dataChecker
    )
    depositTwice
  where
    costCenterName :: BuiltinByteString
    costCenterName = "TestCenter1"

    t1 :: ContractInstanceTag
    t1 = walletInstanceTag (knownWallet 1)

    addressGetter :: OutputBus Treasury -> Maybe Ledger.Address
    addressGetter = fmap (\(Semigroup.Last t) -> treasuryAddress t) . getOutputBus

    valueChecker :: Value.Value -> Bool
    valueChecker v = assetClassValueOf v dUSDAsset == 100

    dataChecker :: TreasuryDatum -> Bool
    dataChecker TreasuryDatum {costCenters = cc} =
      maybe False valueChecker (UniqueMap.lookup costCenterName cc)

    twice :: EmulatorTrace () -> EmulatorTrace ()
    twice f = f >> f

    depositTwice :: EmulatorTrace ()
    depositTwice = do
      (_, treasuryUserId) <- draftTrace
      twice $ do
        callEndpoint @"depositFundsWithCostCenter" treasuryUserId $
          TreasuryDepositParams
            { treasuryDeposit'value = Value.assetClassValue dUSDAsset 50
            , treasuryDeposit'costCenter = costCenterName
            }
        void $ Emulator.waitNSlots 10

depositIntoTwoCostCenters :: TestTree
depositIntoTwoCostCenters =
  checkPredicateOptions
    (defaultCheckOptions & emulatorConfig .~ emCfg)
    "deposit into two different cost center"
    ( walletFundsChange (knownWallet 1) (assetClassValue dUSDAsset 0 <> assetClassValue danaAssetClass 0)
        .&&. walletFundsChange (knownWallet 2) (assetClassValue dUSDAsset (-150) <> assetClassValue danaAssetClass 0)
        .&&. valueAtComputedAddress treasuryStartContract' t1 addressGetter (valueChecker 150)
        .&&. dataAtComputedAddress treasuryStartContract' t1 addressGetter dataChecker
    )
    depositTrace
  where
    costCenter1Name :: BuiltinByteString
    costCenter1Name = "TestCenter1"

    costCenter2Name :: BuiltinByteString
    costCenter2Name = "TestCenter2"

    t1 :: ContractInstanceTag
    t1 = walletInstanceTag (knownWallet 1)

    addressGetter :: OutputBus Treasury -> Maybe Ledger.Address
    addressGetter = fmap (\(Semigroup.Last t) -> treasuryAddress t) . getOutputBus

    valueChecker :: Integer -> Value.Value -> Bool
    valueChecker expected v = assetClassValueOf v dUSDAsset == expected

    dataChecker :: TreasuryDatum -> Bool
    dataChecker TreasuryDatum {costCenters = cc} =
      maybe False (valueChecker 100) (UniqueMap.lookup costCenter1Name cc)
        && maybe False (valueChecker 50) (UniqueMap.lookup costCenter2Name cc)

    depositTrace :: EmulatorTrace ()
    depositTrace = do
      (_, treasuryUserId) <- draftTrace
      callEndpoint @"depositFundsWithCostCenter" treasuryUserId $
        TreasuryDepositParams
          { treasuryDeposit'value = Value.assetClassValue dUSDAsset 100
          , treasuryDeposit'costCenter = costCenter1Name
          }
      void $ Emulator.waitNSlots 10
      callEndpoint @"depositFundsWithCostCenter" treasuryUserId $
        TreasuryDepositParams
          { treasuryDeposit'value = Value.assetClassValue dUSDAsset 50
          , treasuryDeposit'costCenter = costCenter2Name
          }
      void $ Emulator.waitNSlots 10

sumDifferentDeposits :: TestTree
sumDifferentDeposits =
  checkPredicateOptions
    (defaultCheckOptions & emulatorConfig .~ emCfg)
    "deposit different currencies into the same cost center"
    ( walletFundsChange (knownWallet 1) (assetClassValue dUSDAsset 0 <> assetClassValue danaAssetClass 0)
        .&&. walletFundsChange (knownWallet 2) (assetClassValue dUSDAsset (-100) <> assetClassValue danaAssetClass (-5))
        .&&. valueAtComputedAddress treasuryStartContract' t1 addressGetter (valueChecker 15)
        .&&. dataAtComputedAddress treasuryStartContract' t1 addressGetter dataChecker
    )
    depositTrace
  where
    costCenterName :: BuiltinByteString
    costCenterName = "TestCenter1"

    t1 :: ContractInstanceTag
    t1 = walletInstanceTag (knownWallet 1)

    addressGetter :: OutputBus Treasury -> Maybe Ledger.Address
    addressGetter = fmap (\(Semigroup.Last t) -> treasuryAddress t) . getOutputBus

    valueChecker :: Integer -> Value.Value -> Bool
    valueChecker expectedDana v =
      assetClassValueOf v dUSDAsset == 100
        && assetClassValueOf v danaAssetClass == expectedDana -- TODO: treasury has initial DANA, to be changed
    dataChecker :: TreasuryDatum -> Bool
    dataChecker TreasuryDatum {costCenters = cc} =
      maybe False (valueChecker 5) (UniqueMap.lookup costCenterName cc)

    depositTrace :: EmulatorTrace ()
    depositTrace = do
      (_, treasuryUserId) <- draftTrace
      callEndpoint @"depositFundsWithCostCenter" treasuryUserId $
        TreasuryDepositParams
          { treasuryDeposit'value = Value.assetClassValue dUSDAsset 100
          , treasuryDeposit'costCenter = costCenterName
          }
      void $ Emulator.waitNSlots 10
      callEndpoint @"depositFundsWithCostCenter" treasuryUserId $
        TreasuryDepositParams
          { treasuryDeposit'value = Value.assetClassValue danaAssetClass 5
          , treasuryDeposit'costCenter = costCenterName
          }
      void $ Emulator.waitNSlots 10

upgrade :: TestTree
upgrade =
  checkPredicateOptions
    (defaultCheckOptions & emulatorConfig .~ emCfg)
    "upgrade treasury contract"
    ( walletFundsChange (knownWallet 1) (assetClassValue dUSDAsset 0 <> assetClassValue danaAssetClass 0)
        .&&. walletFundsChange (knownWallet 2) (assetClassValue dUSDAsset 0 <> assetClassValue danaAssetClass 0)
        .&&. dataAtComputedAddress treasuryStartContract' t1 addressGetter dataChecker
    )
    upgradeTrace
  where
    nc :: Ledger.ValidatorHash
    nc = Ledger.ValidatorHash "abcd"

    t1 :: ContractInstanceTag
    t1 = walletInstanceTag (knownWallet 1)

    addressGetter :: OutputBus Treasury -> Maybe Ledger.Address
    addressGetter = fmap (\(Semigroup.Last t) -> treasuryAddress t) . getOutputBus

    dataChecker :: TreasuryDatum -> Bool
    dataChecker TreasuryDatum {currentContract = cc} = cc == nc

    upgradeTrace :: EmulatorTrace ()
    upgradeTrace = do
      (_, treasuryUserId) <- draftTrace
      callEndpoint @"initUpgrade" treasuryUserId (NewContract nc)
      void $ Emulator.waitNSlots 10
