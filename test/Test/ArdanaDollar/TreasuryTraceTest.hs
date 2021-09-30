module Test.ArdanaDollar.TreasuryTraceTest (treasuryTraceTests) where

import Control.Lens
import Control.Monad (void)
import Data.Aeson (FromJSON, ToJSON)
import Data.Default (Default (..))
import Data.Map qualified as Map
import Data.Semigroup qualified as Semigroup
import Data.Vector (Vector)
import Ledger qualified
import Ledger.Ada as Ada
import Ledger.Value as Value
import Plutus.Contract (Contract, ContractError, EmptySchema)
import Plutus.Contract qualified as Contract
import Plutus.Contract.Test hiding (not)
import Plutus.Contract.Test.Extra (
  dataAtComputedAddress,
  valueAtComputedAddress,
 )
import Plutus.Trace.Emulator as Emulator
import Plutus.V1.Ledger.Contexts (TxOutRef)
import PlutusTx.Prelude
import PlutusTx.UniqueMap qualified as UniqueMap
import Test.Tasty
import Prelude (String)

import ArdanaDollar.Treasury.Endpoints
import ArdanaDollar.Treasury.OffChain (treasuryAddress)
import ArdanaDollar.Treasury.Types (
  Treasury (..),
  TreasuryDatum (..),
  TreasuryDepositParams (..),
  TreasuryStateTokenParams (..),
  danaAssetClass,
 )
import ArdanaDollar.Vault (dUSDAsset)
import Plutus.PAB.OutputBus
import Test.AssertUtils (logChecker, logErrorAndThrow)

treasuryTraceTests :: TestTree
treasuryTraceTests =
  testGroup
    "Treasury Trace tests"
    [ noTreasury
    , -- , cannotStartTreasury
      depositTwiceCostCenter
    , depositIntoTwoCostCenters
    , sumDifferentDeposits
    ]

-- Utils -----------------------------------------------------------------------
treasuryStartContract' :: Contract (OutputBus Treasury) EmptySchema ContractError ()
treasuryStartContract' = treasuryStartContract <* Contract.waitNSlots 5

emCfg :: EmulatorConfig
emCfg = EmulatorConfig (Left $ Map.fromList [(knownWallet w, v' w) | w <- [1 .. 3]]) def def
  where
    v :: Value.Value
    v = Ada.lovelaceValueOf 1_000_000_000

    v' :: Integer -> Value.Value
    v' w
      | w == 1 = v
      | w == 3 = mempty
      | otherwise = v <> assetClassValue dUSDAsset 1000 <> assetClassValue danaAssetClass 10

draftTrace ::
  EmulatorTrace
    ( Treasury
    , ContractHandle (OutputBus (Vector (BuiltinByteString, Value.Value))) TreasurySchema ContractError
    )
draftTrace = do
  cTreasuryId <- activateContractWallet (knownWallet 1) treasuryStartContract'
  void $ waitNSlots 5
  treasury <- getBus cTreasuryId
  void $ waitNSlots 5
  treasuryUserId <- activateContractWallet (knownWallet 2) (treasuryContract @ContractError treasury <* Contract.waitNSlots 5)
  void $ waitNSlots 5
  return (treasury, treasuryUserId)

getBus ::
  forall w s e.
  ( ContractConstraints s
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
    Nothing -> throwError $ GenericError "error in getOutputBus"

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
        pka <- Ledger.pubKeyAddress <$> Contract.ownPubKey @[TxOutRef] @EmptySchema @ContractError
        utxos <- Map.toList <$> Contract.utxosAt pka
        case utxos of
          [] -> Contract.logError @String "No UTXO found at public address"
          (oref, _) : _ -> Contract.tell [oref]
        void $ Contract.waitNSlots 1_000_000
      void $ waitNSlots 5
      orefs <- observableState walletPubKeyId

      case orefs of
        [] -> logErrorAndThrow "Could not find UTXO"
        (oref : _) -> do
          let mockTreasury =
                Treasury
                  { stateTokenSymbol = Value.assetClass Ada.adaSymbol (TokenName "Token1")
                  , stateTokenParams =
                      TreasuryStateTokenParams
                        { stateToken = TokenName "Token2"
                        , initialOutput = oref
                        }
                  }
          cTreasuryUserId <-
            activateContractWallet
              (knownWallet 2)
              (treasuryContract @ContractError mockTreasury <* Contract.waitNSlots 5)
          void $ Emulator.waitNSlots 2

          callEndpoint @"depositFundsWithCostCenter" cTreasuryUserId $
            TreasuryDepositParams
              { treasuryDepositAmount = 50
              , treasuryDepositCurrency = dUSDAsset
              , treasuryDepositCostCenter = "TestCenter1"
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
            { treasuryDepositAmount = 50
            , treasuryDepositCurrency = dUSDAsset
            , treasuryDepositCostCenter = costCenterName
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
          { treasuryDepositAmount = 100
          , treasuryDepositCurrency = dUSDAsset
          , treasuryDepositCostCenter = costCenter1Name
          }
      void $ Emulator.waitNSlots 10
      callEndpoint @"depositFundsWithCostCenter" treasuryUserId $
        TreasuryDepositParams
          { treasuryDepositAmount = 50
          , treasuryDepositCurrency = dUSDAsset
          , treasuryDepositCostCenter = costCenter2Name
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
          { treasuryDepositAmount = 100
          , treasuryDepositCurrency = dUSDAsset
          , treasuryDepositCostCenter = costCenterName
          }
      void $ Emulator.waitNSlots 10
      callEndpoint @"depositFundsWithCostCenter" treasuryUserId $
        TreasuryDepositParams
          { treasuryDepositAmount = 5
          , treasuryDepositCurrency = danaAssetClass
          , treasuryDepositCostCenter = costCenterName
          }
      void $ Emulator.waitNSlots 10
