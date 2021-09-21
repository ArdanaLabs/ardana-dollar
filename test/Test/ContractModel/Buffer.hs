{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Test.ContractModel.Buffer (contractTests) where

import Control.Lens hiding (elements)
import Control.Monad (void)
import Control.Monad.Freer qualified as Freer
import Control.Monad.Freer.Error qualified as FrError
import Data.Default (Default (def))
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.OpenUnion.Internal (FindElem)
import Data.Semigroup qualified as Semigroup
import Streaming.Prelude qualified as S
import Test.QuickCheck (
  Gen,
  Property,
  arbitrary,
  elements,
  frequency,
  getPositive,
  oneof,
  withMaxSuccess,
 )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Prelude

import Ledger.Ada qualified as Ada
import Ledger.Value qualified as Value
import Plutus.Contract (Contract, ContractError, EmptySchema)
import Plutus.Contract qualified as Contract
import Plutus.Contract.Test hiding (not)
import Plutus.Contract.Test.ContractModel (
  Actions,
  ContractInstanceSpec (..),
  ContractModel (..),
  HandleFun,
  ModelState,
  Spec,
  balanceChange,
  contractState,
  deposit,
  propRunActionsWithOptions,
  wait,
  withdraw,
  ($=),
  ($~),
 )
import Plutus.PAB.OutputBus (OutputBus (getOutputBus))
import Plutus.Trace.Emulator (EmulatorConfig (..), EmulatorTrace, callEndpoint)
import Plutus.Trace.Emulator qualified as Emulator
import Wallet.Emulator.Folds qualified as Folds
import Wallet.Emulator.Stream qualified as Stream

import ArdanaDollar.Buffer.Endpoints
import ArdanaDollar.Treasury.Endpoints (treasuryStartContract)
import ArdanaDollar.Treasury.Types (Treasury, danaAssetClass)
import ArdanaDollar.Utils (safeDivide)
import ArdanaDollar.Vault (dUSDAsset)
import Test.TraceUtils (getBus)

-- MODEL
data BufferModel = BufferModel
  { _availableDana :: Integer
  , _initialized :: Bool
  }
  deriving (Show)

makeLenses 'BufferModel

deriving instance Eq (ContractInstanceKey BufferModel w schema err)
deriving instance Show (ContractInstanceKey BufferModel w schema err)

instance ContractModel BufferModel where
  data ContractInstanceKey BufferModel w schema err where
    BufferAuction :: Wallet -> ContractInstanceKey BufferModel () BufferSchema ContractError

  data Action BufferModel
    = DebtAuction Wallet Integer
    | SurplusAuction Wallet Integer
    | Wait
    deriving (Eq, Show)

  initialState :: BufferModel
  initialState = BufferModel {_availableDana = 10, _initialized = False} -- TODO: it's basic treasury DANA

  perform :: HandleFun BufferModel -> ModelState BufferModel -> Action BufferModel -> EmulatorTrace ()
  perform handle _s cmd = case cmd of
    Wait -> do
      cTreasuryId <- Emulator.activateContractWallet w1 (treasuryStartContract <* Contract.waitNSlots 5)
      void $ Emulator.waitNSlots 5
      treasury <- getBus cTreasuryId
      void $ Emulator.waitNSlots 5
      _ <- Emulator.activateContractWallet w1 (bufferStartContract @() @ContractError treasury <* Contract.waitNSlots 5)
      void $ Emulator.waitNSlots 15
    DebtAuction w i -> do
      callEndpoint @"debtAuction" (handle $ BufferAuction w) i
      void $ Emulator.waitNSlots 10
    SurplusAuction w i -> do
      callEndpoint @"surplusAuction" (handle $ BufferAuction w) i
      void $ Emulator.waitNSlots 10

  nextState :: Action BufferModel -> Spec BufferModel ()
  nextState (DebtAuction w dana) = do
    let toGet = Value.assetClassValue danaAssetClass dana
    availableDana $~ (\x -> x - dana)
    deposit w toGet
    withdraw w $ Value.assetClassValue dUSDAsset (dana * 50) -- TODO: no magic numbers
    wait 2
  nextState (SurplusAuction w dusd) = do
    let dana = fromMaybe 0 $ dusd `safeDivide` 50 -- TODO: no magic numbers
        toGiveBack = Value.assetClassValue danaAssetClass dana
    availableDana $~ (+ dana)
    withdraw w toGiveBack
    deposit w $ Value.assetClassValue dUSDAsset dusd
    wait 2
  nextState Wait = (initialized $= True) >> wait 2

  arbitraryAction :: ModelState BufferModel -> Gen (Action BufferModel)
  arbitraryAction _s =
    oneof
      [ DebtAuction <$> genWallet <*> genPositiveInteger
      , frequency
          [ (10, SurplusAuction <$> genWallet <*> genDusdAmountInteger)
          , (1, (`SurplusAuction` 50) <$> genWallet)
          ]
      , pure Wait
      ]

  precondition :: ModelState BufferModel -> Action BufferModel -> Bool
  precondition s = \case
    Wait -> not hasInited
    DebtAuction _w i -> hasInited && available >= i
    SurplusAuction w dusd ->
      let dana = fromMaybe 0 $ dusd `safeDivide` 50 -- TODO: no magic numbers
       in hasInited && Value.assetClassValueOf (s ^. balanceChange w) danaAssetClass >= dana
    where
      hasInited :: Bool
      hasInited = s ^. contractState . initialized

      available :: Integer
      available = s ^. contractState . availableDana

handleSpec :: [ContractInstanceSpec BufferModel]
handleSpec = case getTreasuryParam of
  Left _ -> []
  Right t ->
    [ ContractInstanceSpec
      (BufferAuction w)
      w
      (bufferAuctionContract @() @ContractError t)
    | w <- knownWallets
    ]

genPositiveInteger :: Gen Integer
genPositiveInteger = getPositive <$> arbitrary

genWallet :: Gen Wallet
genWallet = elements knownWallets

genDusdAmountInteger :: Gen Integer
genDusdAmountInteger = (* 50) <$> genPositiveInteger

getTreasuryParam :: Either String Treasury
getTreasuryParam = case run of
  Left err -> Left $ show err
  Right sOf -> getTreasuryFromBus $ S.fst' sOf
  where
    getTreasuryFromBus :: OutputBus Treasury -> Either String Treasury
    getTreasuryFromBus ob = case getOutputBus ob of
      Just (Semigroup.Last s) -> Right s
      Nothing -> Left "error in getOutputBus"

    con :: Contract (OutputBus Treasury) EmptySchema ContractError ()
    con = treasuryStartContract <* Contract.waitNSlots 10

    fld ::
      (FindElem (FrError.Error Folds.EmulatorFoldErr) effs) =>
      Folds.EmulatorEventFoldM effs (OutputBus Treasury)
    fld = Folds.instanceAccumState con (Emulator.walletInstanceTag w1)

    run :: Either Folds.EmulatorFoldErr (S.Of (OutputBus Treasury) ())
    run = Freer.run $
      FrError.runError @Folds.EmulatorFoldErr $
        Stream.foldEmulatorStreamM fld $
          Stream.takeUntilSlot 10 $
            Emulator.runEmulatorStream emCfg $
              do
                void $ Emulator.activateContractWallet w1 con
                void $ Emulator.waitNSlots 20

-- Property tests
contractTests :: TestTree
contractTests =
  testGroup "Buffer Model" [testProperty "Buffer" $ withMaxSuccess 200 prop_Buffer]

prop_Buffer :: Actions BufferModel -> Property
prop_Buffer = propRunActionsWithOptions opts handleSpec (const $ pure True)
  where
    opts = defaultCheckOptions & emulatorConfig .~ emCfg

emCfg :: EmulatorConfig
emCfg =
  EmulatorConfig
    (Left $ Map.fromList [(w, v) | w <- knownWallets])
    def
    def
  where
    -- we need A LOT of Ada for QuickCheck tests
    v :: Value.Value
    v =
      Ada.lovelaceValueOf 1_000_000_000_000_000
        <> Value.assetClassValue dUSDAsset 1000
