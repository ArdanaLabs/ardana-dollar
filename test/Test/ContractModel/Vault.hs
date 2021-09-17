{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Test.ContractModel.Vault (contractTests) where

import Control.Lens
import Control.Monad (void)
import Data.Default (Default (def))
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Monoid (Last)
import Data.Text (Text)
import Test.QuickCheck (
  Gen,
  Property,
  arbitrary,
  chooseInteger,
  frequency,
  getPositive,
  oneof,
  scale,
  withMaxSuccess,
 )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Prelude

import Ledger.Ada qualified as Ada
import Ledger.Value qualified as Value
import Plutus.Contract.Test hiding (not)
import Plutus.Contract.Test.ContractModel (
  Actions,
  ContractInstanceSpec (..),
  ContractModel (..),
  HandleFun,
  ModelState,
  Spec,
  burn,
  contractState,
  deposit,
  mint,
  propRunActionsWithOptions,
  wait,
  withdraw,
  ($=),
  ($~),
 )
import Plutus.Trace.Emulator (EmulatorConfig (..), EmulatorTrace, callEndpoint)
import Plutus.Trace.Emulator qualified as Emulator

import ArdanaDollar.Utils (safeDivide)
import ArdanaDollar.Vault (
  VaultDatum,
  dUSDAsset,
  isCollaterizationRatioOk,
  vaultContract,
  type VaultSchema,
 )

-- MODEL
data VaultModel = VaultModel
  { _initialized :: Bool
  , _collateral :: Integer
  , _mintedDusd :: Integer
  }
  deriving (Prelude.Show)

makeLenses 'VaultModel

deriving instance Prelude.Eq (ContractInstanceKey VaultModel w schema err)
deriving instance Prelude.Show (ContractInstanceKey VaultModel w schema err)

instance ContractModel VaultModel where
  data ContractInstanceKey VaultModel w schema err where
    WalletKey :: Wallet -> ContractInstanceKey VaultModel (Last VaultDatum) VaultSchema Text

  data Action VaultModel
    = Initialize
    | Deposit Integer
    | Withdraw Integer
    | MintDUSD Integer
    | RepayDUSD Integer
    deriving (Prelude.Eq, Prelude.Show)

  initialState :: VaultModel
  initialState =
    VaultModel
      { _initialized = False
      , _collateral = 0
      , _mintedDusd = 0
      }

  perform :: HandleFun VaultModel -> ModelState VaultModel -> Action VaultModel -> EmulatorTrace ()
  perform handle _s cmd = case cmd of
    Initialize -> do
      callEndpoint @"initializeVault" (handle $ WalletKey w1) ()
      void $ Emulator.waitNSlots 10
    Deposit i -> do
      callEndpoint @"depositCollateral" (handle $ WalletKey w1) i
      void $ Emulator.waitNSlots 10
    Withdraw i -> do
      callEndpoint @"withdrawCollateral" (handle $ WalletKey w1) i
      void $ Emulator.waitNSlots 10
    MintDUSD i -> do
      callEndpoint @"mintDUSD" (handle $ WalletKey w1) i
      void $ Emulator.waitNSlots 10
    RepayDUSD i -> do
      callEndpoint @"repayDUSD" (handle $ WalletKey w1) i
      void $ Emulator.waitNSlots 10

  nextState :: Action VaultModel -> Spec VaultModel ()
  nextState Initialize = initialized $= True >> wait 10
  nextState (Deposit i) = do
    collateral $~ (+ i)
    withdraw w1 $ Ada.lovelaceValueOf i
    wait 2
  nextState (Withdraw i) = do
    collateral $~ (\x -> x - i)
    deposit w1 $ Ada.lovelaceValueOf i
    wait 2
  nextState (MintDUSD i) = do
    let toMint = Value.assetClassValue dUSDAsset i
    mintedDusd $~ (+ i)
    mint toMint
    deposit w1 toMint
    wait 2
  nextState (RepayDUSD i) = do
    let toBurn = Value.assetClassValue dUSDAsset i
    mintedDusd $~ (\x -> x - i)
    burn toBurn
    withdraw w1 toBurn
    wait 2

  arbitraryAction :: ModelState VaultModel -> Gen (Action VaultModel)
  arbitraryAction s =
    oneof
      [ pure Initialize
      , Deposit <$> genAdaAmountInteger
      , frequency
          [ (10, Withdraw <$> genAdaAmountInteger)
          , (1, pure $ Withdraw coll)
          ]
      , frequency
          [ (1, MintDUSD <$> genPositiveInteger)
          , (10, MintDUSD <$> chooseInteger (1, scaleLovelaceToAda coll))
          ]
      , frequency
          [ (1, RepayDUSD <$> genPositiveInteger)
          , (9, RepayDUSD <$> chooseInteger (1, mintDebt))
          , (1, pure $ RepayDUSD mintDebt)
          ]
      ]
    where
      coll :: Integer
      coll = max 1 (s ^. contractState . collateral)

      mintDebt :: Integer
      mintDebt = max 1 (s ^. contractState . mintedDusd)

      scaleLovelaceToAda :: Integer -> Integer
      scaleLovelaceToAda l = max 1 (fromMaybe 0 (l `safeDivide` 1_000_000))

  precondition :: ModelState VaultModel -> Action VaultModel -> Bool
  precondition s = \case
    Initialize -> not hasInited
    Deposit _ -> hasInited
    Withdraw i -> hasInited && coll >= i && isCollaterizationRatioOk (coll - i) mintDebt
    MintDUSD i -> hasInited && coll > 0 && isCollaterizationRatioOk coll (mintDebt + i)
    RepayDUSD i -> hasInited && mintDebt >= i
    where
      coll :: Integer
      coll = s ^. contractState . collateral

      mintDebt :: Integer
      mintDebt = s ^. contractState . mintedDusd

      hasInited :: Bool
      hasInited = s ^. contractState . initialized

handleSpec :: [ContractInstanceSpec VaultModel]
handleSpec = [ContractInstanceSpec (WalletKey w) w vaultContract | w <- wallets]

wallets :: [Wallet]
wallets = [w1]

genPositiveInteger :: Gen Integer
genPositiveInteger = getPositive <$> arbitrary

genAdaAmountInteger :: Gen Integer
genAdaAmountInteger = scale (* 1_000_000) genPositiveInteger

-- Property tests
contractTests :: TestTree
contractTests =
  testGroup
    "Vault Model"
    [testProperty "Vault" $ withMaxSuccess 100 prop_Vault]

prop_Vault :: Actions VaultModel -> Property
prop_Vault = propRunActionsWithOptions opts handleSpec (const $ pure True)
  where
    opts = defaultCheckOptions & emulatorConfig .~ emCfg

emCfg :: EmulatorConfig
emCfg =
  EmulatorConfig
    (Left $ Map.fromList [(knownWallet w, v) | w <- [1 .. 10]])
    def
    def
  where
    -- we need A LOT of Ada for QuickCheck tests
    v :: Value.Value
    v = Ada.lovelaceValueOf 1_000_000_000_000_000
