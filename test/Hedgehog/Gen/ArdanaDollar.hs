module Hedgehog.Gen.ArdanaDollar (
  vaultDatum,
  vaultRedeemer,
  bufferAction,
  bufferDatum,
  danaNftAssetClass,
  danaBalance,
  danaUserData,
  danaTraversalState,
  danaGlobalData,
  danaDatum,
  danaRedeemer,
  treasury,
  treasuryAction,
  treasuryStateTokenParams,
  treasuryDatum,
  treasuryDepositParams,
  treasurySpendParams,
) where

import Control.Monad (replicateM)
import Data.Kind (Type)
import Data.List (sort)
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Gen.Extra (integer)
import Hedgehog.Range qualified as Range
import Prelude

import Hedgehog.Gen.Plutus (
  assetClass,
  builtinByteString,
  pubKeyHash,
  tokenName,
  txOutRef,
  validatorHash,
  value,
 )
import PlutusTx.Prelude qualified as P
import PlutusTx.UniqueMap qualified as UniqueMap

import ArdanaDollar.Buffer.Types qualified as Buffer (
  BufferAction (..),
  BufferDatum (..),
 )
import ArdanaDollar.DanaStakePool.Types qualified as DanaStakePool (
  Balance (..),
  Datum (..),
  GlobalData (..),
  NFTAssetClass (..),
  Redeemer (..),
  TraversalState (..),
  UserData (..),
 )
import ArdanaDollar.Treasury.Types qualified as Treasury (
  NewContract (..),
  Treasury (..),
  TreasuryAction (..),
  TreasuryDatum (..),
  TreasuryDepositParams (..),
  TreasurySpendParams (..),
  TreasuryStateTokenParams (..),
  TreasuryUpgradeContractTokenParams (..),
 )
import ArdanaDollar.Vault (
  VaultDatum (..),
  VaultRedeemer (CollateralRedeemer, DebtRedeemer),
 )

vaultDatum :: forall (m :: Type -> Type). MonadGen m => m VaultDatum
vaultDatum = VaultDatum <$> integer <*> integer

vaultRedeemer :: forall (m :: Type -> Type). MonadGen m => m VaultRedeemer
vaultRedeemer = Gen.element [CollateralRedeemer, DebtRedeemer]

bufferAction :: forall (m :: Type -> Type). MonadGen m => m Buffer.BufferAction
bufferAction =
  Gen.choice
    [ Buffer.MkDebtBid <$> integer
    , Buffer.MkSurplusBid <$> integer
    ]

bufferDatum :: forall (m :: Type -> Type). MonadGen m => m Buffer.BufferDatum
bufferDatum = Buffer.BufferDatum <$> integer <*> integer

danaNftAssetClass :: forall (m :: Type -> Type). MonadGen m => m DanaStakePool.NFTAssetClass
danaNftAssetClass = DanaStakePool.NFTAssetClass <$> assetClass

danaBalance :: forall (m :: Type -> Type). MonadGen m => m DanaStakePool.Balance
danaBalance = DanaStakePool.Balance <$> value <*> value

danaUserData :: forall (m :: Type -> Type). MonadGen m => m DanaStakePool.UserData
danaUserData = DanaStakePool.UserData <$> pubKeyHash <*> danaBalance <*> integer

danaTraversalState :: forall (m :: Type -> Type). MonadGen m => m DanaStakePool.TraversalState
danaTraversalState = Gen.sized $ \n ->
  Gen.frequency
    [ (2, pure DanaStakePool.TraversalInactive)
    , (1 + Range.unSize n, DanaStakePool.TraversalActive <$> value <*> integer)
    ]

danaGlobalData :: forall (m :: Type -> Type). MonadGen m => m DanaStakePool.GlobalData
danaGlobalData =
  DanaStakePool.GlobalData <$> value
    <*> integer
    <*> Gen.bool
    <*> danaTraversalState

danaDatum :: forall (m :: Type -> Type). MonadGen m => m DanaStakePool.Datum
danaDatum =
  Gen.choice
    [ DanaStakePool.UserDatum <$> danaUserData
    , DanaStakePool.GlobalDatum <$> danaGlobalData
    ]

danaRedeemer :: forall (m :: Type -> Type). MonadGen m => m DanaStakePool.Redeemer
danaRedeemer =
  Gen.element
    [ DanaStakePool.DepositOrWithdraw
    , DanaStakePool.ProvideRewards
    , DanaStakePool.DistributeRewards
    , DanaStakePool.WithdrawRewards
    , DanaStakePool.InitializeUser
    ]

peggedCurrency :: forall (m :: Type -> Type). MonadGen m => m P.BuiltinByteString
peggedCurrency =
  Gen.frequency
    [ (16, pure "USD")
    , (2, pure "EUR")
    , (2, pure "GBP")
    , (1, pure "PLN")
    , (1, builtinByteString (Range.singleton 3))
    ]

treasury :: forall (m :: Type -> Type). MonadGen m => m Treasury.Treasury
treasury =
  Treasury.Treasury <$> peggedCurrency
    <*> assetClass
    <*> treasuryStateTokenParams
    <*> assetClass
    <*> treasuryUpgradeContractTokenParams

treasuryAction :: forall (m :: Type -> Type). MonadGen m => m Treasury.TreasuryAction
treasuryAction =
  Gen.choice
    [ pure Treasury.BorrowForAuction
    , Treasury.DepositFundsWithCostCenter <$> treasuryDepositParams
    , Treasury.SpendFundsFromCostCenter <$> builtinByteString (Range.constant 0 128)
    , pure Treasury.AllowMint
    , pure Treasury.AllowBurn
    , Treasury.InitiateUpgrade <$> (Treasury.NewContract <$> validatorHash)
    ]

treasuryStateTokenParams :: forall (m :: Type -> Type). MonadGen m => m Treasury.TreasuryStateTokenParams
treasuryStateTokenParams = Treasury.TreasuryStateTokenParams <$> tokenName <*> txOutRef

treasuryUpgradeContractTokenParams :: forall (m :: Type -> Type). MonadGen m => m Treasury.TreasuryUpgradeContractTokenParams
treasuryUpgradeContractTokenParams =
  Treasury.TreasuryUpgradeContractTokenParams
    <$> pubKeyHash
    <*> builtinByteString (Range.constant 0 128)
    <*> txOutRef

treasuryDatum :: forall (m :: Type -> Type). MonadGen m => m Treasury.TreasuryDatum
treasuryDatum =
  Treasury.TreasuryDatum <$> integer
    <*> validatorHash
    <*> uniqueMap (Range.linear 0 10) (builtinByteString (Range.constant 0 128)) value

treasuryDepositParams :: forall (m :: Type -> Type). MonadGen m => m Treasury.TreasuryDepositParams
treasuryDepositParams =
  Treasury.TreasuryDepositParams <$> value <*> builtinByteString (Range.constant 0 128)

treasurySpendParams :: forall (m :: Type -> Type). MonadGen m => m Treasury.TreasurySpendParams
treasurySpendParams =
  Treasury.TreasurySpendParams <$> value <*> builtinByteString (Range.constant 0 128) <*> pubKeyHash

uniqueMap ::
  forall (m :: Type -> Type) (k :: Type) (v :: Type).
  (MonadGen m, P.Eq k, Ord k) =>
  Range.Range Int ->
  m k ->
  m v ->
  m (UniqueMap.Map k v)
uniqueMap range keyGen valGen = Gen.sized $ \n -> do
  mapSize <- Gen.integral_ range
  keys <- sort <$> replicateM mapSize keyGen
  values <- replicateM mapSize valGen
  if Range.lowerBound n range <= mapSize
    then pure $ UniqueMap.fromList (zip keys values)
    else Gen.discard
