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
  oracleMintingParams,
  oracleValidatorParams,
  priceTracking,
  treasury,
  treasuryAction,
  treasuryStateTokenParams,
  treasuryCostCenters,
  treasuryState,
  treasuryDepositParams,
  treasurySpendParams,
  onchainMapMapInstance,
  onchainMapPointer,
  onchainMapMap,
  onchainMapNode,
  onchainMapDatum,
  onchainTokenRedeemer,
  onchainRedeemer,
  newContract,
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
  address,
  assetClass,
  builtinByteString,
  currencySymbol,
  natural,
  positiveValue,
  pubKeyHash,
  pubKeyWithHash,
  tokenName,
  txOutRef,
  validatorHash,
  value,
 )
import Ledger.Generators qualified as LGen
import Ledger.Value qualified as Value
import PlutusTx qualified
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
import ArdanaDollar.Map.Types qualified as OnchainMap (
  Datum (..),
  Map (..),
  MapInstance (..),
  Node (..),
  Pointer (..),
  Redeemer (..),
  TokenRedeemer (..),
 )
import ArdanaDollar.PriceOracle.OnChain qualified as PriceOracle (
  OracleMintingParams (..),
  OracleValidatorParams (..),
  PriceTracking (..),
 )
import ArdanaDollar.Treasury.Types qualified as Treasury (
  NewContract (..),
  Treasury (..),
  TreasuryAction (..),
  TreasuryDepositParams (..),
  TreasurySpendParams (..),
  TreasuryState (..),
  TreasuryStateTokenParams (..),
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

oracleMintingParams :: forall (m :: Type -> Type). MonadGen m => m PriceOracle.OracleMintingParams
oracleMintingParams = uncurry PriceOracle.OracleMintingParams <$> pubKeyWithHash

oracleValidatorParams :: forall (m :: Type -> Type). MonadGen m => m PriceOracle.OracleValidatorParams
oracleValidatorParams = do
  cs <- currencySymbol
  (pk, pkh) <- pubKeyWithHash
  pc <- peggedCurrency
  pure $
    PriceOracle.OracleValidatorParams
      { PriceOracle.oracleValidatorParams'oracleMintingCurrencySymbol = cs
      , PriceOracle.oracleValidatorParams'operator = pk
      , PriceOracle.oracleValidatorParams'operatorPkh = pkh
      , PriceOracle.oracleValidatorParams'peggedCurrency = pc
      }

peggedCurrency :: forall (m :: Type -> Type). MonadGen m => m P.BuiltinByteString
peggedCurrency =
  Gen.frequency
    [ (16, pure "USD")
    , (2, pure "EUR")
    , (2, pure "GBP")
    , (1, pure "PLN")
    , (1, builtinByteString (Range.singleton 3))
    ]

priceTracking :: forall (m :: Type -> Type). MonadGen m => m PriceOracle.PriceTracking
priceTracking =
  PriceOracle.PriceTracking
    <$> uniqueMap (Range.linear 0 10) (builtinByteString (Range.constant 0 128)) integer
    <*> uniqueMap (Range.linear 0 10) assetClass integer
    <*> (LGen.genSlotConfig >>= LGen.genPOSIXTime)

treasury :: forall (m :: Type -> Type). MonadGen m => m Treasury.Treasury
treasury =
  Treasury.Treasury <$> peggedCurrency
    <*> assetClass
    <*> assetClass
    <*> treasuryStateTokenParams

treasuryAction :: forall (m :: Type -> Type). MonadGen m => m Treasury.TreasuryAction
treasuryAction =
  Gen.element
    [ Treasury.BorrowForAuction
    , Treasury.RefreshAct
    , Treasury.UpdateAct
    ]

treasuryStateTokenParams :: forall (m :: Type -> Type). MonadGen m => m Treasury.TreasuryStateTokenParams
treasuryStateTokenParams = Treasury.TreasuryStateTokenParams <$> tokenName <*> txOutRef

treasuryCostCenters :: forall (m :: Type -> Type). MonadGen m => m (UniqueMap.Map P.BuiltinByteString Value.Value)
treasuryCostCenters = uniqueMap (Range.linear 0 10) (builtinByteString (Range.constant 0 128)) positiveValue

treasuryState :: forall (m :: Type -> Type). MonadGen m => m Treasury.TreasuryState
treasuryState =
  Treasury.TreasuryState
    <$> natural
    <*> natural
    <*> (LGen.genSlotConfig >>= LGen.genPOSIXTime)
    <*> (LGen.genSlotConfig >>= LGen.genPOSIXTime)
    <*> (LGen.genSlotConfig >>= LGen.genPOSIXTime)
    <*> (LGen.genSlotConfig >>= LGen.genPOSIXTime)
    <*> assetClass
    <*> assetClass
    <*> pure (PlutusTx.toBuiltinData ())

treasuryDepositParams :: forall (m :: Type -> Type). MonadGen m => m Treasury.TreasuryDepositParams
treasuryDepositParams =
  Treasury.TreasuryDepositParams <$> value <*> builtinByteString (Range.constant 0 128)

treasurySpendParams :: forall (m :: Type -> Type). MonadGen m => m Treasury.TreasurySpendParams
treasurySpendParams =
  Treasury.TreasurySpendParams
    <$> value
    <*> builtinByteString (Range.constant 0 128)
    <*> address

onchainMapMapInstance :: forall (m :: Type -> Type). MonadGen m => m OnchainMap.MapInstance
onchainMapMapInstance = OnchainMap.MapInstance <$> assetClass

onchainMapPointer :: forall (m :: Type -> Type). MonadGen m => m OnchainMap.Pointer
onchainMapPointer = OnchainMap.Pointer <$> assetClass

onchainMapMap :: forall (m :: Type -> Type). MonadGen m => m OnchainMap.Map
onchainMapMap = OnchainMap.Map <$> Gen.maybe onchainMapPointer

onchainMapNode :: forall (m :: Type -> Type). MonadGen m => m (OnchainMap.Node Integer Integer)
onchainMapNode = OnchainMap.Node <$> integer <*> integer <*> Gen.maybe onchainMapPointer

onchainMapDatum :: forall (m :: Type -> Type). MonadGen m => m (OnchainMap.Datum Integer Integer)
onchainMapDatum =
  Gen.choice
    [ OnchainMap.MapDatum <$> onchainMapMap
    , OnchainMap.NodeDatum <$> onchainMapNode
    ]

onchainRedeemer :: forall (m :: Type -> Type). MonadGen m => m OnchainMap.Redeemer
onchainRedeemer = Gen.element [OnchainMap.Use, OnchainMap.ListOp]

onchainTokenRedeemer :: forall (m :: Type -> Type). MonadGen m => m OnchainMap.TokenRedeemer
onchainTokenRedeemer =
  Gen.choice
    [ pure OnchainMap.AddToEmptyMap
    , pure OnchainMap.AddSmallest
    , OnchainMap.AddInTheMiddle <$> txOutRef
    , OnchainMap.AddGreatest <$> txOutRef
    , pure OnchainMap.RemoveFromOneElementMap
    , pure OnchainMap.RemoveSmallest
    , OnchainMap.RemoveInTheMiddle <$> txOutRef
    , OnchainMap.RemoveGreatest <$> txOutRef
    ]

newContract :: forall (m :: Type -> Type). MonadGen m => m Treasury.NewContract
newContract = Treasury.NewContract <$> validatorHash

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
