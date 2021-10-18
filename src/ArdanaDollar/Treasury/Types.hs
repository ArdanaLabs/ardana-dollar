{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-specialize #-}

module ArdanaDollar.Treasury.Types (
  Treasuring,
  Treasury (..),
  TreasuryStateTokenParams (..),
  TreasuryUpgradeContractTokenParams (..),
  TreasuryDatum (..),
  TreasuryAction (..),
  TreasuryDepositParams (..),
  TreasurySpendParams (..),
  TreasurySpendEndpointParams (..),
  NewContract (..),
  prepareTreasurySpendParams,
  isInitialDatum,
  calculateCostCenterValueOf,
  danaAssetClass,
  danaCurrency,
  danaTokenName,
  danaMintingPolicy,
) where

--------------------------------------------------------------------------------

import Data.Functor ((<&>))
import Data.Kind (Type)
import Data.Text (Text)
import GHC.Generics (Generic)
import Prelude qualified

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi.Schema qualified as OpenApi

--------------------------------------------------------------------------------

import Ledger qualified
import Ledger.Typed.Scripts qualified as Scripts
import Ledger.Value qualified as Value
import Plutus.V1.Ledger.Address (pubKeyHashAddress, scriptHashAddress)
import Plutus.V1.Ledger.Contexts qualified as Contexts
import PlutusTx qualified
import PlutusTx.Prelude
import PlutusTx.UniqueMap qualified as UniqueMap
import Schema (ToSchema)

--------------------------------------------------------------------------------

data Treasuring
instance Scripts.ValidatorTypes Treasuring where
  type DatumType Treasuring = TreasuryDatum
  type RedeemerType Treasuring = TreasuryAction

data Treasury = Treasury
  { treasury'peggedCurrency :: BuiltinByteString
  , treasury'stateTokenSymbol :: Value.AssetClass
  , treasury'stateTokenParams :: TreasuryStateTokenParams
  , treasury'upgradeTokenSymbol :: Value.AssetClass
  , treasury'upgradeTokenParams :: TreasuryUpgradeContractTokenParams
  }
  deriving stock (Prelude.Eq, Generic, Prelude.Show)
  deriving anyclass (FromJSON, ToJSON, OpenApi.ToSchema)

data TreasuryStateTokenParams = TreasuryStateTokenParams
  { stateToken :: !Value.TokenName
  , initialOutput :: !Contexts.TxOutRef
  }
  deriving stock (Prelude.Eq, Generic, Prelude.Show)
  deriving anyclass (FromJSON, ToJSON, OpenApi.ToSchema)

data TreasuryUpgradeContractTokenParams = TreasuryUpgradeContractTokenParams
  { upgradeToken'initialOwner :: !Ledger.PubKeyHash
  , upgradeToken'peggedCurrency :: !BuiltinByteString
  , upgradeToken'initialOutput :: !Contexts.TxOutRef
  }
  deriving stock (Prelude.Eq, Generic, Prelude.Show)
  deriving anyclass (FromJSON, ToJSON, OpenApi.ToSchema)

data TreasuryDatum = TreasuryDatum
  { auctionDanaAmount :: !Integer
  , currentContract :: !Ledger.ValidatorHash
  , costCenters :: !(UniqueMap.Map BuiltinByteString Value.Value)
  }
  deriving stock (Prelude.Eq, Generic, Prelude.Show)
  deriving anyclass (FromJSON, ToJSON)

data TreasuryDepositParams = TreasuryDepositParams
  { treasuryDeposit'value :: !Value.Value
  , treasuryDeposit'costCenter :: !BuiltinByteString
  }
  deriving stock (Prelude.Eq, Generic, Prelude.Show)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data TreasurySpendParams = TreasurySpendParams
  { treasurySpend'value :: !Value.Value
  , treasurySpend'costCenter :: !BuiltinByteString
  , treasurySpend'beneficiary :: !Ledger.Address
  }
  deriving stock (Prelude.Eq, Generic, Prelude.Show)
  deriving anyclass (FromJSON, ToJSON)

-- TODO: For now, there is no `ToSchema` instance for coproduct
-- so to make the endpoint usable we use two Maybes instead of
-- a single Either-like type
data TreasurySpendEndpointParams d = TreasurySpendEndpointParams
  { treasurySpendEndpoint'value :: !Value.Value
  , treasurySpendEndpoint'costCenter :: !BuiltinByteString
  , treasurySpendEndpoint'pubKey :: !(Maybe Ledger.PubKeyHash)
  , treasurySpendEndpoint'validator :: !(Maybe (Ledger.ValidatorHash, d))
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data TreasuryAction
  = BorrowForAuction
  | DepositFundsWithCostCenter !TreasuryDepositParams
  | SpendFundsFromCostCenter !TreasurySpendParams
  | AllowMint !Value.AssetClass
  | AllowBurn
  | InitiateUpgrade !NewContract
  deriving stock (Prelude.Eq, Prelude.Show)

newtype NewContract = NewContract {unNewContract :: Ledger.ValidatorHash}
  deriving stock (Generic)
  deriving newtype (Prelude.Eq, Prelude.Ord, Eq, Ord, PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData, Prelude.Show)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

-- instances
instance Eq TreasuryDatum where
  (TreasuryDatum ada1 cc1 ccs1) == (TreasuryDatum ada2 cc2 ccs2) =
    ada1 == ada2 && cc1 == cc2 && ccs1 == ccs2

-- helper functions
prepareTreasurySpendParams ::
  forall (d :: Type) (i :: Type) (o :: Type).
  (PlutusTx.ToData d) =>
  TreasurySpendEndpointParams d ->
  Either Text (TreasurySpendParams, Value.Value -> TxConstraints i o)
prepareTreasurySpendParams params =
  onChainAddress <&> \(oca, d) ->
    ( TreasurySpendParams
        { treasurySpend'value = treasurySpendEndpoint'value params
        , treasurySpend'costCenter = treasurySpendEndpoint'costCenter params
        , treasurySpend'beneficiary = oca
        }
    , d
    )
  where
    pubKey :: Maybe Ledger.PubKeyHash
    pubKey = treasurySpendEndpoint'pubKey params

    validatorHash :: Maybe (Ledger.ValidatorHash, d)
    validatorHash = treasurySpendEndpoint'validator params

    onChainAddress ::
      Either Text (Ledger.Address, Value.Value -> TxConstraints i o)
    onChainAddress = case (pubKey, validatorHash) of
      (Just pk, Nothing) ->
        Right (pubKeyHashAddress pk, Constraints.mustPayToPubKey pk)
      (Nothing, Just (vh, d)) ->
        Right
          ( scriptHashAddress vh
          , Constraints.mustPayToOtherScript
              vh
              (Ledger.Datum $ PlutusTx.toBuiltinData d)
          )
      (Just _, Just _) -> Left "two beneficiary addresses supplied"
      (Nothing, Nothing) -> Left "no beneficiary address supplied"

{-# INLINEABLE isInitialDatum #-}
isInitialDatum :: TreasuryDatum -> Bool
isInitialDatum td = UniqueMap.null (costCenters td)

{-# INLINEABLE calculateCostCenterValueOf #-}
calculateCostCenterValueOf :: Value.AssetClass -> TreasuryDatum -> Integer
calculateCostCenterValueOf ac TreasuryDatum {costCenters = cc} =
  foldr (\el acc -> (el `Value.assetClassValueOf` ac) + acc) 0 (UniqueMap.elems cc)

-- helper currencies (a bit debugable)
{-# INLINEABLE danaTokenName #-}
danaTokenName :: Value.TokenName
danaTokenName = Value.TokenName "DANA"

{-# INLINEABLE mkDanaMintingPolicy #-}
mkDanaMintingPolicy :: Value.TokenName -> () -> Contexts.ScriptContext -> Bool
mkDanaMintingPolicy danaToken _ sc =
  traceIfFalse "DANA isn't minted" $ danaToken `elem` tokenList
  where
    valuesFromCtx = Value.flattenValue . Ledger.txInfoMint . Ledger.scriptContextTxInfo
    tokenList = (\(_, tokenName, _) -> tokenName) <$> valuesFromCtx sc

{-# INLINEABLE danaMintingPolicy #-}
danaMintingPolicy :: Ledger.MintingPolicy
danaMintingPolicy =
  Ledger.mkMintingPolicyScript $
    $$(PlutusTx.compile [||Scripts.wrapMintingPolicy . mkDanaMintingPolicy||])
      `PlutusTx.applyCode` PlutusTx.liftCode danaTokenName

{-# INLINEABLE danaCurrency #-}
danaCurrency :: Value.CurrencySymbol
danaCurrency = Ledger.scriptCurrencySymbol danaMintingPolicy

{-# INLINEABLE danaAssetClass #-}
danaAssetClass :: Value.AssetClass
danaAssetClass = Value.AssetClass (danaCurrency, danaTokenName)

PlutusTx.makeLift ''Treasury
PlutusTx.makeIsDataIndexed ''TreasuryDatum [('TreasuryDatum, 0)]
PlutusTx.makeLift ''TreasuryDatum
PlutusTx.makeLift ''TreasuryStateTokenParams
PlutusTx.makeLift ''TreasuryUpgradeContractTokenParams
PlutusTx.makeIsDataIndexed ''TreasuryDepositParams [('TreasuryDepositParams, 0)]
PlutusTx.makeLift ''TreasuryDepositParams
PlutusTx.makeIsDataIndexed ''TreasurySpendParams [('TreasurySpendParams, 0)]
PlutusTx.makeLift ''TreasurySpendParams
PlutusTx.makeIsDataIndexed
  ''TreasuryAction
  [ ('BorrowForAuction, 0)
  , ('DepositFundsWithCostCenter, 1)
  , ('SpendFundsFromCostCenter, 2)
  , ('AllowMint, 3)
  , ('AllowBurn, 4)
  , ('InitiateUpgrade, 5)
  ]
PlutusTx.makeLift ''TreasuryAction
PlutusTx.makeLift ''NewContract
