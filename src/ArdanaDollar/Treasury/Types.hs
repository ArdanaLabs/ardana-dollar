{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-specialize #-}

module ArdanaDollar.Treasury.Types (
  Treasuring,
  Treasury (..),
  TreasuryStateTokenParams (..),
  TreasuryDatum (..),
  TreasuryAction (..),
  TreasuryDepositParams (..),
  TreasurySpendParams (..),
  isInitialDatum,
  calculateCostCenterValueOf,
  danaAssetClass,
  danaCurrency,
  danaTokenName,
  danaMintingPolicy,
) where

--------------------------------------------------------------------------------

import GHC.Generics (Generic)
import Prelude (Show)
import Prelude qualified

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON, ToJSON)

--------------------------------------------------------------------------------

import Ledger qualified
import Ledger.Typed.Scripts qualified as Scripts
import Ledger.Value qualified as Value
import Plutus.V1.Ledger.Contexts qualified as Contexts
import PlutusTx qualified
import PlutusTx.Prelude
import Schema (ToSchema)

--------------------------------------------------------------------------------

import PlutusTx.UniqueMap qualified as UniqueMap

--------------------------------------------------------------------------------

data Treasuring
instance Scripts.ValidatorTypes Treasuring where
  type DatumType Treasuring = TreasuryDatum
  type RedeemerType Treasuring = TreasuryAction

data Treasury = Treasury
  { stateTokenSymbol :: Value.AssetClass
  , stateTokenParams :: TreasuryStateTokenParams
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data TreasuryStateTokenParams = TreasuryStateTokenParams
  { stateToken :: !Value.TokenName
  , initialOutput :: !Contexts.TxOutRef
  }
  deriving stock (Prelude.Eq, Generic, Prelude.Show)
  deriving anyclass (FromJSON, ToJSON)

data TreasuryDatum = TreasuryDatum
  { auctionDanaAmount :: !Integer
  , costCenters :: !(UniqueMap.Map ByteString Value.Value)
  }
  deriving stock (Show, Generic, Prelude.Eq)
  deriving anyclass (FromJSON, ToJSON)

data TreasuryDepositParams = TreasuryDepositParams
  { treasuryDepositAmount :: !Integer
  , treasuryDepositCurrency :: !Value.AssetClass
  , treasuryDepositCostCenter :: !ByteString
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data TreasurySpendParams = TreasurySpendParams
  { treasurySpendValue :: Value.Value
  , treasurySpendCostCenter :: ByteString
  , treasurySpendBeneficiary :: Ledger.PubKeyHash
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

-- TODO: Should the Redeemer give more information?
data TreasuryAction
  = BorrowForAuction
  | DepositFundsWithCostCenter TreasuryDepositParams
  | SpendFundsFromCostCenter ByteString
  | AllowMint
  | AllowBurn
  | InitiateUpgrade
  deriving (Show)

-- instances
instance Eq TreasuryDatum where
  (TreasuryDatum ada1 cc1) == (TreasuryDatum ada2 cc2)
    | ada1 == ada2 && cc1 == cc2 = True
    | otherwise = False

-- helper functions
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
    valuesFromCtx = Value.flattenValue . Ledger.txInfoForge . Ledger.scriptContextTxInfo
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
PlutusTx.makeIsDataIndexed ''TreasuryDepositParams [('TreasuryDepositParams, 0)]
PlutusTx.makeLift ''TreasuryDepositParams
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
