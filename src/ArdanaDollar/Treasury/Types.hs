{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module ArdanaDollar.Treasury.Types (
  Treasury (..),
  TreasuryDatum (..),
  TreasuryAction (..),
  TreasuryDepositParams (..),
  TreasurySpendParams (..),
  danaAssetClass,
  danaCurrency,
  danaTokenName,
  danaMintingPolicy,
  treasuryTokenName,
) where

--------------------------------------------------------------------------------

import GHC.Generics (Generic)
import Prelude (Show)

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

data Treasury = Treasury
  { tSymbol :: Value.AssetClass
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data TreasuryDatum = TreasuryDatum
  { auctionDanaAmount :: !Integer
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data TreasuryDepositParams = TreasuryDepositParams
  { treasuryDepositAmount :: Integer
  , treasuryDepositCurrency :: Value.AssetClass
  , treasuryDepositCostCenter :: ByteString
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
  | DepositFundsWithCostCenter
  | SpendFundsFromCostCenter
  | AllowMint
  | AllowBurn
  | InitiateUpgrade
  deriving (Show)

-- instances
instance Eq TreasuryDatum where
  (TreasuryDatum ada1) == (TreasuryDatum ada2)
    | ada1 == ada2 = True
    | otherwise = False

-- helper currencies (a bit debugable)
{-# INLINEABLE treasuryTokenName #-}
treasuryTokenName :: Value.TokenName
treasuryTokenName = Value.TokenName "treasury"

{-# INLINEABLE danaTokenName #-}
danaTokenName :: Value.TokenName
danaTokenName = Value.TokenName "DANA"

{-# INLINEABLE mkDanaMintingPolicy #-}
mkDanaMintingPolicy :: Value.TokenName -> () -> Contexts.ScriptContext -> Bool
mkDanaMintingPolicy danaToken _ sc = tokenList == [danaToken]
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
