{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module ArdanaDollar.Treasury.Types (
  Treasury (..),
  TreasuryDatum (..),
  TreasuryAction (..),
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

-- TODO: Should the Redeemer give more information?
data TreasuryAction = BorrowForAuction
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
PlutusTx.makeIsDataIndexed ''TreasuryAction [('BorrowForAuction, 0)]
PlutusTx.makeLift ''TreasuryAction
