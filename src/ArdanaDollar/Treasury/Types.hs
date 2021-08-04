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
  , dusdAssetClass :: Value.AssetClass
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)
PlutusTx.makeLift ''Treasury

data TreasuryDatum = TreasuryDatum
  { currentDebtAuctionPrice :: !Integer
  , currentSurplusAuctionPrice :: !Integer
  , auctionDanaAmount :: !Integer
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)
PlutusTx.makeIsDataIndexed ''TreasuryDatum [('TreasuryDatum, 0)]

data TreasuryAction = MkDebtBid Integer | MkSurplusBid Integer deriving (Show)
PlutusTx.makeIsDataIndexed
  ''TreasuryAction
  [ ('MkDebtBid, 0)
  , ('MkSurplusBid, 1)
  ]
PlutusTx.makeLift ''TreasuryAction

-- instances
instance Eq TreasuryDatum where
  (TreasuryDatum cdap1 csap1 ada1) == (TreasuryDatum cdap2 csap2 ada2)
    | cdap1 == cdap2 && csap1 == csap2 && ada1 == ada2 = True
    | otherwise = False

-- helper currencies
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
