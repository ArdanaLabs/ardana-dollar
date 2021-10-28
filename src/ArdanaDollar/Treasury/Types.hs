{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-specialize #-}

module ArdanaDollar.Treasury.Types (
  Treasuring,
  Treasury (..),
  TreasuryStateTokenParams (..),
  TreasuryState (..),
  TreasuryAction (..),
  TreasuryDepositParams (..),
  TreasurySpendParams (..),
  TreasurySpendEndpointParams (..),
  NewContract (..),
  prepareTreasurySpendParams,
  isInitialDatum,
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

import Ledger (POSIXTime)
import Ledger qualified
import Ledger.Constraints (TxConstraints)
import Ledger.Constraints qualified as Constraints
import Ledger.Typed.Scripts qualified as Scripts
import Ledger.Value qualified as Value
import Plutus.V1.Ledger.Address (pubKeyHashAddress, scriptHashAddress)
import Plutus.V1.Ledger.Contexts qualified as Contexts
import PlutusTx qualified
import PlutusTx.Natural (Natural)
import PlutusTx.Prelude
import Schema (ToSchema)

--------------------------------------------------------------------------------

data Treasuring
instance Scripts.ValidatorTypes Treasuring where
  type DatumType Treasuring = TreasuryState
  type RedeemerType Treasuring = TreasuryAction

data Treasury = Treasury
  { treasury'peggedCurrency :: BuiltinByteString
  , treasury'danaAssetClass :: Value.AssetClass
  , treasury'stateTokenAssetClass :: Value.AssetClass
  , treasury'stateTokenParams :: TreasuryStateTokenParams
  }
  deriving stock (Prelude.Eq, Generic, Prelude.Show)
  deriving anyclass (FromJSON, ToJSON, OpenApi.ToSchema)

data TreasuryStateTokenParams = TreasuryStateTokenParams
  { stateTokenName :: !Value.TokenName
  , oneShotUtxo :: !Contexts.TxOutRef
  }
  deriving stock (Prelude.Eq, Generic, Prelude.Show)
  deriving anyclass (FromJSON, ToJSON, OpenApi.ToSchema)

data TreasuryState = TreasuryState
  { certTokenStart :: Natural
  , certTokenBase :: Natural
  , certTokenExpiration :: POSIXTime
  , refreshInterval :: POSIXTime
  , validRangeSize :: POSIXTime
  , timestamp :: POSIXTime
  , ownerAuthToken :: Value.AssetClass
  , dUSDPermissionToken :: Value.AssetClass
  , -- This is useful if the minting policy for `ownerAuthToken`
    -- wants to store extra data in the state.
    extra :: PlutusTx.BuiltinData
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
  | RefreshAct
  | UpdateAct
  deriving stock (Prelude.Eq, Prelude.Show)

newtype NewContract = NewContract {unNewContract :: Ledger.ValidatorHash}
  deriving stock (Generic)
  deriving newtype (Prelude.Eq, Prelude.Ord, Eq, Ord, PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData, Prelude.Show)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

-- instances
instance Eq TreasuryState where
  ts1 == ts2 =
    (certTokenStart ts1 == certTokenStart ts2)
      && (certTokenBase ts1 == certTokenBase ts2)
      && (certTokenExpiration ts1 == certTokenExpiration ts2)
      && (refreshInterval ts1 == refreshInterval ts2)
      && (validRangeSize ts1 == validRangeSize ts2)
      && (timestamp ts1 == timestamp ts2)
      && (ownerAuthToken ts1 == ownerAuthToken ts2)
      && (dUSDPermissionToken ts1 == dUSDPermissionToken ts2)
      && (extra ts1 == extra ts2)

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
isInitialDatum :: TreasuryState -> Bool
isInitialDatum _ts = True -- TODO: when is it initial?

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
PlutusTx.makeIsDataIndexed ''TreasuryState [('TreasuryState, 0)]
PlutusTx.makeLift ''TreasuryState
PlutusTx.makeLift ''TreasuryStateTokenParams
PlutusTx.makeIsDataIndexed ''TreasuryDepositParams [('TreasuryDepositParams, 0)]
PlutusTx.makeLift ''TreasuryDepositParams
PlutusTx.makeIsDataIndexed ''TreasurySpendParams [('TreasurySpendParams, 0)]
PlutusTx.makeLift ''TreasurySpendParams
PlutusTx.makeIsDataIndexed
  ''TreasuryAction
  [ ('BorrowForAuction, 0)
  , ('RefreshAct, 1)
  , ('UpdateAct, 2)
  ]
PlutusTx.makeLift ''TreasuryAction
PlutusTx.makeLift ''NewContract
