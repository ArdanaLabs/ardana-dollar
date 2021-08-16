{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module ArdanaDollar.Treasury.OffChain (
  startTreasury,
  depositFundsWithCostCenter,
  spendFromCostCenter,
  queryCostCenters,
  initiateUpgrade,
  treasuryAddress,
  treasuryInst,
  treasuryValidator,
  findTreasury,
) where

--------------------------------------------------------------------------------

import Control.Monad (void)
import Data.Kind (Type)
import Data.Map qualified as Map
import Data.Row (Row)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Text.Printf (printf)
import Prelude (Semigroup (..), String, show)

--------------------------------------------------------------------------------

import Ledger qualified
import Ledger.Constraints qualified as Constraints
import Ledger.Typed.Scripts qualified as Scripts
import Ledger.Value qualified as Value
import Plutus.Contract
import Plutus.Contracts.Currency qualified as Currency
import Plutus.V1.Ledger.Contexts qualified as Contexts
import PlutusTx qualified
import PlutusTx.Prelude hiding (Semigroup (..))

--------------------------------------------------------------------------------

import ArdanaDollar.Treasury.OnChain (mkTreasuryValidator)
import ArdanaDollar.Treasury.Types
import ArdanaDollar.Vault (dUSDAsset, getDatumOffChain)
import Plutus.PAB.OutputBus

data Treasuring
instance Scripts.ValidatorTypes Treasuring where
  type DatumType Treasuring = TreasuryDatum
  type RedeemerType Treasuring = TreasuryAction

treasuryInst :: Treasury -> Value.AssetClass -> Scripts.TypedValidator Treasuring
treasuryInst t dac =
  Scripts.mkTypedValidator @Treasuring
    ( $$(PlutusTx.compile [||mkTreasuryValidator||])
        `PlutusTx.applyCode` PlutusTx.liftCode t
        `PlutusTx.applyCode` PlutusTx.liftCode dac
        `PlutusTx.applyCode` PlutusTx.liftCode dUSDAsset
    )
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.wrapValidator @TreasuryDatum @TreasuryAction

treasuryValidator :: Treasury -> Value.AssetClass -> Ledger.Validator
treasuryValidator t dac = Scripts.validatorScript $ treasuryInst t dac

treasuryAddress :: Treasury -> Value.AssetClass -> Ledger.Address
treasuryAddress t dac = Ledger.scriptAddress $ treasuryValidator t dac

findTreasury ::
  forall (w :: Type) (s :: Row Type) (e :: Type).
  (AsContractError e) =>
  Treasury ->
  Contract w s e (Maybe (Contexts.TxOutRef, Ledger.TxOutTx, TreasuryDatum))
findTreasury treasury = do
  utxos <- Map.filter f <$> utxoAt (treasuryAddress treasury danaAssetClass)
  logInfo @String $ printf "Found UTXOs:\n%s" (show utxos)
  return $ case Map.toList utxos of
    [(oref, o)] -> do
      datum <- getDatumOffChain o
      return (oref, o, datum)
    _ -> Nothing
  where
    f :: Ledger.TxOutTx -> Bool
    f o = Value.assetClassValueOf (Ledger.txOutValue $ Ledger.txOutTxOut o) (tSymbol treasury) == 1

-- Treasury start contract
startTreasury :: forall (w :: Type). Contract w EmptySchema ContractError Treasury
startTreasury = do
  pkh <- Contexts.pubKeyHash <$> ownPubKey
  cs <-
    fmap Currency.currencySymbol $
      mapError (\(Currency.CurContractError e) -> e) $
        Currency.mintContract pkh [(treasuryTokenName, 1)]
  let ac = Value.assetClass cs treasuryTokenName
      treasury = Treasury ac
      td = TreasuryDatum {auctionDanaAmount = 10}
      treasuryValue = Value.assetClassValue ac 1 <> Value.assetClassValue danaAssetClass 10
      lookups =
        Constraints.mintingPolicy danaMintingPolicy
          <> Constraints.typedValidatorLookups (treasuryInst treasury danaAssetClass)
          <> Constraints.otherScript (treasuryValidator treasury danaAssetClass)
      tx =
        Constraints.mustPayToTheScript td treasuryValue
          <> Constraints.mustMintValue (Value.assetClassValue danaAssetClass 10)

  ledgerTx <- submitTxConstraintsWith lookups tx
  void $ awaitTxConfirmed $ Ledger.txId ledgerTx
  logInfo @String $ printf "initialized Treasury %s" (show td)
  return treasury

-- Treasury usage contracts
depositFundsWithCostCenter ::
  forall (w :: Type) (s :: Row Type) (e :: Type).
  TreasuryDepositParams ->
  Contract w s e ()
depositFundsWithCostCenter params = do
  logInfo ("called depositFundsWithCostCenter" :: ByteString)
  logInfo (show params)

spendFromCostCenter :: forall (w :: Type) (s :: Row Type) (e :: Type). TreasurySpendParams -> Contract w s e ()
spendFromCostCenter params = do
  logInfo ("called spendFromCostCenter" :: ByteString)
  logInfo (show params)

queryCostCenters :: forall (s :: Row Type) (e :: Type). Contract (OutputBus (Vector (ByteString, Value.Value))) s e ()
queryCostCenters = do
  logInfo ("called queryCostCenters" :: ByteString)
  sendBus Vector.empty

initiateUpgrade :: forall (w :: Type) (s :: Row Type) (e :: Type). Contract w s e ()
initiateUpgrade = logInfo ("called initiateUpgrade" :: ByteString)
