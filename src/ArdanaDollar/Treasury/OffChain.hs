{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module ArdanaDollar.Treasury.OffChain (
  startTreasury,
  debtAuction,
  surplusAuction,
  depositAuctionFloat,
  upgrade,
) where

--------------------------------------------------------------------------------

import Control.Monad (void)
import Data.Kind (Type)
import Data.Map qualified as Map
import Data.Row (Row)
import Text.Printf (printf)
import Prelude (Semigroup (..), String, div, rem, show)

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
import ArdanaDollar.Vault (getDatumOffChain)

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
    )
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.wrapValidator @TreasuryDatum @TreasuryAction

treasuryValidator :: Treasury -> Value.AssetClass -> Ledger.Validator
treasuryValidator t dac = Scripts.validatorScript $ treasuryInst t dac

treasuryAddress :: Treasury -> Value.AssetClass -> Ledger.Address
treasuryAddress t dac = Ledger.scriptAddress $ treasuryValidator t dac

findTreasury ::
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
startTreasury :: forall (w :: Type). Value.AssetClass -> Contract w EmptySchema ContractError Treasury
startTreasury dac = do
  pkh <- Contexts.pubKeyHash <$> ownPubKey
  cs <-
    fmap Currency.currencySymbol $
      mapError (\(Currency.CurContractError e) -> e) $
        Currency.mintContract pkh [(treasuryTokenName, 1)]
  let ac = Value.assetClass cs treasuryTokenName
      treasury = Treasury ac dac
      td =
        TreasuryDatum
          { currentDebtAuctionPrice = 50
          , currentSurplusAuctionPrice = 50
          , auctionDanaAmount = 10
          }
      tx = Constraints.mustPayToTheScript td (Value.assetClassValue ac 1)
  ledgerTx <- submitTxConstraints (treasuryInst treasury danaAssetClass) tx
  void $ awaitTxConfirmed $ Ledger.txId ledgerTx
  logInfo @String $ printf "initialized Treasury %s" (show td)
  return treasury

-- Treasury usage contracts
debtAuction ::
  forall (w :: Type) (s :: Row Type) (e :: Type).
  (AsContractError e) =>
  Treasury ->
  Integer ->
  Contract w s e ()
debtAuction treasury danaAmount = do
  pkh <- Contexts.pubKeyHash <$> ownPubKey

  logInfo @String $ printf "User %s has called debtAuction" (show pkh)
  utxoWithDatum <- findTreasury treasury
  case utxoWithDatum of
    Nothing -> logError @String "no treasury utxo at the script address"
    Just (oref, o, td) -> do
      logInfo @String $ printf "Found treasury, it's datum:\n%s" (show td)
      let dusdPrice = currentDebtAuctionPrice td * danaAmount
          v =
            Ledger.txOutValue (Ledger.txOutTxOut o)
              <> Value.assetClassValue (dusdAssetClass treasury) dusdPrice
          danaValue = Value.assetClassValue danaAssetClass danaAmount
          td' = Ledger.Datum $ PlutusTx.toBuiltinData td

          lookups =
            Constraints.typedValidatorLookups (treasuryInst treasury danaAssetClass)
              <> Constraints.otherScript (treasuryValidator treasury danaAssetClass)
              <> Constraints.unspentOutputs (Map.singleton oref o)
              <> Constraints.mintingPolicy danaMintingPolicy
          tx =
            Constraints.mustSpendScriptOutput oref (Ledger.Redeemer . PlutusTx.toBuiltinData $ MkDebtBid danaAmount)
              <> Constraints.mustPayToOtherScript (Ledger.validatorHash $ treasuryValidator treasury danaAssetClass) td' v
              <> Constraints.mustMintValue danaValue
              <> Constraints.mustPayToPubKey pkh danaValue
      ledgerTx <- submitTxConstraintsWith lookups tx
      awaitTxConfirmed $ Ledger.txId ledgerTx
      logInfo @String $ printf "User %s has paid %s dUSD for %s DANA" (show pkh) (show dusdPrice) (show danaAmount)

surplusAuction ::
  forall (w :: Type) (s :: Row Type) (e :: Type).
  (AsContractError e) =>
  Treasury ->
  Integer ->
  Contract w s e ()
surplusAuction treasury dusdAmount = do
  pkh <- Contexts.pubKeyHash <$> ownPubKey
  logInfo @String $ printf "User %s has called surplusAuction " (show pkh)
  utxoWithDatum <- findTreasury treasury
  case utxoWithDatum of
    Nothing -> logError @String "no treasury utxo at the script address" -- throwError?
    Just (oref, o, td) -> do
      logInfo @String $ printf "Found treasury, it's datum:\n%s" (show td)
      let danaPrice = dusdAmount `div` currentSurplusAuctionPrice td -- [DANA], surplus is dUSD or DANA?
          danaRem = dusdAmount `rem` currentSurplusAuctionPrice td -- [DANA], should be 0
          td' = Ledger.Datum $ PlutusTx.toBuiltinData td
          userValue = Value.assetClassValue (dusdAssetClass treasury) dusdAmount
          scriptValue =
            Ledger.txOutValue (Ledger.txOutTxOut o)
              -- <> Value.assetClassValue danaAssetClass danaPrice
              <> Value.assetClassValue (dusdAssetClass treasury) (negate dusdAmount)
          scriptHash = Ledger.validatorHash $ treasuryValidator treasury danaAssetClass

          lookups =
            Constraints.typedValidatorLookups (treasuryInst treasury danaAssetClass)
              <> Constraints.otherScript (treasuryValidator treasury danaAssetClass)
              <> Constraints.unspentOutputs (Map.singleton oref o)
              <> Constraints.mintingPolicy danaMintingPolicy
          tx =
            Constraints.mustSpendScriptOutput oref (Ledger.Redeemer . PlutusTx.toBuiltinData $ MkSurplusBid dusdAmount)
              <> Constraints.mustPayToOtherScript scriptHash td' scriptValue
              <> Constraints.mustMintValue (Value.assetClassValue danaAssetClass $ negate danaPrice)
              <> Constraints.mustPayToPubKey pkh userValue

      if danaRem /= 0
        then logError @String $ printf "Cannot buy %s dUSD, it will require paying non-integer amount of DANA" (show dusdAmount)
        else do
          ledgerTx <- submitTxConstraintsWith lookups tx
          awaitTxConfirmed $ Ledger.txId ledgerTx
          logInfo @String $ printf "User %s has paid %s DANA for %s dUSD" (show pkh) (show danaPrice) (show dusdAmount)

depositAuctionFloat :: forall (w :: Type) (s :: Row Type) (e :: Type). Contract w s e ()
depositAuctionFloat = logInfo ("called depositAuctionFloat" :: ByteString)

upgrade :: forall (w :: Type) (s :: Row Type) (e :: Type). Contract w s e ()
upgrade = logInfo ("called depositAuctionFloat" :: ByteString)
