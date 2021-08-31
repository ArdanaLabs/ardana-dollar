{-# LANGUAGE TemplateHaskell #-}

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
import Ledger.Crypto qualified as Crypto
import Ledger.Typed.Scripts qualified as Scripts
import Ledger.Value qualified as Value
import Plutus.Contract
import Plutus.V1.Ledger.Contexts qualified as Contexts
import PlutusTx qualified
import PlutusTx.Data.Extra (toRedeemer)
import PlutusTx.Prelude hiding (Semigroup (..))
import PlutusTx.UniqueMap qualified as UniqueMap

--------------------------------------------------------------------------------

import ArdanaDollar.Treasury.OnChain (mkTreasuryValidator)
import ArdanaDollar.Treasury.StateToken (
  treasuryStateTokenAssetClass,
  treasuryStateTokenMintingConstraints,
  treasuryStateTokenParams,
 )
import ArdanaDollar.Treasury.Types
import ArdanaDollar.Utils (datumForOffchain)
import Plutus.PAB.OutputBus

treasuryInst :: Treasury -> Scripts.TypedValidator Treasuring
treasuryInst t =
  Scripts.mkTypedValidator @Treasuring
    ( $$(PlutusTx.compile [||mkTreasuryValidator||])
        `PlutusTx.applyCode` PlutusTx.liftCode t
    )
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.wrapValidator @TreasuryDatum @TreasuryAction

treasuryValidator :: Treasury -> Ledger.Validator
treasuryValidator = Scripts.validatorScript . treasuryInst

treasuryAddress :: Treasury -> Ledger.Address
treasuryAddress = Ledger.scriptAddress . treasuryValidator

findTreasury ::
  forall (w :: Type) (s :: Row Type) (e :: Type).
  (AsContractError e) =>
  Treasury ->
  Contract w s e (Maybe (Contexts.TxOutRef, Ledger.TxOutTx, TreasuryDatum))
findTreasury treasury = do
  utxos <- Map.filter f <$> utxoAt (treasuryAddress treasury)
  return $ case Map.toList utxos of
    [(oref, o)] -> do
      datum <- datumForOffchain o
      return (oref, o, datum)
    _ -> Nothing
  where
    tokenAC :: Value.AssetClass
    tokenAC = stateTokenSymbol treasury

    f :: Ledger.TxOutTx -> Bool
    f o = Value.assetClassValueOf (Ledger.txOutValue $ Ledger.txOutTxOut o) tokenAC == 1

-- Treasury start contract
startTreasury ::
  forall (w :: Type).
  Contract w EmptySchema ContractError (Maybe Treasury)
startTreasury = do
  pka <- Ledger.pubKeyAddress <$> ownPubKey
  utxos <- Map.toList <$> utxoAt pka
  case utxos of
    [] -> logError @String "No UTXO found at public address" >> return Nothing
    (oref, o) : _ -> do
      let stParams = treasuryStateTokenParams oref
          treasury =
            Treasury
              { stateTokenParams = stParams
              , stateTokenSymbol = treasuryStateTokenAssetClass stParams
              }
          (tokenLookup, tokenTx, tokenValue) = treasuryStateTokenMintingConstraints stParams
          td = TreasuryDatum {auctionDanaAmount = 10, costCenters = UniqueMap.empty}
          treasuryValue = tokenValue <> Value.assetClassValue danaAssetClass 10
      let lookups =
            tokenLookup
              <> Constraints.mintingPolicy danaMintingPolicy
              <> Constraints.typedValidatorLookups (treasuryInst treasury)
              <> Constraints.otherScript (treasuryValidator treasury)
              <> Constraints.unspentOutputs (Map.singleton oref o)
          tx =
            tokenTx
              <> Constraints.mustPayToTheScript td treasuryValue
              <> Constraints.mustMintValue (Value.assetClassValue danaAssetClass 10)
              <> Constraints.mustSpendPubKeyOutput oref

      ledgerTx <- submitTxConstraintsWith lookups tx
      void $ awaitTxConfirmed $ Ledger.txId ledgerTx
      logInfo @String $ printf "initialized Treasury %s" (show td)
      return $ Just treasury

-- Treasury usage contracts
depositFundsWithCostCenter ::
  forall (w :: Type) (s :: Row Type) (e :: Type).
  (AsContractError e) =>
  Treasury ->
  TreasuryDepositParams ->
  Contract w s e ()
depositFundsWithCostCenter treasury params = do
  pkh <- Crypto.pubKeyHash <$> ownPubKey
  logInfo ("called depositFundsWithCostCenter with params: " ++ show params)
  let ac = treasuryDepositCurrency params
      amount = treasuryDepositAmount params
      centerName = treasuryDepositCostCenter params
      depositValue = Value.assetClassValue ac amount
  findTreasury treasury >>= \case
    Nothing -> logError @String "no treasury utxo at the script address"
    Just (oref, o, td) -> do
      let td' = td {costCenters = UniqueMap.alter centerName (Just depositValue <>) (costCenters td)}
          scriptValue = Ledger.txOutValue (Ledger.txOutTxOut o) <> depositValue
          lookups =
            Constraints.typedValidatorLookups (treasuryInst treasury)
              <> Constraints.otherScript (treasuryValidator treasury)
              <> Constraints.unspentOutputs (Map.singleton oref o)
          tx =
            Constraints.mustPayToTheScript td' scriptValue
              <> Constraints.mustSpendScriptOutput
                oref
                (toRedeemer @Treasuring $ DepositFundsWithCostCenter params)
      ledgerTx <- submitTxConstraintsWith lookups tx
      awaitTxConfirmed $ Ledger.txId ledgerTx
      logInfo @String $ printf "User %s has deposited %s %s" (show pkh) (show amount) (show ac)

spendFromCostCenter ::
  forall (w :: Type) (s :: Row Type) (e :: Type).
  TreasurySpendParams ->
  Contract w s e ()
spendFromCostCenter params = do
  logInfo @String "called spendFromCostCenter"
  logInfo (show params)

queryCostCenters ::
  forall (s :: Row Type) (e :: Type).
  (AsContractError e) =>
  Treasury ->
  Contract (OutputBus (Vector (BuiltinByteString, Value.Value))) s e ()
queryCostCenters treasury = do
  logInfo @String "called queryCostCenters"
  findTreasury treasury >>= \case
    Nothing -> logError @String "no treasury utxo at the script address"
    Just (_, _, td) ->
      sendBus . Vector.fromList . UniqueMap.toList $ costCenters td

initiateUpgrade ::
  forall (w :: Type) (s :: Row Type) (e :: Type).
  Contract w s e ()
initiateUpgrade = logInfo @String "called initiateUpgrade"
