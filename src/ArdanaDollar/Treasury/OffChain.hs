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

import Control.Lens ((^.))
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

import ArdanaDollar.MockAdmin (adminValidator, findAdmin)
import ArdanaDollar.Treasury.CanSpendToken (
  canSpendTokenAssetClass,
  canSpendTokenMintingConstraints,
 )
import ArdanaDollar.Treasury.OnChain (mkTreasuryValidator)
import ArdanaDollar.Treasury.StateToken (
  treasuryStateTokenAssetClass,
  treasuryStateTokenMintingConstraints,
  treasuryStateTokenParams,
 )
import ArdanaDollar.Treasury.Types
import ArdanaDollar.Treasury.UpgradeContractToken (
  upgradeContractTokenAssetClass,
  upgradeContractTokenMintingConstraints,
 )
import ArdanaDollar.Utils (datumForOffchain)
import Plutus.PAB.OutputBus

treasuryInst :: Treasury -> Scripts.TypedValidator Treasuring
treasuryInst t =
  Scripts.mkTypedValidator @Treasuring
    ( $$(PlutusTx.compile [||mkTreasuryValidator||])
        `PlutusTx.applyCode` PlutusTx.liftCode t
        `PlutusTx.applyCode` PlutusTx.liftCode (canSpendTokenAssetClass t)
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
  Contract w s e (Maybe (Contexts.TxOutRef, Ledger.ChainIndexTxOut, TreasuryDatum))
findTreasury treasury = do
  utxos <- Map.filter f <$> utxosAt (treasuryAddress treasury)
  case Map.toList utxos of
    [(oref, o)] -> do
      datum <- datumForOffchain o
      return $ (,,) oref o <$> datum
    _ -> return Nothing
  where
    tokenAC :: Value.AssetClass
    tokenAC = treasury'stateTokenSymbol treasury

    f :: Ledger.ChainIndexTxOut -> Bool
    f o = Value.assetClassValueOf (o ^. Ledger.ciTxOutValue) tokenAC == 1

-- Treasury start contract
startTreasury ::
  forall (w :: Type).
  Ledger.ValidatorHash ->
  BuiltinByteString ->
  Contract w EmptySchema ContractError (Maybe Treasury)
startTreasury vh peggedCurrency = do
  pk <- ownPubKey
  let pka = Ledger.pubKeyAddress pk
      pkh = Ledger.pubKeyHash pk
  utxos <- Map.toList <$> utxosAt pka
  case utxos of
    [] -> logError @String "No UTXO found at public address" >> return Nothing
    (oref, o) : _ -> do
      let utParams =
            TreasuryUpgradeContractTokenParams
              { upgradeToken'initialOwner = pkh
              , upgradeToken'peggedCurrency = "d" <> peggedCurrency
              , upgradeToken'initialOutput = oref
              }
          (upgradeTokenLookup, upgradeTokenTx, upgradeTokenValue) = upgradeContractTokenMintingConstraints utParams

          stParams = treasuryStateTokenParams oref
          treasury =
            Treasury
              { treasury'peggedCurrency = peggedCurrency
              , treasury'stateTokenParams = stParams
              , treasury'stateTokenSymbol = treasuryStateTokenAssetClass stParams
              , treasury'upgradeTokenParams = utParams
              , treasury'upgradeTokenSymbol = upgradeContractTokenAssetClass utParams
              }
          (tokenLookup, tokenTx, tokenValue) = treasuryStateTokenMintingConstraints stParams

          td =
            TreasuryDatum
              { auctionDanaAmount = 10
              , currentContract = vh
              , costCenters = UniqueMap.empty
              }

          treasuryValue = tokenValue <> upgradeTokenValue <> Value.assetClassValue danaAssetClass 10
          adminValue = upgradeTokenValue
      let lookups =
            tokenLookup <> upgradeTokenLookup
              <> Constraints.mintingPolicy danaMintingPolicy
              <> Constraints.typedValidatorLookups (treasuryInst treasury)
              <> Constraints.otherScript (treasuryValidator treasury)
              <> Constraints.unspentOutputs (Map.singleton oref o)
          tx =
            tokenTx <> upgradeTokenTx
              <> Constraints.mustPayToTheScript td treasuryValue
              <> Constraints.mustPayToOtherScript vh (Ledger.Datum $ PlutusTx.toBuiltinData ()) adminValue
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
  logInfo ("called depositFundsWithCostCenter with params: " <> show params)
  let centerName = treasuryDeposit'costCenter params
      depositValue = treasuryDeposit'value params
  findTreasury treasury >>= \case
    Nothing -> logError @String "no treasury utxo at the script address"
    Just (oref, o, td) -> do
      let td' = td {costCenters = UniqueMap.alter centerName (Just depositValue <>) (costCenters td)}
          scriptValue = (o ^. Ledger.ciTxOutValue) <> depositValue
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
      logInfo @String $ printf "User %s has deposited:\n%s" (show pkh) (show depositValue)

spendFromCostCenter ::
  forall (d :: Type) (w :: Type) (s :: Row Type) (e :: Type).
  (AsContractError e, PlutusTx.ToData d) =>
  Treasury ->
  TreasurySpendEndpointParams d ->
  Contract w s e ()
spendFromCostCenter treasury params = case prepareTreasurySpendParams params of
  Left err -> logError err
  Right (tsp, beneficiaryTxCont) -> do
    logInfo @String ("called spendFromCostCenter: " <> show tsp)
    -- TODO: Debug ad-hoc mint of CanSpend
    pkh <- Crypto.pubKeyHash <$> ownPubKey
    canSpendValue <- debugMintCanSpend pkh treasury
    let centerName = treasurySpend'costCenter tsp
        spendValue = treasurySpend'value tsp
        beneficiary = treasurySpend'beneficiary tsp
    findTreasury treasury >>= \case
      Nothing -> logError @String "no treasury utxo at the script address"
      Just (oref, o, td) -> do
        let td' =
              td
                { costCenters =
                    UniqueMap.alter
                      centerName
                      (Just (inv spendValue) <>)
                      (costCenters td)
                }

            treasuryValue = (o ^. Ledger.ciTxOutValue) <> inv spendValue

            lookups =
              Constraints.typedValidatorLookups (treasuryInst treasury)
                <> Constraints.otherScript (treasuryValidator treasury)
                <> Constraints.unspentOutputs (Map.singleton oref o)
            tx =
              Constraints.mustPayToTheScript td' treasuryValue
                <> Constraints.mustSpendScriptOutput oref (toRedeemer @Treasuring $ SpendFundsFromCostCenter tsp)
                <> Constraints.mustPayToPubKey pkh canSpendValue
                <> beneficiaryTxCont spendValue
        ledgerTx <- submitTxConstraintsWith lookups tx
        awaitTxConfirmed $ Ledger.txId ledgerTx
        logInfo @String $
          printf
            "Spent %s from %s, sent it to %s"
            (show spendValue)
            (show centerName)
            (show beneficiary)

-- TODO: Debug ad-hoc mint of CanSpend
debugMintCanSpend ::
  forall (w :: Type) (s :: Row Type) (e :: Type).
  (AsContractError e) =>
  Crypto.PubKeyHash ->
  Treasury ->
  Contract w s e Value.Value
debugMintCanSpend pkh treasury =
  findTreasury treasury >>= \case
    Nothing -> logError @String "no treasury utxo at the script address" >> return mempty
    Just (oref, o, td) -> do
      let (canSpendLookup, canSpendTx, canSpendValue) = canSpendTokenMintingConstraints treasury
          mintLookups =
            canSpendLookup <> Constraints.typedValidatorLookups (treasuryInst treasury)
              <> Constraints.otherScript (treasuryValidator treasury)
              <> Constraints.unspentOutputs (Map.singleton oref o)
          mintTx =
            canSpendTx
              <> Constraints.mustPayToPubKey pkh canSpendValue
              <> Constraints.mustPayToTheScript td (o ^. Ledger.ciTxOutValue)
              <> Constraints.mustSpendScriptOutput
                oref
                (toRedeemer @Treasuring $ AllowMint (canSpendTokenAssetClass treasury))
      mintLedgerTx <- submitTxConstraintsWith mintLookups mintTx
      void $ awaitTxConfirmed $ Ledger.txId mintLedgerTx
      return canSpendValue

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
  (AsContractError e) =>
  Treasury ->
  NewContract ->
  Contract w s e ()
initiateUpgrade treasury nc@(NewContract newContract) = do
  logInfo @String "called initiateUpgrade"
  findTreasury treasury >>= \case
    Nothing -> logError @String "no treasury utxo at the script address"
    Just (oref, o, td@TreasuryDatum {currentContract = oldContract}) -> do
      logInfo @String ("found treasury datum: " <> show td)
      findAdmin treasury (Ledger.scriptHashAddress oldContract) >>= \case
        Nothing -> logError @String "no treasury utxo at the script address"
        Just (oldRef, oldO) -> do
          logInfo @String ("found oldContract at: " <> show oldContract)
          let td' = td {currentContract = newContract}
              singleUpgradeToken = Value.assetClassValue (treasury'upgradeTokenSymbol treasury) 1

              scriptValue = o ^. Ledger.ciTxOutValue
              oldContractValue = oldO ^. Ledger.ciTxOutValue <> inv singleUpgradeToken
              newContractValue = singleUpgradeToken

              lookups =
                Constraints.typedValidatorLookups (treasuryInst treasury)
                  <> Constraints.otherScript (treasuryValidator treasury)
                  <> Constraints.otherScript (adminValidator 2) -- TODO: NO MAGIC NUMBERS!
                  <> Constraints.unspentOutputs (Map.fromList [(oref, o), (oldRef, oldO)])
              tx =
                Constraints.mustPayToTheScript td' scriptValue
                  <> Constraints.mustPayToOtherScript oldContract (Ledger.Datum $ PlutusTx.toBuiltinData ()) oldContractValue
                  <> Constraints.mustPayToOtherScript newContract (Ledger.Datum $ PlutusTx.toBuiltinData ()) newContractValue
                  <> Constraints.mustSpendScriptOutput oref (toRedeemer @Treasuring $ InitiateUpgrade nc)
                  <> Constraints.mustSpendScriptOutput oldRef (Ledger.Redeemer $ PlutusTx.toBuiltinData ())
          ledgerTx <- submitTxConstraintsWith lookups tx
          awaitTxConfirmed $ Ledger.txId ledgerTx
          logInfo @String $
            printf
              "Treasury updated, contract changed from %s to %s"
              (show $ currentContract td)
              (show $ currentContract td')
