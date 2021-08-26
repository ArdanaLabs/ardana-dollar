{-# LANGUAGE MultiWayIf #-}

module ArdanaDollar.DanaStakePool.Contracts (
  initializeSystem,
  initializeUser,
  deposit,
  withdraw,
  provideRewards,
  distributeRewards,
  distributeRewardsUser,
  withdrawRewards,
  queryUser,
  querySelf,
) where

import Prelude

import Plutus.Contract
import PlutusTx
import PlutusTx.Numeric as Numeric
import PlutusTx.Prelude (emptyByteString, fold, mempty)

import Ledger qualified
import Ledger.Constraints qualified as Constraints
import Ledger.Value qualified as Value
import Plutus.Contracts.Currency qualified as Currency

import Control.Monad.Except
import Data.Kind
import Data.Map qualified as Map
import Data.Monoid (Last (Last))
import Data.Row
import Data.Text (Text, pack)

import ArdanaDollar.DanaStakePool.DanaCurrency
import ArdanaDollar.DanaStakePool.Utils
import ArdanaDollar.DanaStakePool.Validators
import ArdanaDollar.Utils
import ArdanaDollar.Vault as Vault

mintNFT :: forall (s :: Row Type) (w :: Type). Contract w s Text Ledger.AssetClass
mintNFT = do
  self <- Ledger.pubKeyHash <$> ownPubKey
  let nftTokenName = Value.TokenName PlutusTx.Prelude.emptyByteString
  x <-
    mapError
      (pack . show @Currency.CurrencyError)
      (Currency.mintContract self [(nftTokenName, 1)])
  return $ Value.assetClass (Currency.currencySymbol x) nftTokenName

userDatumUtxos :: forall (s :: Row Type) (w :: Type). NFTAssetClass -> Contract w s Text [((Ledger.TxOutRef, Ledger.TxOutTx), UserData)]
userDatumUtxos nft = do
  utxos <- utxoAt (spAddress nft)
  return $ Map.toList utxos >>= own
  where
    own e@(_, o) =
      case datumForOffchain o of
        Just (UserDatum datum) -> [(e, datum)]
        _ -> []

userUtxos :: forall (s :: Row Type) (w :: Type). NFTAssetClass -> Ledger.PubKeyHash -> Contract w s Text [((Ledger.TxOutRef, Ledger.TxOutTx), UserData)]
userUtxos nft self = do
  filter (belongsTo self) <$> userDatumUtxos nft
  where
    belongsTo pkh (_, dat) = dPkh dat == pkh

ownUtxos :: forall (s :: Row Type) (w :: Type). NFTAssetClass -> Contract w s Text [((Ledger.TxOutRef, Ledger.TxOutTx), UserData)]
ownUtxos nft = do
  self <- Ledger.pubKeyHash <$> ownPubKey
  userUtxos nft self

globalUtxo :: forall (s :: Row Type) (w :: Type). NFTAssetClass -> Contract w s Text ((Ledger.TxOutRef, Ledger.TxOutTx), GlobalData)
globalUtxo nft = do
  utxos <- Map.filter hasNft <$> utxoAt (spAddress nft)
  let found = Map.toList utxos >>= own
  if length found == 1
    then return $ head found
    else throwError "not global utxo"
  where
    own e@(_, o) =
      case datumForOffchain o of
        Just (GlobalDatum datum) -> [(e, datum)]
        _ -> []

    hasNft :: Ledger.TxOutTx -> Bool
    hasNft txOutTx =
      1
        == Value.assetClassValueOf
          (Ledger.txOutValue (Ledger.txOutTxOut txOutTx))
          (unNFTAssetClass nft)

totalBalance :: [Balance] -> Balance
totalBalance = fold

balanceToUserValue :: NFTAssetClass -> Balance -> Value.Value
balanceToUserValue nftAC b = dStake b <> dReward b <> Value.assetClassValue (userInitProofAssetClass nftAC) 1

spendWithConstRedeemer :: Redeemer -> UtxoMap -> Constraints.TxConstraints Redeemer Datum
spendWithConstRedeemer r utxos =
  mconcat
    [ Constraints.mustSpendScriptOutput
      oref
      $ Ledger.Redeemer $ PlutusTx.dataToBuiltinData $ PlutusTx.toData r
    | oref <- fst <$> Map.toList utxos
    ]

addReward :: UserData -> Value.Value -> UserData
addReward (UserData pkh (Balance stake r) id') v = UserData pkh (Balance stake (r <> v)) id'

initializeSystem :: forall (s :: Row Type). Contract (Last NFTAssetClass) s Text ()
initializeSystem = do
  nftAssetClass <- mintNFT
  let c =
        Constraints.mustPayToTheScript
          (GlobalDatum (GlobalData PlutusTx.Prelude.mempty 0 False TraversalInactive))
          (Value.assetClassValue nftAssetClass 1)

  ledgerTx <- submitTxConstraints (spInst (NFTAssetClass nftAssetClass)) c
  awaitTxConfirmed $ Ledger.txId ledgerTx
  logInfo @String $ "Initialized global state represented by token: " ++ show nftAssetClass
  tell $ Last $ Just (NFTAssetClass nftAssetClass)
  return ()

initializeUser :: forall (s :: Row Type). NFTAssetClass -> Contract (Last Datum) s Text ()
initializeUser nft = do
  self <- Ledger.pubKeyHash <$> ownPubKey
  global <- globalUtxo nft

  let val = Value.assetClassValue (userInitProofAssetClass nft) 1
      toSpend = Map.fromList [fst global]
      oldData = snd global

      lookups =
        Constraints.typedValidatorLookups (spInst nft)
          <> Constraints.otherScript (spValidator nft)
          <> Constraints.mintingPolicy (userInitProofPolicy nft)
          <> Constraints.unspentOutputs toSpend
      tx =
        Constraints.mustMintValue val
          <> Constraints.mustPayToTheScript (UserDatum (UserData self PlutusTx.Prelude.mempty (dUserDatumCount oldData))) val
          <> Constraints.mustPayToTheScript (GlobalDatum (oldData {dUserDatumCount = dUserDatumCount oldData Prelude.+ 1})) (Ledger.txOutValue $ Ledger.txOutTxOut $ snd $ fst global)
          <> spendWithConstRedeemer InitializeUser toSpend

  ledgerTx <- submitTxConstraintsWith @ValidatorTypes lookups tx
  void $ awaitTxConfirmed $ Ledger.txId ledgerTx

deposit :: forall (s :: Row Type) (w :: Type). NFTAssetClass -> Integer -> Contract w s Text ()
deposit nft amount = do
  self <- Ledger.pubKeyHash <$> ownPubKey
  utxos <- ownUtxos nft
  global <- globalUtxo nft

  let danaAmount = Value.assetClassValue danaAsset amount
      oldBalance = totalBalance (dBalance . snd <$> utxos)
      newBalance = oldBalance <> Balance danaAmount PlutusTx.Prelude.mempty

      toSpend = Map.fromList (fst <$> utxos) <> Map.fromList [fst global]

      lookups =
        Constraints.typedValidatorLookups (spInst nft)
          <> Constraints.otherScript (spValidator nft)
          <> Constraints.unspentOutputs toSpend
      tx =
        Constraints.mustPayToTheScript (UserDatum (UserData self newBalance 0)) (balanceToUserValue nft newBalance)
          <> Constraints.mustPayToTheScript
            (GlobalDatum $ addTotalStake (snd global) danaAmount)
            (Ledger.txOutValue $ Ledger.txOutTxOut (snd $ fst global))
          <> spendWithConstRedeemer DepositOrWithdraw toSpend

  if positive (dStake newBalance)
    then do
      ledgerTx <- submitTxConstraintsWith lookups tx
      void $ awaitTxConfirmed $ Ledger.txId ledgerTx
    else throwError "Negative balance not acceptable"

withdraw :: forall (s :: Row Type). NFTAssetClass -> Integer -> Contract (Last Datum) s Text ()
withdraw nft amount = deposit nft (Numeric.negate amount)

provideRewards :: forall (s :: Row Type). NFTAssetClass -> Integer -> Contract (Last Datum) s Text ()
provideRewards nft amount = do
  global <- globalUtxo nft
  pkh <- Ledger.pubKeyHash <$> ownPubKey

  let dusdAmount = Value.assetClassValue Vault.dUSDAsset amount
      toSpend = Map.fromList [fst global]
      globalValue = Ledger.txOutValue $ Ledger.txOutTxOut (snd $ fst global)

      lookups =
        Constraints.typedValidatorLookups (spInst nft)
          <> Constraints.otherScript (spValidator nft)
          <> Constraints.unspentOutputs toSpend
          <> Constraints.ownPubKeyHash pkh
      tx =
        Constraints.mustPayToTheScript (GlobalDatum $ snd global) (globalValue <> dusdAmount)
          <> spendWithConstRedeemer ProvideRewards toSpend

  ledgerTx <- submitTxConstraintsWith lookups tx
  void $ awaitTxConfirmed $ Ledger.txId ledgerTx

distributeRewardsTrigger :: forall (s :: Row Type). NFTAssetClass -> Contract (Last Datum) s Text ()
distributeRewardsTrigger nft = do
  global <- globalUtxo nft

  let totalReward = Ledger.txOutValue $ Ledger.txOutTxOut $ snd $ fst global
      globalData = snd global

      lookups =
        Constraints.typedValidatorLookups (spInst nft)
          <> Constraints.otherScript (spValidator nft)
          <> Constraints.unspentOutputs (Map.fromList [fst global])
      tx =
        Constraints.mustPayToTheScript (GlobalDatum $ globalData {dTraversal = TraversalActive totalReward 0}) totalReward
          <> spendWithConstRedeemer DistributeRewards (Map.fromList [fst global])

  ledgerTx <- submitTxConstraintsWith lookups tx
  void $ awaitTxConfirmed $ Ledger.txId ledgerTx

distributeRewardsUser :: forall (s :: Row Type). NFTAssetClass -> Value.Value -> ((Ledger.TxOutRef, Ledger.TxOutTx), UserData) -> Contract (Last Datum) s Text ()
distributeRewardsUser nft totalReward (tuple', userData) = do
  global <- globalUtxo nft

  let totalStake = dTotalStake $ snd global
      reward' = rewardHelper danaAsset totalStake totalReward (dStake $ dBalance userData)
      leftover = totalReward <> Numeric.negate reward'

      lookups =
        Constraints.typedValidatorLookups (spInst nft)
          <> Constraints.otherScript (spValidator nft)
          <> Constraints.unspentOutputs (Map.fromList [fst global, tuple'])
      tx =
        Constraints.mustPayToTheScript (GlobalDatum $ snd global) leftover
          <> Constraints.mustPayToTheScript (UserDatum $ addReward userData reward') (reward' <> Ledger.txOutValue (Ledger.txOutTxOut $ snd tuple'))
          <> spendWithConstRedeemer DistributeRewards (Map.fromList [fst global, tuple'])

  ledgerTx <- submitTxConstraintsWith lookups tx
  void $ awaitTxConfirmed $ Ledger.txId ledgerTx

distributeRewards :: forall (s :: Row Type). NFTAssetClass -> () -> Contract (Last Datum) s Text ()
distributeRewards nft _ = do
  utxos <- userDatumUtxos nft
  global <- globalUtxo nft

  let totalStakeUtxo = dStake $ totalBalance $ dBalance . snd <$> utxos
      totalStakeGlobal = dTotalStake (snd global)
      totalReward = Ledger.txOutValue $ Ledger.txOutTxOut $ snd $ fst global
      sorted = sortBy (\(_, d1) (_, d2) -> compare (dId d1) (dId d2)) utxos
      txs = mapM_ (distributeRewardsUser nft totalReward) sorted
  if
      | totalStakeUtxo /= totalStakeGlobal -> throwError "not all utxos provided"
      | totalStakeGlobal == PlutusTx.Prelude.mempty -> logInfo @String $ "total stake is zero"
      | totalReward == PlutusTx.Prelude.mempty -> logInfo @String $ "no rewards"
      | null utxos -> throwError "no user utxos"
      | otherwise ->
        do
          distributeRewardsTrigger nft
          txs

withdrawRewards :: forall (s :: Row Type). NFTAssetClass -> () -> Contract (Last Datum) s Text ()
withdrawRewards nft _ = do
  self <- Ledger.pubKeyHash <$> ownPubKey
  utxos <- ownUtxos nft
  let oldBalance = totalBalance (dBalance . snd <$> utxos)
      newBalance = Balance (dStake oldBalance) PlutusTx.Prelude.mempty
      toSpend = Map.fromList (fst <$> utxos)

      lookups =
        Constraints.typedValidatorLookups (spInst nft)
          <> Constraints.otherScript (spValidator nft)
          <> Constraints.unspentOutputs toSpend
      tx =
        Constraints.mustPayToTheScript (UserDatum (UserData self newBalance 0)) (balanceToUserValue nft newBalance)
          <> Constraints.mustPayToPubKey self (dReward oldBalance)
          <> spendWithConstRedeemer WithdrawRewards toSpend

  ledgerTx <- submitTxConstraintsWith lookups tx
  void $ awaitTxConfirmed $ Ledger.txId ledgerTx

queryUser :: forall (s :: Row Type). NFTAssetClass -> Ledger.PubKeyHash -> Contract (Last UserData) s Text ()
queryUser nft pkh = do
  utxos <- userUtxos nft pkh
  let total = totalBalance (dBalance . snd <$> utxos)
  tell $ Last $ Just (UserData pkh total 0)

querySelf :: forall (s :: Row Type). NFTAssetClass -> Contract (Last UserData) s Text ()
querySelf nft = do
  self <- Ledger.pubKeyHash <$> ownPubKey
  utxos <- userUtxos nft self
  let total = totalBalance (dBalance . snd <$> utxos)
  tell $ Last $ Just (UserData self total 0)