{-# LANGUAGE MultiWayIf #-}

module ArdanaDollar.DanaStakePool.Contracts (
  initializeSystem,
  initializeUser,
  deposit,
  withdraw,
  provideRewards,
  distributeRewards,
  withdrawRewards,
  queryUser,
  querySelf,
) where

import Prelude hiding (init, last)

import Plutus.Contract
import PlutusTx
import PlutusTx.Numeric qualified as Numeric
import PlutusTx.Prelude (emptyByteString, fold, mempty)

import Ledger qualified
import Ledger.Constraints qualified as Constraints
import Ledger.Value qualified as Value
import Plutus.Contracts.Currency qualified as Currency

import Control.Lens ((^.))
import Control.Monad.Except
import Data.Functor ((<&>))
import Data.Kind
import Data.List.NonEmpty (init, last, nonEmpty)
import Data.Map qualified as Map
import Data.Monoid (Last (Last))
import Data.Row
import Data.Text (Text, pack)

import ArdanaDollar.DanaStakePool.DanaCurrency
import ArdanaDollar.DanaStakePool.Types
import ArdanaDollar.DanaStakePool.Utils
import ArdanaDollar.DanaStakePool.Validators
import ArdanaDollar.DanaStakePool.ValidatorsTH
import ArdanaDollar.Utils
import ArdanaDollar.Vault as Vault

nftToken :: NFTAssetClass -> Value.Value
nftToken nftAC = Value.assetClassValue (unNFTAssetClass nftAC) 1

mintNFT :: forall (s :: Row Type) (w :: Type). Contract w s Text Ledger.AssetClass
mintNFT = do
  self <- ownPubKeyHash
  let nftTokenName = Value.TokenName PlutusTx.Prelude.emptyByteString
  x <-
    mapError
      (pack . show @Currency.CurrencyError)
      (Currency.mintContract self [(nftTokenName, 1)])
  return $ Value.assetClass (Currency.currencySymbol x) nftTokenName

userDatumUtxos ::
  forall (s :: Row Type) (w :: Type).
  NFTAssetClass ->
  Contract w s Text [((Ledger.TxOutRef, Ledger.ChainIndexTxOut), UserData)]
userDatumUtxos nft = do
  utxos <- utxosAt (spAddress nft)
  concat <$> forM (Map.toList utxos) own
  where
    own e@(_, o) =
      datumForOffchain o <&> \case
        Just (UserDatum datum) -> [(e, datum)]
        _ -> []

userUtxos ::
  forall (s :: Row Type) (w :: Type).
  NFTAssetClass ->
  Ledger.PubKeyHash ->
  Contract w s Text [((Ledger.TxOutRef, Ledger.ChainIndexTxOut), UserData)]
userUtxos nft self = do
  filter (belongsTo self) <$> userDatumUtxos nft
  where
    belongsTo pkh (_, dat) = userData'pkh dat == pkh

ownUtxos ::
  forall (s :: Row Type) (w :: Type).
  NFTAssetClass ->
  Contract w s Text [((Ledger.TxOutRef, Ledger.ChainIndexTxOut), UserData)]
ownUtxos nft = do
  self <- ownPubKeyHash
  userUtxos nft self

globalUtxo ::
  forall (s :: Row Type) (w :: Type).
  NFTAssetClass ->
  Contract w s Text ((Ledger.TxOutRef, Ledger.ChainIndexTxOut), GlobalData)
globalUtxo nft = do
  utxos <- Map.filter hasNft <$> utxosAt (spAddress nft)
  found <- concat <$> forM (Map.toList utxos) own
  case found of
    [h] -> return h
    _ -> throwError "not global utxo"
  where
    own e@(_, o) =
      datumForOffchain o <&> \case
        Just (GlobalDatum datum) -> [(e, datum)]
        _ -> []

    hasNft :: Ledger.ChainIndexTxOut -> Bool
    hasNft txOutTx =
      1 == Value.assetClassValueOf (txOutTx ^. Ledger.ciTxOutValue) (unNFTAssetClass nft)

totalBalance :: [Balance] -> Balance
totalBalance = fold

balanceToUserValue :: NFTAssetClass -> Balance -> Value.Value
balanceToUserValue nftAC b = balance'stake b <> balance'reward b <> Value.assetClassValue (userInitProofAssetClass nftAC) 1

addTotalStake :: GlobalData -> Value.Value -> GlobalData
addTotalStake (GlobalData s c l t) v = GlobalData (s <> v) c l t

spendWithConstRedeemer ::
  Redeemer ->
  Map.Map Ledger.TxOutRef Ledger.ChainIndexTxOut ->
  Constraints.TxConstraints Redeemer Datum
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
  awaitTxConfirmed $ Ledger.getCardanoTxId ledgerTx
  logInfo @String $ "Initialized global state represented by token: " <> show nftAssetClass
  tell $ Last $ Just (NFTAssetClass nftAssetClass)
  return ()

initializeUser :: forall (s :: Row Type). NFTAssetClass -> Contract (Last Datum) s Text ()
initializeUser nft = do
  self <- ownPubKeyHash
  global <- globalUtxo nft

  let userInitProofToken = Value.assetClassValue (userInitProofAssetClass nft) 1
      toSpend = Map.fromList [fst global]

      oldGlobalData = snd global
      newGlobalData = oldGlobalData {globalData'count = globalData'count oldGlobalData + 1}
      oldGlobalValue = snd (fst global) ^. Ledger.ciTxOutValue
      newGlobalValue = oldGlobalValue
      newUserData = UserData self PlutusTx.Prelude.mempty (globalData'count oldGlobalData)

      lookups =
        Constraints.typedValidatorLookups (spInst nft)
          <> Constraints.otherScript (spValidator nft)
          <> Constraints.mintingPolicy (userInitProofPolicy nft)
          <> Constraints.unspentOutputs toSpend
      tx =
        Constraints.mustMintValue userInitProofToken
          <> Constraints.mustPayToTheScript (UserDatum newUserData) userInitProofToken
          <> Constraints.mustPayToTheScript (GlobalDatum newGlobalData) newGlobalValue
          <> spendWithConstRedeemer InitializeUser toSpend

  logInfo @String $ "initializing user with: %s" <> show newUserData

  ledgerTx <- submitTxConstraintsWith @ValidatorTypes lookups tx
  void $ awaitTxConfirmed $ Ledger.getCardanoTxId ledgerTx

deposit :: forall (s :: Row Type) (w :: Type). NFTAssetClass -> Integer -> Contract w s Text ()
deposit nft amount =
  ownUtxos nft >>= \case
    [] -> throwError "Cannot find user's UTxOs"
    (utxo : _) -> do
      global <- globalUtxo nft

      let danaAmount = Value.assetClassValue danaAsset amount

          oldBalance = userData'balance $ snd utxo
          newBalance = oldBalance <> Balance danaAmount PlutusTx.Prelude.mempty
          oldGlobalData = snd global
          newGlobalData = addTotalStake oldGlobalData danaAmount
          oldUserData = snd utxo
          newUserData = oldUserData {userData'balance = newBalance}
          oldGlobalValue = snd (fst global) ^. Ledger.ciTxOutValue
          newGlobalValue = oldGlobalValue

          toSpend = Map.fromList [fst global, fst utxo]

          lookups =
            Constraints.typedValidatorLookups (spInst nft)
              <> Constraints.otherScript (spValidator nft)
              <> Constraints.unspentOutputs toSpend
          tx =
            Constraints.mustPayToTheScript (UserDatum newUserData) (balanceToUserValue nft newBalance)
              <> Constraints.mustPayToTheScript (GlobalDatum newGlobalData) newGlobalValue
              <> spendWithConstRedeemer DepositOrWithdraw toSpend

      if positive (balance'stake newBalance)
        then do
          ledgerTx <- submitTxConstraintsWith lookups tx
          void $ awaitTxConfirmed $ Ledger.getCardanoTxId ledgerTx
        else throwError "Negative balance not acceptable"

withdraw :: forall (s :: Row Type). NFTAssetClass -> Integer -> Contract (Last Datum) s Text ()
withdraw nft amount = deposit nft (Numeric.negate amount)

provideRewards :: forall (s :: Row Type). NFTAssetClass -> Integer -> Contract (Last Datum) s Text ()
provideRewards nft amount = do
  global <- globalUtxo nft

  let dusdAmount = Value.assetClassValue Vault.dUSDAsset amount
      toSpend = Map.fromList [fst global]

      oldGlobalValue = snd (fst global) ^. Ledger.ciTxOutValue
      newGlobalValue = oldGlobalValue <> dusdAmount
      oldGlobalData = snd global
      newGlobalData = oldGlobalData

      lookups =
        Constraints.typedValidatorLookups (spInst nft)
          <> Constraints.otherScript (spValidator nft)
          <> Constraints.unspentOutputs toSpend
      tx =
        Constraints.mustPayToTheScript (GlobalDatum newGlobalData) newGlobalValue
          <> spendWithConstRedeemer ProvideRewards toSpend

  ledgerTx <- submitTxConstraintsWith lookups tx
  void $ awaitTxConfirmed $ Ledger.getCardanoTxId ledgerTx

distributeRewardsTrigger :: forall (s :: Row Type). NFTAssetClass -> Contract (Last Datum) s Text ()
distributeRewardsTrigger nft = do
  global <- globalUtxo nft

  let oldGlobalValue = snd (fst global) ^. Ledger.ciTxOutValue
      newGlobalValue = oldGlobalValue
      totalReward = oldGlobalValue <> Numeric.negate (nftToken nft)
      oldGlobalData = snd global
      newGlobalData = oldGlobalData {globaldata'traversal = TraversalActive totalReward 0, globalData'locked = True}

      toSpend = Map.fromList [fst global]

      lookups =
        Constraints.typedValidatorLookups (spInst nft)
          <> Constraints.otherScript (spValidator nft)
          <> Constraints.unspentOutputs toSpend
      tx =
        Constraints.mustPayToTheScript (GlobalDatum newGlobalData) newGlobalValue
          <> spendWithConstRedeemer DistributeRewards toSpend

  ledgerTx <- submitTxConstraintsWith lookups tx
  void $ awaitTxConfirmed $ Ledger.getCardanoTxId ledgerTx

distributeRewardsUser ::
  forall (s :: Row Type).
  NFTAssetClass ->
  Value.Value ->
  Bool ->
  ((Ledger.TxOutRef, Ledger.ChainIndexTxOut), UserData) ->
  Contract (Last Datum) s Text ()
distributeRewardsUser nft totalReward locked (tuple', oldUserData) = do
  global <- globalUtxo nft

  let oldGlobalData = snd global
      oldGlobalValue = snd (fst global) ^. Ledger.ciTxOutValue

      newTraversal =
        if userData'id oldUserData == globalData'count oldGlobalData - 1
          then TraversalInactive
          else TraversalActive totalReward $ userData'id oldUserData + 1

      totalStake = globalData'totalStake oldGlobalData
      reward' = rewardHelper danaAsset totalStake totalReward (balance'stake $ userData'balance oldUserData)

      newGlobalData = oldGlobalData {globaldata'traversal = newTraversal, globalData'locked = locked}
      newUserData = addReward oldUserData reward'
      leftover = oldGlobalValue <> Numeric.negate reward'
      newGlobalValue = leftover

      toSpend = Map.fromList [fst global, tuple']

      lookups =
        Constraints.typedValidatorLookups (spInst nft)
          <> Constraints.otherScript (spValidator nft)
          <> Constraints.unspentOutputs toSpend
      tx =
        Constraints.mustPayToTheScript (GlobalDatum newGlobalData) newGlobalValue
          <> Constraints.mustPayToTheScript (UserDatum newUserData) (balanceToUserValue nft $ userData'balance newUserData)
          <> spendWithConstRedeemer DistributeRewards toSpend

  ledgerTx <- submitTxConstraintsWith lookups tx
  void $ awaitTxConfirmed $ Ledger.getCardanoTxId ledgerTx

distributeRewards :: forall (s :: Row Type). NFTAssetClass -> () -> Contract (Last Datum) s Text ()
distributeRewards nft _ = do
  utxos <- userDatumUtxos nft
  global <- globalUtxo nft

  let totalStakeUtxo = balance'stake $ totalBalance $ userData'balance . snd <$> utxos
      totalStakeGlobal = globalData'totalStake (snd global)

      oldGlobalValue = snd (fst global) ^. Ledger.ciTxOutValue
      totalReward = oldGlobalValue <> Numeric.negate (nftToken nft)
      sorted = sortBy (\(_, d1) (_, d2) -> compare (userData'id d1) (userData'id d2)) utxos
  case nonEmpty sorted of
    Nothing -> throwError "No UTxOs"
    Just sorted' -> do
      let txs = mapM_ (distributeRewardsUser nft totalReward True) (init sorted')
      if
          | totalStakeUtxo /= totalStakeGlobal -> throwError "not all utxos provided"
          | totalStakeGlobal == PlutusTx.Prelude.mempty -> logInfo @String $ "total stake is zero"
          | totalReward == PlutusTx.Prelude.mempty -> logInfo @String $ "no rewards"
          | null utxos -> throwError "no user utxos"
          | otherwise ->
            do
              logInfo @String $ "found utxos" <> show (length sorted) <> show sorted
              distributeRewardsTrigger nft
              txs
              distributeRewardsUser nft totalReward False (last sorted')

withdrawRewards :: forall (s :: Row Type). NFTAssetClass -> () -> Contract (Last Datum) s Text ()
withdrawRewards nft _ =
  ownUtxos nft >>= \case
    [] -> throwError "Could not find user's UTxOs"
    (utxo : _) -> do
      self <- ownPubKeyHash

      let oldBalance = userData'balance $ snd utxo
          newBalance = Balance (balance'stake oldBalance) PlutusTx.Prelude.mempty
          oldUserData = snd utxo
          newUserData = oldUserData{userData'balance = newBalance}

          toSpend = Map.fromList [fst utxo]

          lookups =
            Constraints.typedValidatorLookups (spInst nft)
              <> Constraints.otherScript (spValidator nft)
              <> Constraints.unspentOutputs toSpend
          tx =
            Constraints.mustPayToTheScript (UserDatum newUserData) (balanceToUserValue nft newBalance)
              <> Constraints.mustPayToPubKey self (balance'reward oldBalance)
              <> spendWithConstRedeemer WithdrawRewards toSpend

      ledgerTx <- submitTxConstraintsWith lookups tx
      void $ awaitTxConfirmed $ Ledger.getCardanoTxId ledgerTx

queryUser :: forall (s :: Row Type). NFTAssetClass -> Ledger.PubKeyHash -> Contract (Last [UserData]) s Text ()
queryUser nft pkh = do
  utxos <- userUtxos nft pkh
  tell $ Last $ Just (snd <$> utxos)

querySelf :: forall (s :: Row Type). NFTAssetClass -> Contract (Last [UserData]) s Text ()
querySelf nft = do
  self <- ownPubKeyHash
  queryUser nft self
