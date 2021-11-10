{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module ArdanaDollar.Map.SnapshotOpContracts (
  createSnapshot,
  initiateSnapshot,
  splitSnapshotPerm,
  mergeUnlockPerms,
  sortUnlockPermInfo,
) where

import Prelude

import Ledger qualified
import Ledger.Constraints qualified as Constraints
import Ledger.Scripts qualified as Scripts
import Plutus.Contract (
  Contract (..),
  logInfo,
  ownPubKey,
  submitTx,
  submitTxConstraintsWith,
  throwError,
  txFromTxId,
  unContract,
  utxosAt,
  utxosTxOutTxFromTx,
  waitNSlots,
 )
import Plutus.Contract.Types (ContractEffs)
import Plutus.V1.Ledger.Api qualified as Ledger
import Plutus.V1.Ledger.Value qualified as Value
import PlutusTx.IsData.Class (ToData (toBuiltinData))

import Control.Lens ((^.))
import Control.Monad (join, void)
import Data.Kind (Type)
import Data.List (find, foldl', partition)
import Data.Map qualified as M
import Data.Map.Strict qualified as SM
import Data.Maybe (fromJust, isNothing, maybeToList)
import Data.Row (Row)
import Data.Sequence qualified as S
import Data.Text (Text)

import Ledger.Ada as Ada

import Control.Monad.Freer
import Control.Monad.Freer.Reader
import Control.Monad.Freer.State
import Data.OpenUnion

import ArdanaDollar.Map.ContractUtils (
  MapLookup (..),
  Tpl,
  fromJust',
  lookups',
  mkMapLookup,
 )
import ArdanaDollar.Map.MapTerms (
  MapTerms,
  MapTerms' (K', V', ValidatorTypes', nodeValidPolicy', snapshotPolicy', unlockPermPolicy', unlockPolicy'),
 )
import ArdanaDollar.Map.TxUtils qualified as U
import ArdanaDollar.Map.Types (
  Datum (
    MapDatum,
    MapSnapshotDatum,
    NodeDatum,
    NodeSnapshotDatum,
    SnapshotPermDatum,
    UnlockDatum,
    UnlockPermDatum
  ),
  LockState (..),
  Map (..),
  MapInstance (..),
  MapSnapshot (MapSnapshot),
  Node (..),
  NodeSnapshot (..),
  Pointer (..),
  Redeemer (SnapshotOp, UnlockOp, UnlockPermOp),
  SnapshotPerm (..),
  SnapshotPointer (..),
  SnapshotTokenRedeemer (..),
  SnapshotVersion (..),
  Unlock (..),
  UnlockPerm (..),
  UnlockPermTokenRedeemer (..),
  UnlockTokenRedeemer (..),
 )
import ArdanaDollar.Map.Types qualified as T
import PlutusTx.Builtins (emptyByteString)

data MapLookupSeq k v = MapLookupSeq
  { mapLookupSeq'map :: (Tpl, Map)
  , mapLookupSeq'nodes :: S.Seq (Tpl, Node k v)
  }

type EffC t w a = Eff ('[Reader MapInstance, State (MapLookupSeq (K' t) (V' t))] :++: ContractEffs w Text) a

splitFunds ::
  forall (s :: Row Type) (w :: Type).
  Contract w s Text ()
splitFunds = do
  printNumberOfSelfUtxos

  pubKey <- ownPubKey
  let key = Ledger.pubKeyHash pubKey
  let tt = mconcat $ replicate 10 (Constraints.mustPayToPubKey key (Ada.lovelaceValueOf 100_000_000))

  ownUtxos <- getOwnUtxos

  if length ownUtxos > 2
    then return ()
    else do
      ledgerTx <- submitTx tt
      void $ awaitTxConfirmed' $ Ledger.txId ledgerTx

  printNumberOfSelfUtxos

getOwnUtxos ::
  forall (s :: Row Type) (w :: Type).
  Contract w s Text (M.Map Ledger.TxOutRef Ledger.ChainIndexTxOut)
getOwnUtxos = do
  pubKey <- ownPubKey
  utxosAt (Ledger.pubKeyAddress pubKey)

printNumberOfSelfUtxos ::
  forall (s :: Row Type) (w :: Type).
  Contract w s Text ()
printNumberOfSelfUtxos = do
  utxos <- getOwnUtxos
  logInfo @String $ "Map: self utxos: " <> show (length utxos)

createSnapshot ::
  forall (t :: Type) (s :: Row Type) (w :: Type).
  MapTerms t =>
  MapInstance ->
  Contract w s Text ()
createSnapshot mapInstance = do
  splitFunds

  logInfo @String $ "Map: creating snapshot"

  lkp <- mkMapLookup @t mapInstance
  if isNothing $ T.map'head $ snd $ mapLookup'map lkp
    then createSnapshotOfEmptyMap @t mapInstance (mapLookup'map lkp)
    else do
      createSnapshotOfNonEmptyMap @t mapInstance lkp (mapLookup'map lkp) (last $ mapLookup'nodes lkp)
      unlockWholeMap @t mapInstance

unlockWholeMap ::
  forall (t :: Type) (s :: Row Type) (w :: Type).
  MapTerms t =>
  MapInstance ->
  Contract w s Text ()
unlockWholeMap mapInstance = do
  lkp <- mkMapLookup @t mapInstance
  unlock <- generateUnlock @t mapInstance (mapLookup'map lkp) (SnapshotVersion (unSnapshotVersion (map'nextVersion $ snd $ mapLookup'map lkp) - 1))
  logInfo @String $ "Map: unlock generated"
  unlocks <- cloneUnlock @t mapInstance unlock (toInteger $ length $ mapLookup'nodes lkp)
  logInfo @String $ "Map: unlock cloned"
  let zipped = zip unlocks (mapLookup'nodes lkp)
  bundled <- bundleWithFeeSource zipped
  conts <- mapM (doUnlock @t mapInstance) bundled
  sequence_ conts
  logInfo @String $ "Map: all entries unlocked"

assetClass1 :: forall (t :: Type). MapTerms t => MapInstance -> Value.AssetClass
assetClass1 mapInstance =
  Value.AssetClass
    ( Ledger.scriptCurrencySymbol (snapshotPolicy' @t mapInstance)
    , Value.TokenName emptyByteString
    )

assetClass2 :: forall (t :: Type). MapTerms t => MapInstance -> Value.AssetClass
assetClass2 mapInstance =
  Value.AssetClass
    ( Ledger.scriptCurrencySymbol (unlockPermPolicy' @t mapInstance)
    , Value.TokenName emptyByteString
    )

assetClass3 :: forall (t :: Type). MapTerms t => MapInstance -> Value.AssetClass
assetClass3 mapInstance =
  Value.AssetClass
    ( Ledger.scriptCurrencySymbol (unlockPolicy' @t mapInstance)
    , Value.TokenName emptyByteString
    )

createSnapshotOfEmptyMap ::
  forall (t :: Type) (s :: Row Type) (w :: Type).
  MapTerms t =>
  MapInstance ->
  (Tpl, Map) ->
  Contract w s Text ()
createSnapshotOfEmptyMap mapInstance (inputMap', inputMap) = do
  let inputMapValue = snd inputMap' ^. Ledger.ciTxOutValue
      version = T.map'nextVersion inputMap
      outputMap = inputMap {T.map'nextVersion = U.incrementSV version}
      mapSnapshot = MapSnapshot inputMapValue Nothing version

  let toSpend = M.fromList [inputMap']
      lookups =
        lookups' @t mapInstance toSpend
          <> Constraints.mintingPolicy (snapshotPolicy' @t mapInstance)
      tokenRedeemer = MakeSnapshotOfEmpty
      tokenValue = Value.assetClassValue (assetClass1 @t mapInstance) 1

      tx =
        Constraints.mustMintValueWithRedeemer
          (Ledger.Redeemer $ Ledger.toBuiltinData @SnapshotTokenRedeemer tokenRedeemer)
          tokenValue
          <> Constraints.mustPayToTheScript (MapDatum outputMap) inputMapValue
          <> Constraints.mustPayToTheScript (MapSnapshotDatum mapSnapshot) tokenValue
          <> Constraints.mustSpendScriptOutput
            (fst inputMap')
            (Ledger.Redeemer $ Ledger.toBuiltinData SnapshotOp)

  logInfo @String $ "Map: make snapshot of empty map"
  ledgerTx <- submitTxConstraintsWith @(ValidatorTypes' t) lookups tx
  void $ awaitTxConfirmed' $ Ledger.txId ledgerTx

createSnapshotOfNonEmptyMap ::
  forall (t :: Type) (s :: Row Type) (w :: Type).
  MapTerms t =>
  MapInstance ->
  MapLookup (K' t) (V' t) ->
  (Tpl, Map) ->
  (Tpl, Node (K' t) (V' t)) ->
  Contract w s Text ()
createSnapshotOfNonEmptyMap mapInstance lkp inputMap inputLast =
  Contract $
    evalState (MapLookupSeq (mapLookup'map lkp) (S.fromList $ mapLookup'nodes lkp)) $
      runReader mapInstance act
  where
    act :: EffC t w ()
    act =
      do
        raiseEnh @t $ logInfo @String $ "Map: make snapshot of non-empty map"
        snapshotPermInfo <- initiateSnapshot @t inputMap inputLast
        raiseEnh @t $ logInfo @String $ "Map: initiate snapshot: " <> show snapshotPermInfo
        splitted <- splitSnapshotPerm @t [snapshotPermInfo]
        raiseEnh @t $ logInfo @String $ "Map: splitting finished: " <> show splitted
        snapshots <- makeSnapshot @t splitted
        raiseEnh @t $ logInfo @String $ "Map: snapshots: size: " <> show (length snapshots)
        unlockPerms <- initializeUnlockPerm @t snapshots
        raiseEnh @t $ logInfo @String $ "Map: unlock perms: size: " <> show (length unlockPerms)
        ss <- raiseEnh @t $ fromJust' "Cannot sort perm info" $ sortUnlockPermInfo unlockPerms
        unlockPerm <- mergeUnlockPerms @t ss
        raiseEnh @t $ logInfo @String $ "Map: merged unlock perm: " <> show unlockPerm
        makeUnlockPerm @t unlockPerm
        raiseEnh @t $ logInfo @String $ "Map: makeUnlockPerm"

findPointer' :: forall (t :: Type). MapTerms t => MapInstance -> Value.Value -> Maybe Pointer
findPointer' mapInstance value =
  T.Pointer
    <$> U.lookupToken'
      (Ledger.scriptCurrencySymbol $ nodeValidPolicy' @t mapInstance)
      value

findPointer :: forall (t :: Type). MapTerms t => MapInstance -> Tpl -> Maybe Pointer
findPointer mapInstance (_, chainIndexTxOut) = findPointer' @t mapInstance (chainIndexTxOut ^. Ledger.ciTxOutValue)

repackage ::
  forall (t :: Type).
  MapTerms t =>
  MapInstance ->
  Pointer ->
  SnapshotPointer
repackage mapInstance (Pointer (Value.AssetClass (_, tn))) =
  SnapshotPointer (Value.AssetClass (Ledger.scriptCurrencySymbol $ snapshotPolicy' @t mapInstance, tn))

initiateSnapshot ::
  forall (t :: Type) (w :: Type).
  MapTerms t =>
  (Tpl, Map) ->
  (Tpl, Node (K' t) (V' t)) ->
  EffC t w SnapshotPermInfo
initiateSnapshot (inputMap', inputMap) (inputLast', inputLast) = do
  mapInstance <- ask
  let inputMapValue = snd inputMap' ^. Ledger.ciTxOutValue
      inputLastValue = snd inputLast' ^. Ledger.ciTxOutValue
      assetClass1Token = Value.assetClassValue (assetClass1 @t mapInstance) 1

      mapSnapshot = MapSnapshot inputMapValue (repackage @t mapInstance <$> T.map'head inputMap) version
      snapshotPerm =
        SnapshotPerm
          (fromJust $ T.map'head inputMap)
          (fromJust $ findPointer @t mapInstance inputLast')
          version

  let toSpend = M.fromList [inputMap', inputLast']
      lookups =
        lookups' @t mapInstance toSpend
          <> Constraints.mintingPolicy (snapshotPolicy' @t mapInstance)
          <> Constraints.mintingPolicy (nodeValidPolicy' @t mapInstance)
      tokenRedeemer = InitiateSnapshot (fst inputLast')

      tx =
        Constraints.mustMintValueWithRedeemer
          (Ledger.Redeemer $ Ledger.toBuiltinData @SnapshotTokenRedeemer tokenRedeemer)
          (assetClass1Token <> assetClass1Token)
          <> Constraints.mustPayToTheScript (MapDatum outputMap) inputMapValue
          <> Constraints.mustPayToTheScript (NodeDatum outputLast) inputLastValue
          <> Constraints.mustPayToTheScript (MapSnapshotDatum mapSnapshot) assetClass1Token
          <> Constraints.mustPayToTheScript (SnapshotPermDatum snapshotPerm) assetClass1Token
          <> Constraints.mustSpendScriptOutput
            (fst inputMap')
            (Ledger.Redeemer $ Ledger.toBuiltinData SnapshotOp)
          <> Constraints.mustSpendScriptOutput
            (fst inputLast')
            (Ledger.Redeemer $ Ledger.toBuiltinData SnapshotOp)

  ledgerTx <- raiseEnh @t $ submitTxConstraintsWith @(ValidatorTypes' t) lookups tx
  let txId = Ledger.txId ledgerTx

  raiseEnh @t $ awaitTxConfirmed' $ Ledger.txId ledgerTx

  updateMapLookup txId

  tpl <- raiseEnh @t $ toTpl' @t txId (SnapshotPermDatum snapshotPerm)

  lkp :: MapLookupSeq (K' t) (V' t) <- get
  let lastId = toInteger (length $ mapLookupSeq'nodes lkp) - 1

  return (tpl, SnapshotPermEx snapshotPerm (0, lastId))
  where
    version :: T.SnapshotVersion
    version = T.map'nextVersion inputMap

    outputLast :: Node (K' t) (V' t)
    outputLast = inputLast{T.node'lockState = LockedFor version}

    outputMap :: Map
    outputMap = inputMap {T.map'lockState = SnapshotDone version, T.map'nextVersion = U.incrementSV version}

    updateMapLookup :: Ledger.TxId -> EffC t w ()
    updateMapLookup txId = do
      lkp :: MapLookupSeq (K' t) (V' t) <- get
      let lastId = toInteger (length $ mapLookupSeq'nodes lkp) - 1
      lastNodeTpl <- raiseEnh @t $ toTpl' @t txId (NodeDatum outputLast)
      mapTpl <- raiseEnh @t $ toTpl' @t txId (MapDatum outputMap)
      put
        ( MapLookupSeq
            (mapTpl, outputMap)
            (S.update (fromInteger lastId) (lastNodeTpl, outputLast) (mapLookupSeq'nodes lkp))
        )

type Range = (Integer, Integer)
data SnapshotPermEx = SnapshotPermEx
  { snapshotPermEx'perm :: SnapshotPerm
  , snapshotPermEx'range :: Range
  }
  deriving stock (Show)

type SnapshotPermInfo = (Tpl, SnapshotPermEx)

newtype FeeSource = FeeSource {unFeeSource :: (Ledger.TxOutRef, Ledger.ChainIndexTxOut)}

awaitTxConfirmed' :: forall (w :: Type) (s :: Row Type). Ledger.TxId -> Contract w s Text ()
awaitTxConfirmed' txId = do
  maybeTx <- txFromTxId txId
  case maybeTx of
    Just _ -> return ()
    Nothing -> do
      void $ waitNSlots 1
      awaitTxConfirmed' txId

toTplList ::
  forall (t :: Type) (s :: Row Type) (w :: Type).
  MapTerms t =>
  Ledger.TxId ->
  Datum (K' t) (V' t) ->
  Contract w s Text [Tpl]
toTplList txId datum = do
  chainTx <- txFromTxId txId >>= ("No such transaction" `fromJust'`)
  utxos <- utxosTxOutTxFromTx chainTx

  return $
    (\(txOutRef, (chainIndexTxOut, _)) -> (txOutRef, chainIndexTxOut))
      <$> ((\(_, (chainIndexTxOut, _)) -> isOk chainIndexTxOut) `filter` utxos)
  where
    dh :: Ledger.DatumHash
    dh = Scripts.datumHash $ Scripts.Datum $ toBuiltinData datum

    isOk :: Ledger.ChainIndexTxOut -> Bool
    isOk txOut = case txOut of
      Ledger.ScriptChainIndexTxOut _ _ (Left dh') _ -> dh == dh'
      Ledger.ScriptChainIndexTxOut _ _ (Right datum') _ -> Scripts.Datum (toBuiltinData datum) == datum'
      _ -> False

toTpl ::
  forall (t :: Type) (s :: Row Type) (w :: Type).
  MapTerms t =>
  Ledger.TxId ->
  Datum (K' t) (V' t) ->
  Contract w s Text (Maybe Tpl)
toTpl txId datum = toUnique <$> toTplList @t txId datum
  where
    toUnique :: [Tpl] -> Maybe Tpl
    toUnique l = case l of
      [tpl] -> Just tpl
      _ -> Nothing

toTpl' ::
  forall (t :: Type) (s :: Row Type) (w :: Type).
  MapTerms t =>
  Ledger.TxId ->
  Datum (K' t) (V' t) ->
  Contract w s Text Tpl
toTpl' txId datum = toTpl @t txId datum >>= fromJust' "Cannot find by datum and TxId"

raiseEnh ::
  forall (t :: Type) (s :: Row Type) (w :: Type) (a :: Type).
  Contract w s Text a ->
  EffC t w a
raiseEnh = raise . raise . unContract

bundleWithFeeSource ::
  forall (w :: Type) (s :: Row Type) (a :: Type).
  [a] ->
  Contract w s Text [(FeeSource, a)]
bundleWithFeeSource l = do
  sources <- M.toList <$> getOwnUtxos
  if length sources < length l
    then throwError "Insufficient self utxos to power parallel transactions"
    else return $ zip (FeeSource <$> sources) l

split' ::
  forall (t :: Type) (s :: Row Type) (w :: Type).
  MapTerms t =>
  MapInstance ->
  FeeSource ->
  (Tpl, SnapshotPerm) ->
  (Tpl, Node (K' t) (V' t)) ->
  (Tpl, Node (K' t) (V' t)) ->
  Contract w s Text (Ledger.TxId, (SnapshotPerm, SnapshotPerm))
split' mapInstance (FeeSource sourceTpl) a1 a2 a3 = do
  let (snapshotPermInput', snapshotPermInput) = a1
      (leftNodeInput', leftNodeInput) = a2
      (rightNodeInput', rightNodeInput) = a3

      snapshotPermToken = Value.assetClassValue (assetClass1 @t mapInstance) 1
      version = T.snapshotPerm'version snapshotPermInput
      snapshotPermLeftOutput =
        SnapshotPerm
          (T.snapshotPerm'start snapshotPermInput)
          (fromJust $ findPointer @t mapInstance leftNodeInput')
          version
      snapshotPermRightOutput =
        SnapshotPerm
          (fromJust $ findPointer @t mapInstance rightNodeInput')
          (T.snapshotPerm'end snapshotPermInput)
          version
      leftNodeOutput = leftNodeInput {T.node'lockState = LockedFor version}
      rightNodeOutput = rightNodeInput {T.node'lockState = LockedFor version}

  let toSpend = M.fromList [snapshotPermInput', leftNodeInput', rightNodeInput', sourceTpl]
      lookups =
        lookups' @t mapInstance toSpend
          <> Constraints.mintingPolicy (snapshotPolicy' @t mapInstance)
      tokenRedeemer =
        SplitSnapshot
          (fst snapshotPermInput')
          (fst leftNodeInput')
          (fst rightNodeInput')

      tx =
        Constraints.mustMintValueWithRedeemer
          (Ledger.Redeemer $ Ledger.toBuiltinData @SnapshotTokenRedeemer tokenRedeemer)
          snapshotPermToken
          <> Constraints.mustPayToTheScript (NodeDatum leftNodeOutput) (snd leftNodeInput' ^. Ledger.ciTxOutValue)
          <> Constraints.mustPayToTheScript (NodeDatum rightNodeOutput) (snd rightNodeInput' ^. Ledger.ciTxOutValue)
          <> Constraints.mustPayToTheScript (SnapshotPermDatum snapshotPermLeftOutput) snapshotPermToken
          <> Constraints.mustPayToTheScript (SnapshotPermDatum snapshotPermRightOutput) snapshotPermToken
          <> Constraints.mustSpendScriptOutput
            (fst snapshotPermInput')
            (Ledger.Redeemer $ Ledger.toBuiltinData SnapshotOp)
          <> Constraints.mustSpendScriptOutput
            (fst leftNodeInput')
            (Ledger.Redeemer $ Ledger.toBuiltinData SnapshotOp)
          <> Constraints.mustSpendScriptOutput
            (fst rightNodeInput')
            (Ledger.Redeemer $ Ledger.toBuiltinData SnapshotOp)
          <> Constraints.mustSpendPubKeyOutput (fst sourceTpl)

  ledgerTx <- submitTxConstraintsWith @(ValidatorTypes' t) lookups tx

  return
    ( Ledger.txId ledgerTx
    ,
      ( snapshotPermLeftOutput
      , snapshotPermRightOutput
      )
    )

splitSnapshotPerm' ::
  forall (t :: Type) (w :: Type).
  MapTerms t =>
  (FeeSource, SnapshotPermInfo) ->
  EffC t w (EffC t w [SnapshotPermInfo])
splitSnapshotPerm' (feeSource, (tpl, SnapshotPermEx snapshotPerm (startId, endId))) = do
  mapInstance <- ask
  lkp :: MapLookupSeq (K' t) (V' t) <- get
  let nodes' = mapLookupSeq'nodes lkp
  left <- raiseEnh @t $ fromJust' "Cannot get left node by index" $ nodes' S.!? fromInteger leftId
  right <- raiseEnh @t $ fromJust' "Cannot get right node by index" $ nodes' S.!? fromInteger rightId

  (txId, (leftPerm, rightPerm)) <- raiseEnh @t $ split' @t mapInstance feeSource (tpl, snapshotPerm) left right

  return $ do
    void $ (raiseEnh @t . awaitTxConfirmed') txId

    updateMapLookup txId left right

    leftTpl <- raiseEnh @t $ toTpl' @t txId (SnapshotPermDatum leftPerm)
    rightTpl <- raiseEnh @t $ toTpl' @t txId (SnapshotPermDatum rightPerm)

    raiseEnh @t $
      logInfo @String $
        "Map: splitted: "
          <> show snapshotPerm
          <> ", into: "
          <> show leftPerm
          <> ", and: "
          <> show rightPerm

    return
      [ (leftTpl, SnapshotPermEx leftPerm (startId, leftId))
      , (rightTpl, SnapshotPermEx rightPerm (rightId, endId))
      ]
  where
    leftId :: Integer
    leftId = (startId + endId) `div` 2

    rightId :: Integer
    rightId = leftId + 1

    updateMapLookup :: Ledger.TxId -> (Tpl, Node (K' t) (V' t)) -> (Tpl, Node (K' t) (V' t)) -> EffC t w ()
    updateMapLookup txId left right = do
      lkp :: MapLookupSeq (K' t) (V' t) <- get
      let nodes' = mapLookupSeq'nodes lkp
          version = snapshotPerm'version snapshotPerm
          outputLeft = (snd left){node'lockState = LockedFor version}
          outputRight = (snd right) {node'lockState = LockedFor version}

      leftNodeTpl <- raiseEnh @t $ toTpl' @t txId (NodeDatum outputLeft)
      rightNodeTpl <- raiseEnh @t $ toTpl' @t txId (NodeDatum outputRight)
      let newNodes =
            S.update (fromInteger leftId) (leftNodeTpl, outputLeft) $
              S.update (fromInteger rightId) (rightNodeTpl, outputRight) nodes'

      put (lkp {mapLookupSeq'nodes = newNodes})

splitSnapshotPerm ::
  forall (t :: Type) (w :: Type).
  MapTerms t =>
  [SnapshotPermInfo] ->
  EffC t w [SnapshotPermInfo]
splitSnapshotPerm snapshotPerms = do
  let (points, intervals) = partition isPoint snapshotPerms
  return (points ++)
    <*> if null intervals
      then return []
      else do
        bundled <- raiseEnh @t $ bundleWithFeeSource intervals
        conts <- mapM (splitSnapshotPerm' @t) bundled

        res <- join <$> sequence conts

        splitSnapshotPerm @t res
  where
    isPoint :: SnapshotPermInfo -> Bool
    isPoint (_, SnapshotPermEx (SnapshotPerm start end _) _) = start == end

type NodeSnapshotInfo t = (Tpl, NodeSnapshot (K' t) (V' t))

type UnlockPermInfo = (Tpl, UnlockPerm)

type UnlockInfo = (Tpl, Unlock)

makeSnapshot' ::
  forall (t :: Type) (w :: Type).
  MapTerms t =>
  (FeeSource, SnapshotPermInfo) ->
  EffC t w (EffC t w (NodeSnapshotInfo t))
makeSnapshot' (FeeSource sourceTpl, (tpl, SnapshotPermEx snapshotPerm range)) = do
  mapInstance <- ask
  lkp :: MapLookupSeq (K' t) (V' t) <- get

  let nodes' = mapLookupSeq'nodes lkp
  (nodeInput', nodeInput) <- raiseEnh @t $ fromJust' "Cannot get node by index" $ nodes' S.!? fromInteger (fst range)
  snapshotPointer <-
    raiseEnh @t $
      fromJust' "Cannot find pointer" $
        repackage @t mapInstance <$> findPointer @t mapInstance nodeInput'

  let toSpend = M.fromList [tpl, nodeInput', sourceTpl]
      lookups =
        lookups' @t mapInstance toSpend
          <> Constraints.mintingPolicy (snapshotPolicy' @t mapInstance)
      tokenRedeemer = MakeSnapshot (fst tpl) (fst nodeInput')
      assetClass1Token = Value.assetClassValue (assetClass1 @t mapInstance) (-1)
      snapshotToken = Value.assetClassValue (unSnapshotPointer snapshotPointer) 1
      v = T.snapshotPerm'version snapshotPerm
      nodeOutput = nodeInput{T.node'lockState = SnapshotDone v}

      nodeSnapshot =
        NodeSnapshot
          (snd nodeInput' ^. Ledger.ciTxOutValue)
          nodeOutput
          (repackage @t mapInstance <$> T.node'next nodeInput)
          (snapshotPerm'version snapshotPerm)

      tx =
        Constraints.mustMintValueWithRedeemer
          (Ledger.Redeemer $ Ledger.toBuiltinData @SnapshotTokenRedeemer tokenRedeemer)
          (assetClass1Token <> snapshotToken)
          <> Constraints.mustPayToTheScript (NodeDatum nodeOutput) (snd nodeInput' ^. Ledger.ciTxOutValue)
          <> Constraints.mustPayToTheScript (NodeSnapshotDatum nodeSnapshot) snapshotToken
          <> Constraints.mustSpendScriptOutput
            (fst nodeInput')
            (Ledger.Redeemer $ Ledger.toBuiltinData SnapshotOp)
          <> Constraints.mustSpendScriptOutput
            (fst tpl)
            (Ledger.Redeemer $ Ledger.toBuiltinData SnapshotOp)
          <> Constraints.mustSpendPubKeyOutput (fst sourceTpl)

  ledgerTx <- raiseEnh @t $ submitTxConstraintsWith @(ValidatorTypes' t) lookups tx

  return $ do
    let txId = Ledger.txId ledgerTx

    raiseEnh @t $ awaitTxConfirmed' txId

    updateMapLookup txId nodeOutput

    tpl' <- raiseEnh @t $ toTpl' @t txId (NodeSnapshotDatum nodeSnapshot)

    return (tpl', nodeSnapshot)
  where
    updateMapLookup :: Ledger.TxId -> Node (K' t) (V' t) -> EffC t w ()
    updateMapLookup txId node = do
      lkp :: MapLookupSeq (K' t) (V' t) <- get
      let nodes' = mapLookupSeq'nodes lkp

      nodeTpl <- raiseEnh @t $ toTpl' @t txId (NodeDatum node)
      let newNodes =
            S.update (fromInteger $ fst range) (nodeTpl, node) nodes'

      put (lkp {mapLookupSeq'nodes = newNodes})

makeSnapshot ::
  forall (t :: Type) (w :: Type).
  MapTerms t =>
  [SnapshotPermInfo] ->
  EffC t w [NodeSnapshotInfo t]
makeSnapshot snapshotPerms = do
  bundled <- raiseEnh @t $ bundleWithFeeSource snapshotPerms
  conts <- mapM (makeSnapshot' @t) bundled
  sequence conts

initializeUnlockPerm' ::
  forall (t :: Type) (w :: Type).
  MapTerms t =>
  (FeeSource, NodeSnapshotInfo t) ->
  EffC t w (EffC t w UnlockPermInfo)
initializeUnlockPerm' (FeeSource sourceTpl, (tpl, nodeSnapshot)) = do
  mapInstance <- ask

  pointer <- raiseEnh @t $ ("No pointer" `fromJust'`) $ findPointer' @t mapInstance $ nodeSnapshot'assets nodeSnapshot

  let toSpend = M.fromList [tpl, sourceTpl]
      lookups =
        lookups' @t mapInstance toSpend
          <> Constraints.mintingPolicy (unlockPermPolicy' @t mapInstance)
      tokenRedeemer = InitiateUnlockPerm (fst tpl)
      assetClass2Token = Value.assetClassValue (assetClass2 @t mapInstance) 1
      unlockPerm =
        UnlockPerm
          pointer
          pointer
          (T.node'next $ T.nodeSnapshot'datum nodeSnapshot)
          (T.nodeSnapshot'version nodeSnapshot)

  let tx =
        Constraints.mustMintValueWithRedeemer
          (Ledger.Redeemer $ Ledger.toBuiltinData @UnlockPermTokenRedeemer tokenRedeemer)
          assetClass2Token
          <> Constraints.mustPayToTheScript (NodeSnapshotDatum nodeSnapshot) (snd tpl ^. Ledger.ciTxOutValue)
          <> Constraints.mustPayToTheScript (UnlockPermDatum unlockPerm) assetClass2Token
          <> Constraints.mustSpendScriptOutput
            (fst tpl)
            (Ledger.Redeemer $ Ledger.toBuiltinData UnlockPermOp)
          <> Constraints.mustSpendPubKeyOutput (fst sourceTpl)

  ledgerTx <- raiseEnh @t $ submitTxConstraintsWith @(ValidatorTypes' t) lookups tx

  return $ do
    let txId = Ledger.txId ledgerTx

    raiseEnh @t $ awaitTxConfirmed' txId

    tpl' <- raiseEnh @t $ toTpl' @t txId (UnlockPermDatum unlockPerm)
    return (tpl', unlockPerm)

initializeUnlockPerm ::
  forall (t :: Type) (w :: Type).
  MapTerms t =>
  [NodeSnapshotInfo t] ->
  EffC t w [UnlockPermInfo]
initializeUnlockPerm snapshots = do
  bundled <- raiseEnh @t $ bundleWithFeeSource snapshots
  conts <- mapM (initializeUnlockPerm' @t) bundled
  sequence conts

sortUnlockPermInfo :: [UnlockPermInfo] -> Maybe [UnlockPermInfo]
sortUnlockPermInfo infos =
  do
    first <- maybeFirst
    r <- go first
    if length r == length infos
      then return r
      else Nothing
  where
    pointer2Info = M.fromList $ (\i -> (T.unlockPerm'start $ snd i, i)) <$> infos
    unlocks = snd <$> infos
    (prev2next, next2Prev) =
      foldl'
        ( \(m', m'') i -> case T.unlockPerm'next i of
            Just p ->
              let start = T.unlockPerm'start i
               in (SM.insert start p m', SM.insert p start m'')
            Nothing -> (m', m'')
        )
        (M.empty, M.empty)
        unlocks
    maybeFirst = find (\p -> not $ p `M.member` next2Prev) (T.unlockPerm'start <$> unlocks)

    go :: Pointer -> Maybe [UnlockPermInfo]
    go pointer = do
      info <- pointer `M.lookup` pointer2Info
      let cons = case pointer `M.lookup` prev2next of
            Just next -> go next
            Nothing -> Just []
      (info :) <$> cons

mergeUnlockPerms' ::
  forall (t :: Type) (w :: Type).
  MapTerms t =>
  (FeeSource, (UnlockPermInfo, UnlockPermInfo)) ->
  EffC t w (EffC t w UnlockPermInfo)
mergeUnlockPerms' (FeeSource sourceTpl, (leftUnlockPermInfo, rightUnlockPermInfo)) =
  do
    mapInstance <- ask

    let toSpend = M.fromList [sourceTpl, fst leftUnlockPermInfo, fst rightUnlockPermInfo]
        lookups =
          lookups' @t mapInstance toSpend
            <> Constraints.mintingPolicy (unlockPermPolicy' @t mapInstance)
        tokenRedeemer = MergeUnlockPerm (fst $ fst leftUnlockPermInfo) (fst $ fst rightUnlockPermInfo)
        assetClass2Token = Value.assetClassValue (assetClass2 @t mapInstance) 1
        unlockPerm =
          UnlockPerm
            (T.unlockPerm'start $ snd leftUnlockPermInfo)
            (T.unlockPerm'end $ snd rightUnlockPermInfo)
            (T.unlockPerm'next $ snd rightUnlockPermInfo)
            (T.unlockPerm'version $ snd rightUnlockPermInfo)

    let tx =
          Constraints.mustMintValueWithRedeemer
            (Ledger.Redeemer $ Ledger.toBuiltinData @UnlockPermTokenRedeemer tokenRedeemer)
            (Value.assetClassValue (assetClass2 @t mapInstance) (-1))
            <> Constraints.mustPayToTheScript (UnlockPermDatum unlockPerm) assetClass2Token
            <> Constraints.mustSpendScriptOutput
              (fst $ fst leftUnlockPermInfo)
              (Ledger.Redeemer $ Ledger.toBuiltinData UnlockPermOp)
            <> Constraints.mustSpendScriptOutput
              (fst $ fst rightUnlockPermInfo)
              (Ledger.Redeemer $ Ledger.toBuiltinData UnlockPermOp)
            <> Constraints.mustSpendPubKeyOutput (fst sourceTpl)

    raiseEnh @t $
      logInfo @String $
        "Map: merging: "
          <> show leftUnlockPermInfo
          <> ", and: "
          <> show rightUnlockPermInfo

    ledgerTx <- raiseEnh @t $ submitTxConstraintsWith @(ValidatorTypes' t) lookups tx

    return $ do
      let txId = Ledger.txId ledgerTx

      raiseEnh @t $ awaitTxConfirmed' txId

      tpl' <- raiseEnh @t $ toTpl' @t txId (UnlockPermDatum unlockPerm)
      return (tpl', unlockPerm)

pairing :: forall (a :: Type). [a] -> ([(a, a)], Maybe a)
pairing l = case l of
  x : x' : xs ->
    let (p, p') = pairing xs
     in ((x, x') : p, p')
  [x] -> ([], Just x)
  [] -> ([], Nothing)

mergeUnlockPerms ::
  forall (t :: Type) (w :: Type).
  MapTerms t =>
  [UnlockPermInfo] ->
  EffC t w UnlockPermInfo
mergeUnlockPerms unlockPermInfos = do
  case unlockPermInfos of
    _ : _ : _ ->
      do
        let (paired, maybeLast') = pairing unlockPermInfos
        bundled <- raiseEnh @t $ bundleWithFeeSource paired
        conts <- sequence (mergeUnlockPerms' @t <$> bundled)
        merged <- sequence conts
        mergeUnlockPerms @t (merged <> maybeToList maybeLast')
    [x] -> return x
    _ -> raiseEnh @t $ throwError "Empty"

makeUnlockPerm ::
  forall (t :: Type) (w :: Type).
  MapTerms t =>
  UnlockPermInfo ->
  EffC t w ()
makeUnlockPerm (tpl, _) = do
  mapInstance <- ask

  lkp :: MapLookupSeq (K' t) (V' t) <- get

  let (mapInput', mapInput) = mapLookupSeq'map lkp
  (lastNode', lastNode) <- case mapLookupSeq'nodes lkp of
    _ S.:|> tpl' -> return tpl'
    S.Empty -> raiseEnh @t $ throwError "Cannot retrieve last node"

  let toSpend = M.fromList [tpl, mapInput', lastNode']
      lookups =
        lookups' @t mapInstance toSpend
          <> Constraints.mintingPolicy (unlockPermPolicy' @t mapInstance)
      tokenRedeemer = MakeUnlockPerm (fst lastNode') (fst tpl)
      assetClass2Token = Value.assetClassValue (assetClass2 @t mapInstance) (-1)

  let tx =
        Constraints.mustMintValueWithRedeemer
          (Ledger.Redeemer $ Ledger.toBuiltinData @UnlockPermTokenRedeemer tokenRedeemer)
          assetClass2Token
          <> Constraints.mustPayToTheScript (MapDatum mapInput{map'lockState = T.Unlocked}) (snd mapInput' ^. Ledger.ciTxOutValue)
          <> Constraints.mustPayToTheScript (NodeDatum lastNode{node'lockState = T.Unlocked}) (snd lastNode' ^. Ledger.ciTxOutValue)
          <> Constraints.mustSpendScriptOutput
            (fst mapInput')
            (Ledger.Redeemer $ Ledger.toBuiltinData UnlockPermOp)
          <> Constraints.mustSpendScriptOutput
            (fst lastNode')
            (Ledger.Redeemer $ Ledger.toBuiltinData UnlockPermOp)
          <> Constraints.mustSpendScriptOutput
            (fst tpl)
            (Ledger.Redeemer $ Ledger.toBuiltinData UnlockPermOp)

  ledgerTx <- raiseEnh @t $ submitTxConstraintsWith @(ValidatorTypes' t) lookups tx

  let txId = Ledger.txId ledgerTx

  raiseEnh @t $ awaitTxConfirmed' txId

generateUnlock ::
  forall (t :: Type) (s :: Row Type) (w :: Type).
  MapTerms t =>
  MapInstance ->
  (Tpl, Map) ->
  SnapshotVersion ->
  Contract w s Text UnlockInfo
generateUnlock mapInstance (mapInput', mapInput) snapshotVersion = do
  let toSpend = M.fromList [mapInput']
      lookups =
        lookups' @t mapInstance toSpend
          <> Constraints.mintingPolicy (unlockPolicy' @t mapInstance)
      tokenRedeemer = GenerateUnlock snapshotVersion
      assetClass3Token = Value.assetClassValue (assetClass3 @t mapInstance) 1
      unlock = Unlock snapshotVersion

  let tx =
        Constraints.mustMintValueWithRedeemer
          (Ledger.Redeemer $ Ledger.toBuiltinData @UnlockTokenRedeemer tokenRedeemer)
          assetClass3Token
          <> Constraints.mustPayToTheScript (MapDatum mapInput) (snd mapInput' ^. Ledger.ciTxOutValue)
          <> Constraints.mustPayToTheScript (UnlockDatum unlock) assetClass3Token
          <> Constraints.mustSpendScriptOutput
            (fst mapInput')
            (Ledger.Redeemer $ Ledger.toBuiltinData UnlockOp)

  ledgerTx <- submitTxConstraintsWith @(ValidatorTypes' t) lookups tx

  let txId = Ledger.txId ledgerTx

  awaitTxConfirmed' txId

  tpl' <- toTpl' @t txId (UnlockDatum unlock)

  return (tpl', unlock)

cloneUnlock ::
  forall (t :: Type) (s :: Row Type) (w :: Type).
  MapTerms t =>
  MapInstance ->
  UnlockInfo ->
  Integer ->
  Contract w s Text [UnlockInfo]
cloneUnlock mapInstance (tpl, unlock) number = do
  let toSpend = M.fromList [tpl]
      lookups =
        lookups' @t mapInstance toSpend
          <> Constraints.mintingPolicy (unlockPolicy' @t mapInstance)
      tokenRedeemer = CloneUnlock (fst tpl)
      assetClass3Tokens = Value.assetClassValue (assetClass3 @t mapInstance) (number - 1)
      assetClass3Token = Value.assetClassValue (assetClass3 @t mapInstance) 1
  let tx =
        Constraints.mustMintValueWithRedeemer
          (Ledger.Redeemer $ Ledger.toBuiltinData @UnlockTokenRedeemer tokenRedeemer)
          assetClass3Tokens
          <> mconcat (replicate (fromInteger number) $ Constraints.mustPayToTheScript (UnlockDatum unlock) assetClass3Token)
          <> Constraints.mustSpendScriptOutput
            (fst tpl)
            (Ledger.Redeemer $ Ledger.toBuiltinData UnlockOp)

  ledgerTx <- submitTxConstraintsWith @(ValidatorTypes' t) lookups tx

  let txId = Ledger.txId ledgerTx

  awaitTxConfirmed' txId

  tpls <- toTplList @t txId (UnlockDatum unlock)

  return ((,unlock) <$> tpls)

doUnlock ::
  forall (t :: Type) (s :: Row Type) (w :: Type).
  MapTerms t =>
  MapInstance ->
  ( FeeSource
  , ( UnlockInfo
    , (Tpl, Node (K' t) (V' t))
    )
  ) ->
  Contract w s Text (Contract w s Text ())
doUnlock mapInstance (FeeSource feeTpl, ((unlockTpl, unlock), (tpl, node))) = do
  let toSpend = M.fromList [feeTpl, tpl, unlockTpl]
      lookups =
        lookups' @t mapInstance toSpend
          <> Constraints.mintingPolicy (unlockPolicy' @t mapInstance)
      tokenRedeemer = DoUnlock (fst unlockTpl) (fst tpl)
      assetClass3Token = Value.assetClassValue (assetClass3 @t mapInstance) (-1)
  let tx =
        Constraints.mustMintValueWithRedeemer
          (Ledger.Redeemer $ Ledger.toBuiltinData @UnlockTokenRedeemer tokenRedeemer)
          assetClass3Token
          <> Constraints.mustPayToTheScript (NodeDatum node{node'lockState = T.Unlocked}) (snd tpl ^. Ledger.ciTxOutValue)
          <> Constraints.mustSpendScriptOutput
            (fst tpl)
            (Ledger.Redeemer $ Ledger.toBuiltinData UnlockOp)
          <> Constraints.mustSpendScriptOutput
            (fst unlockTpl)
            (Ledger.Redeemer $ Ledger.toBuiltinData UnlockOp)
          <> Constraints.mustSpendPubKeyOutput (fst feeTpl)

  logInfo @String $
    "Map: do unlock: "
      <> "unlock: "
      <> show unlock
      <> ", node lock state: "
      <> show (node'lockState node)

  if node'lockState node == T.Unlocked
    then return $ return ()
    else do
      ledgerTx <- submitTxConstraintsWith @(ValidatorTypes' t) lookups tx

      return $ do
        let txId = Ledger.txId ledgerTx

        awaitTxConfirmed' txId
