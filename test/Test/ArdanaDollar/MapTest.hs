{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.ArdanaDollar.MapTest (
  testAddToEmptyMap,
  testAddSmallest,
  testAddGreatest,
  testAddInTheMiddle,
  testAddDuplicateKey,
  testRemoveFromOneElementMap,
  testRemoveSmallest,
  testRemoveGreatest,
  testRemoveInTheMiddle,
  testSnapshot,
  mapTests,
  runTrace,
  createMap,
  mapIs,
  emCfg,
) where

import Prelude

import Ledger qualified
import Ledger.Ada as Ada
import Ledger.Value qualified as Value
import Plutus.Contract.Test
import Plutus.Trace.Emulator as Emulator

import Data.Coerce
import Data.Default (Default (..))
import Data.Foldable
import Data.Kind (Type)
import Data.List (sortOn)
import Data.Map qualified as Map
import Data.Maybe
import Data.Monoid
import Data.Text (Text)

import Control.Lens
import Control.Monad (void)

import ArdanaDollar.Map.ContractUtils (address)
import ArdanaDollar.Map.Endpoints
import ArdanaDollar.Map.TestUtils
import ArdanaDollar.Map.TxUtils (isUnlocked)
import ArdanaDollar.Map.Types as Types
import ArdanaDollar.Map.ValidatorsTH (Integer2IntegerMap)

import Test.Tasty

type Key = Integer
type Value = Integer

emCfg :: EmulatorConfig
emCfg = EmulatorConfig (Left $ Map.fromList [(knownWallet 1, Ada.lovelaceValueOf 10_000_000_000)]) def def

runTrace :: EmulatorTrace () -> IO ()
runTrace = runEmulatorTraceIO' def emCfg

mapAC :: Value.AssetClass
mapAC = Value.AssetClass (adaSymbol, adaToken)

mapInstance' :: MapInstance
mapInstance' = MapInstance mapAC

-- tag :: ContractInstanceTag
-- tag = walletInstanceTag $ knownWallet 1

smallestPair :: (Integer, Integer)
smallestPair = (5, 3)

middlePair :: (Integer, Integer)
middlePair = (6, 4)

greatestPair :: (Integer, Integer)
greatestPair = (7, 5)

bigMap :: [(Integer, Integer)]
bigMap = [(1, 0), (2, 3), (7, 4), (3, 0), (0, 1), (8, 5), (11, 1), (4, 0), (5, 9)]

sortedBigMap :: [(Integer, Integer)]
sortedBigMap = sortOn fst bigMap

asNode :: Types.Datum Key Value -> Maybe (Types.Node Key Value)
asNode (Types.NodeDatum n) = Just n
asNode _ = Nothing

asMap :: Types.Datum Key Value -> Maybe Types.Map
asMap (Types.MapDatum m) = Just m
asMap _ = Nothing

asNodeSnapshot :: Types.Datum Key Value -> Maybe (Types.NodeSnapshot Key Value)
asNodeSnapshot (Types.NodeSnapshotDatum m) = Just m
asNodeSnapshot _ = Nothing

asMapSnapshot :: Types.Datum Key Value -> Maybe Types.MapSnapshot
asMapSnapshot (Types.MapSnapshotDatum m) = Just m
asMapSnapshot _ = Nothing

class SthLinked a where
  type PointerT a
  nextPointer :: a -> Maybe (PointerT a)

instance SthLinked (Types.Node k v) where
  type PointerT (Types.Node k v) = Types.Pointer
  nextPointer = Types.node'next

instance SthLinked (Types.NodeSnapshot k v) where
  type PointerT (Types.NodeSnapshot k v) = Types.SnapshotPointer
  nextPointer = Types.nodeSnapshot'next

toSortedList ::
  forall (a :: Type).
  (SthLinked a, Coercible (PointerT a) Ledger.AssetClass) =>
  [(a, Value.Value)] ->
  PointerT a ->
  Maybe [(a, Value.Value)]
toSortedList utxos pointer =
  let f = (\(_, v) -> Value.assetClassValueOf v (coerce pointer) == 1) `filter` utxos
   in case f of
        [(node, value)] ->
          let e = (node, value <> Value.assetClassValue (coerce pointer) (-1))
           in case nextPointer node of
                Just nextPointer' -> (e :) <$> toSortedList utxos nextPointer'
                Nothing -> pure [e]
        _ -> Nothing

toSortedListMap :: [(Types.Datum Key Value, Value.Value)] -> Maybe [(Types.Node Key Value, Value.Value)]
toSortedListMap utxos =
  let f = mapMaybe (\(d, v) -> (,v) <$> asMap d) utxos
      nodes = mapMaybe (\(d, v) -> (,v) <$> asNode d) utxos
   in case f of
        [(d, _)] -> case map'head d of
          Just pointer -> toSortedList nodes pointer
          Nothing -> Just []
        _ -> Nothing

toSortedListSnapshot :: SnapshotVersion -> [(Types.Datum Key Value, Value.Value)] -> Maybe [(Types.NodeSnapshot Key Value, Value.Value)]
toSortedListSnapshot snapshotVersion utxos =
  let f = mapMaybe (\(d, v) -> (,v) <$> asMapSnapshot d) utxos
      nodes = mapMaybe (\(d, v) -> (,v) <$> asNodeSnapshot d) utxos
      head' = (\(s, _) -> mapSnapshot'version s == snapshotVersion) `filter` f
      nodes' = (\(s, _) -> nodeSnapshot'version s == snapshotVersion) `filter` nodes
   in case head' of
        [(d, _)] -> case mapSnapshot'head d of
          Just pointer -> toSortedList nodes' pointer
          Nothing -> Just []
        _ -> Nothing

mapIs :: [((Key, Value), Value.Value)] -> TracePredicate
mapIs expected =
  dataAndValueAtAddress (address @Integer2IntegerMap mapInstance') pred'
  where
    pred' utxos =
      fromMaybe False $ do
        utxos' <- toSortedListMap utxos
        let utxos'' = (\(node, value) -> ((node'key node, node'value node), value)) <$> utxos'
        let unlocked = all (\(node, _) -> isUnlocked (node'lockState node)) utxos'
        return (utxos'' == expected && unlocked)

snapshotIs :: SnapshotVersion -> [((Key, Value), Value.Value)] -> TracePredicate
snapshotIs snapshotVersion expected =
  dataAndValueAtAddress (address @Integer2IntegerMap mapInstance') pred'
  where
    pred' :: [(Datum Key Value, Value.Value)] -> Bool
    pred' utxos =
      maybe
        False
        ( \utxos' ->
            ((\(node, value) -> ((node'key $ nodeSnapshot'datum node, node'value $ nodeSnapshot'datum node), value)) <$> utxos')
              == expected
        )
        (toSortedListSnapshot snapshotVersion utxos)

type ContractHandleT = ContractHandle (Last ()) Schema Text

createMap :: EmulatorTrace MapInstance
createMap = do
  h <- activateContractWallet (knownWallet 1) createEndpoint
  callEndpoint @"createTest" h mapAC
  void $ Emulator.waitNSlots 5
  mapInstance <- getLast <$> observableState h
  case mapInstance of
    Just m -> return m
    Nothing -> throwError $ GenericError "Could not create map"

testAddToEmptyMap :: EmulatorTrace ContractHandleT
testAddToEmptyMap = do
  mapInstance <- createMap

  h1 <- activateContractWallet (knownWallet 1) (endpoints mapInstance)
  void $ Emulator.waitNSlots 1
  callEndpoint @"insert" h1 smallestPair
  void $ Emulator.waitNSlots 1

  return h1

testAddDuplicateKey :: EmulatorTrace ContractHandleT
testAddDuplicateKey = do
  mapInstance <- createMap

  h1 <- activateContractWallet (knownWallet 1) (endpoints mapInstance)
  void $ Emulator.waitNSlots 1
  callEndpoint @"insert" h1 smallestPair
  void $ Emulator.waitNSlots 1
  callEndpoint @"insert" h1 smallestPair
  void $ Emulator.waitNSlots 1

  return h1

testAddSmallest :: EmulatorTrace ContractHandleT
testAddSmallest = do
  mapInstance <- createMap

  h1 <- activateContractWallet (knownWallet 1) (endpoints mapInstance)
  void $ Emulator.waitNSlots 1
  callEndpoint @"insert" h1 middlePair
  void $ Emulator.waitNSlots 1
  callEndpoint @"insert" h1 smallestPair
  void $ Emulator.waitNSlots 1

  return h1

testAddGreatest :: EmulatorTrace ContractHandleT
testAddGreatest = do
  mapInstance <- createMap

  h1 <- activateContractWallet (knownWallet 1) (endpoints mapInstance)
  void $ Emulator.waitNSlots 1
  callEndpoint @"insert" h1 middlePair
  void $ Emulator.waitNSlots 1
  callEndpoint @"insert" h1 greatestPair
  void $ Emulator.waitNSlots 1

  return h1

testAddInTheMiddle :: EmulatorTrace ContractHandleT
testAddInTheMiddle = do
  mapInstance <- createMap

  h1 <- activateContractWallet (knownWallet 1) (endpoints mapInstance)
  void $ Emulator.waitNSlots 1
  callEndpoint @"insert" h1 smallestPair
  void $ Emulator.waitNSlots 1
  callEndpoint @"insert" h1 greatestPair
  void $ Emulator.waitNSlots 1
  callEndpoint @"insert" h1 middlePair
  void $ Emulator.waitNSlots 1

  return h1

testRemoveFromOneElementMap :: EmulatorTrace ContractHandleT
testRemoveFromOneElementMap = do
  h1 <- testAddToEmptyMap

  callEndpoint @"remove" h1 (fst smallestPair)
  void $ Emulator.waitNSlots 1

  return h1

testRemoveSmallest :: EmulatorTrace ()
testRemoveSmallest = do
  h1 <- testAddSmallest

  callEndpoint @"remove" h1 (fst smallestPair)
  void $ Emulator.waitNSlots 1

testRemoveGreatest :: EmulatorTrace ()
testRemoveGreatest = do
  h1 <- testAddGreatest

  callEndpoint @"remove" h1 (fst greatestPair)
  void $ Emulator.waitNSlots 1

testRemoveInTheMiddle :: EmulatorTrace ()
testRemoveInTheMiddle = do
  h1 <- testAddInTheMiddle

  callEndpoint @"remove" h1 (fst middlePair)
  void $ Emulator.waitNSlots 1

testIncrement :: EmulatorTrace ContractHandleT
testIncrement = do
  h1 <- testAddToEmptyMap

  callEndpoint @"increment" h1 (fst smallestPair)
  void $ Emulator.waitNSlots 1

  return h1

testSnapshot :: EmulatorTrace ContractHandleT
testSnapshot = do
  mapInstance <- createMap

  h1 <- activateContractWallet (knownWallet 1) (endpoints mapInstance)

  traverse_
    ( \pair -> do
        void $ Emulator.waitNSlots 1
        callEndpoint @"insert" h1 pair
    )
    bigMap

  void $ Emulator.waitNSlots 30

  callEndpoint @"createSnapshot" h1 ()
  void $ Emulator.waitUntilSlot 65

  return h1

testSnapshot1 :: EmulatorTrace ContractHandleT
testSnapshot1 = do
  h1 <- testSnapshot

  traverse_
    ( \(key, _) -> do
        void $ Emulator.waitNSlots 1
        callEndpoint @"increment" h1 key
    )
    bigMap

  void $ Emulator.waitNSlots 30

  callEndpoint @"createSnapshot" h1 ()
  void $ Emulator.waitNSlots 60

  return h1

testAddToEmptyMap' :: TestTree
testAddToEmptyMap' =
  checkPredicateOptions
    (defaultCheckOptions & emulatorConfig .~ emCfg)
    "testAddToEmptyMap"
    ( assertNoFailedTransactions
        .&&. mapIs [(smallestPair, mempty)]
    )
    (void testAddToEmptyMap)

testAddSmallest' :: TestTree
testAddSmallest' =
  checkPredicateOptions
    (defaultCheckOptions & emulatorConfig .~ emCfg)
    "testAddSmallest"
    ( assertNoFailedTransactions
        .&&. mapIs [(smallestPair, mempty), (middlePair, mempty)]
    )
    (void testAddSmallest)

testAddGreatest' :: TestTree
testAddGreatest' =
  checkPredicateOptions
    (defaultCheckOptions & emulatorConfig .~ emCfg)
    "testAddGreatest"
    ( assertNoFailedTransactions
        .&&. mapIs [(middlePair, mempty), (greatestPair, mempty)]
    )
    (void testAddGreatest)

testAddInTheMiddle' :: TestTree
testAddInTheMiddle' =
  checkPredicateOptions
    (defaultCheckOptions & emulatorConfig .~ emCfg)
    "testAddInTheMiddle"
    ( assertNoFailedTransactions
        .&&. mapIs [(smallestPair, mempty), (middlePair, mempty), (greatestPair, mempty)]
    )
    (void testAddInTheMiddle)

testAddDuplicateKey' :: TestTree
testAddDuplicateKey' =
  checkPredicateOptions
    (defaultCheckOptions & emulatorConfig .~ emCfg)
    "testAddDuplicateKey"
    ( assertNoFailedTransactions
        .&&. mapIs [(smallestPair, mempty)]
    )
    (void testAddDuplicateKey)

testRemoveFromOneElementMap' :: TestTree
testRemoveFromOneElementMap' =
  checkPredicateOptions
    (defaultCheckOptions & emulatorConfig .~ emCfg)
    "testRemoveFromOneElementMap"
    ( assertNoFailedTransactions
        .&&. mapIs []
    )
    (void testRemoveFromOneElementMap)

testRemoveSmallest' :: TestTree
testRemoveSmallest' =
  checkPredicateOptions
    (defaultCheckOptions & emulatorConfig .~ emCfg)
    "testRemoveSmallest"
    ( assertNoFailedTransactions
        .&&. mapIs [(middlePair, mempty)]
    )
    (void testRemoveSmallest)

testRemoveGreatest' :: TestTree
testRemoveGreatest' =
  checkPredicateOptions
    (defaultCheckOptions & emulatorConfig .~ emCfg)
    "testRemoveGreatest"
    ( assertNoFailedTransactions
        .&&. mapIs [(middlePair, mempty)]
    )
    (void testRemoveGreatest)

testRemoveInTheMiddle' :: TestTree
testRemoveInTheMiddle' =
  checkPredicateOptions
    (defaultCheckOptions & emulatorConfig .~ emCfg)
    "testRemoveInTheMiddle"
    ( assertNoFailedTransactions
        .&&. mapIs [(smallestPair, mempty), (greatestPair, mempty)]
    )
    (void testRemoveInTheMiddle)

testIncrement' :: TestTree
testIncrement' =
  checkPredicateOptions
    (defaultCheckOptions & emulatorConfig .~ emCfg)
    "testIncrement"
    ( assertNoFailedTransactions
        .&&. mapIs [((fst smallestPair, snd smallestPair + 1), mempty)]
    )
    (void testIncrement)

testSnapshot' :: TestTree
testSnapshot' =
  checkPredicateOptions
    (defaultCheckOptions & emulatorConfig .~ emCfg)
    "testSnapshot"
    ( let expected = (,mempty) <$> sortedBigMap
       in assertNoFailedTransactions
            .&&. snapshotIs (SnapshotVersion 0) expected
            .&&. mapIs expected
    )
    (void testSnapshot)

testSnapshot1' :: TestTree
testSnapshot1' =
  checkPredicateOptions
    (defaultCheckOptions & emulatorConfig .~ emCfg)
    "testSnapshot1"
    ( let expected' = (,mempty) <$> sortedBigMap
          expected = ((,mempty) . ((+ 1) <$>) <$> sortedBigMap)
       in assertNoFailedTransactions
            .&&. snapshotIs (SnapshotVersion 0) expected'
            .&&. snapshotIs (SnapshotVersion 1) expected
            .&&. mapIs expected
    )
    (void testSnapshot1)

mapTests :: TestTree
mapTests =
  testGroup
    "map tests"
    [ testAddToEmptyMap'
    , testAddSmallest'
    , testAddGreatest'
    , testAddInTheMiddle'
    , testAddDuplicateKey'
    , testRemoveFromOneElementMap'
    , testRemoveSmallest'
    , testRemoveGreatest'
    , testRemoveInTheMiddle'
    , testIncrement'
    , testSnapshot'
    , testSnapshot1'
    ]
