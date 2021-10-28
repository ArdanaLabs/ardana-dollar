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
  mapTests,
  runTrace,
  createMap,
  mapIs,
) where

import Prelude

import Ledger.Ada as Ada
import Ledger.Value qualified as Value
import Plutus.Contract.Test
import Plutus.Trace.Emulator as Emulator

import Data.Default (Default (..))
import Data.Map qualified as Map
import Data.Maybe
import Data.Monoid
import Data.Text (Text)

import Control.Lens
import Control.Monad (void)

import ArdanaDollar.Map.Contracts (address)
import ArdanaDollar.Map.Endpoints
import ArdanaDollar.Map.TestUtils
import ArdanaDollar.Map.Types as Types
import ArdanaDollar.Map.ValidatorsTH (Integer2IntegerMap)
import Test.Tasty

type Key = Integer
type Value = Integer

emCfg :: EmulatorConfig
emCfg = EmulatorConfig (Left $ Map.fromList [(knownWallet 1, Ada.lovelaceValueOf 100_000_000)]) def def

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

asNode :: Types.Datum Key Value -> Maybe (Types.Node Key Value)
asNode (Types.NodeDatum n) = Just n
asNode _ = Nothing

asMap :: Types.Datum Key Value -> Maybe Types.Map
asMap (Types.MapDatum m) = Just m
asMap _ = Nothing

toSortedList :: [(Types.Node Key Value, Value.Value)] -> Types.Pointer -> Maybe [((Key, Value), Value.Value)]
toSortedList utxos pointer =
  let f = (\(_, v) -> Value.assetClassValueOf v (unPointer pointer) == 1) `filter` utxos
   in case f of
        [(node, value)] ->
          let e = ((node'key node, node'value node), value <> Value.assetClassValue (unPointer pointer) (-1))
           in case node'next node of
                Just nextPointer -> (e :) <$> toSortedList utxos nextPointer
                Nothing -> pure [e]
        _ -> Nothing

toSortedList' :: [(Types.Datum Key Value, Value.Value)] -> Maybe [((Key, Value), Value.Value)]
toSortedList' utxos =
  let f = utxos >>= (\(d, v) -> maybeToList $ (,v) <$> asMap d)
      nodes = utxos >>= (\(d, v) -> maybeToList $ (,v) <$> asNode d)
   in case f of
        [(d, _)] -> case map'head d of
          Just pointer -> toSortedList nodes pointer
          Nothing -> Just []
        _ -> Nothing

mapIs :: [((Key, Value), Value.Value)] -> TracePredicate
mapIs expected =
  dataAndValueAtAddress (address @Integer2IntegerMap mapInstance') pred'
  where
    pred' utxos = toSortedList' utxos == Just expected

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
    ]
