{-# LANGUAGE RecordWildCards #-}

module Test.ArdanaDollar.PriceOracle.OnChain.Model.Exploration (
  genSpaceTreeIO,
) where

import Test.ArdanaDollar.PriceOracle.OnChain.Model.Checker
import Test.ArdanaDollar.PriceOracle.OnChain.Model.Constraints
import Test.ArdanaDollar.PriceOracle.OnChain.Model.Gen
import Test.ArdanaDollar.PriceOracle.OnChain.Model.Parameters

import Control.Monad.Trans.State
import Data.Kind (Type)
import Data.Map qualified as M
import Hedgehog (MonadGen)
import Hedgehog.Gen qualified as Gen
import PlutusTx.Prelude
import Test.Tasty (testGroup)
import Test.Tasty.Runners (TestTree (..))
import Prelude (Bounded (..), IO)

-- TODO rejection sampling is super inefficient. Some effort could be put into more efficient sampling.
-- I don't recommending upping the sample count...
genSpaceTreeIO :: (Integer, Integer) -> IO TestTree
genSpaceTreeIO bounds = do
  s <- runSpaceExplorationIO bounds
  let nps = nameGeneratedParams <$> flattenSpaceExploration s
  return $ testGroup "Price Oracle Validator Generated Test Space Exploration" $ parametricValidatorTest <$> nps

flattenSpaceExploration :: SpaceExploration -> [TestParameters]
flattenSpaceExploration SpaceExploration {..} = shouldPass <> (snd =<< M.toList singleDimFailures) <> detritus

emptySpaceExploration :: SpaceExploration
emptySpaceExploration = SpaceExploration [] M.empty []

runSpaceExplorationIO :: (Integer, Integer) -> IO SpaceExploration
runSpaceExplorationIO coverage =
  Gen.sample $ execStateT (spaceExploration coverage) emptySpaceExploration

data SpaceExploration = SpaceExploration
  { shouldPass :: [TestParameters]
  , singleDimFailures :: M.Map Constraint [TestParameters]
  , detritus :: [TestParameters]
  }

spaceCovered :: Integer -> SpaceExploration -> Bool
spaceCovered coverage_lb SpaceExploration {..} =
  length shouldPass >= coverage_lb
    && length detritus >= coverage_lb
    && and (hasCoverage singleDimFailures <$> [minBound .. maxBound])
  where
    hasCoverage :: M.Map Constraint [TestParameters] -> Constraint -> Bool
    hasCoverage m c =
      case M.lookup c m of
        Nothing -> False
        Just so -> length so >= coverage_lb

insertPointInSpace :: Integer -> TestParameters -> SpaceExploration -> SpaceExploration
insertPointInSpace coverage_ub p s@SpaceExploration {..} =
  case constraintViolations p of
    [] ->
      if length shouldPass < coverage_ub
        then s {shouldPass = p : shouldPass}
        else s
    [c] -> case M.lookup c singleDimFailures of
      Nothing -> s {singleDimFailures = M.insert c [p] singleDimFailures}
      Just so ->
        if length so < coverage_ub
          then s {singleDimFailures = M.insert c (p : so) singleDimFailures}
          else s
    _ ->
      if length detritus < coverage_ub
        then s {detritus = p : detritus}
        else s

spaceExploration ::
  forall (m :: Type -> Type).
  MonadGen m =>
  (Integer, Integer) ->
  StateT SpaceExploration m ()
spaceExploration coverage = do
  s <- get
  if spaceCovered (fst coverage) s
    then return ()
    else do
      p <- genTestParameters
      put $ insertPointInSpace (snd coverage) p s
      spaceExploration coverage
