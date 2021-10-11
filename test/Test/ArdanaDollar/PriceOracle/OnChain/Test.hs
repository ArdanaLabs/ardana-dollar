module Test.ArdanaDollar.PriceOracle.OnChain.Test (
  genPriceOracleOnChainTestTree,
) where

import Test.ArdanaDollar.PriceOracle.OnChain.Model.Exploration

import Test.Tasty.Runners (TestTree (..))
import Prelude (IO)

genPriceOracleOnChainTestTree :: IO TestTree
genPriceOracleOnChainTestTree = genSpaceTreeIO (3, 9)
