module Test.ArdanaDollar.PriceOracle.OnChain.Test
  ( genPriceOracleOnChainTestTree
  ) where
import Test.ArdanaDollar.PriceOracle.OnChain.Model.Exploration

import Prelude (IO)
import Test.Tasty.Runners (TestTree (..))

genPriceOracleOnChainTestTree :: IO TestTree
genPriceOracleOnChainTestTree = genSpaceTreeIO (3,9)

