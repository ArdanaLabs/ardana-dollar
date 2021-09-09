module Main (main) where

import Test.ArdanaDollar.BufferAuctionTraceTest (bufferTraceTests)
import Test.ArdanaDollar.DanaStakePoolTest
import Test.ArdanaDollar.Utils (vaultUnitTests)
import Test.Roundtrips.JSON (jsonRoundtripTests)
import Test.Tasty (defaultMain, testGroup)
import Prelude (IO, ($))

main :: IO ()
main =
  defaultMain $
    testGroup
      "tests"
      [ jsonRoundtripTests
      , vaultUnitTests
      , bufferTraceTests
      , danaStakePoolTests
      ]
