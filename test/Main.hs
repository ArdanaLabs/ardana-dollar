module Main (main) where

import Test.ArdanaDollar.TreasuryTraceTest (treasuryTraceTests)
import Test.ArdanaDollar.Utils (vaultUnitTests)
import Test.Tasty (defaultMain, testGroup)
import Prelude (IO, ($))

main :: IO ()
main =
  defaultMain $
    testGroup
      "tests"
      [ vaultUnitTests
      , treasuryTraceTests
      ]
