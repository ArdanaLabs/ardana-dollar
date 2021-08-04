module Main (main) where

import Prelude (IO, ($))
import Test.Tasty (defaultMain, testGroup)
import Test.ArdanaDollar.Utils (vaultUnitTests)


main :: IO ()
main = defaultMain $ testGroup "tests"
  [ vaultUnitTests ]
