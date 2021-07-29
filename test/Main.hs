module Main (main) where

import Test.Tasty (defaultMain, testGroup)
import Prelude

main :: IO ()
main =
  defaultMain $
    testGroup
      "tests"
      []
