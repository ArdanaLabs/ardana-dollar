module Main (main) where

import System.IO (hSetEncoding, stderr, stdout, utf8)
import Test.ArdanaDollar.BufferAuctionTraceTest (bufferTraceTests)
import Test.ArdanaDollar.DanaStakePoolTest
import Test.ArdanaDollar.Utils (vaultUnitTests)
import Test.Roundtrips.BuiltinData (builtinDataRoundtripTests)
import Test.Roundtrips.JSON (jsonRoundtripTests)
import Test.ContractModel.Vault qualified as VaultModel (contractTests)
import Test.Tasty (defaultMain, testGroup)
import Prelude (IO, ($))

main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  defaultMain $
    testGroup
      "tests"
      [ vaultUnitTests
      , bufferTraceTests
      , danaStakePoolTests
      , jsonRoundtripTests
      , builtinDataRoundtripTests
      , VaultModel.contractTests
      ]
