module Main (main) where

import System.IO (hSetEncoding, stderr, stdout, utf8)
import Prelude

import Test.ArdanaDollar.BufferAuctionTraceTest (bufferTraceTests)
import Test.ArdanaDollar.Certification.OnChain.Model.Proper
import Test.ArdanaDollar.DanaStakePoolTest
import Test.ArdanaDollar.PriceOracle.OnChain.Model.Proper
import Test.ArdanaDollar.TreasuryTraceTest (treasuryTraceTests)
import Test.ArdanaDollar.TreasuryValidatorTest (treasuryValidatorTests)
import Test.ArdanaDollar.Utils (vaultUnitTests)

import Test.ContractModel.Buffer qualified as BufferModel (contractTests)
import Test.ContractModel.Vault qualified as VaultModel (contractTests)
import Test.Roundtrips.BuiltinData (builtinDataRoundtripTests)
import Test.Roundtrips.JSON (jsonRoundtripTests)
import Test.Tasty (testGroup)

import TastyDefaultMain

main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8

  defaultMain $ \(ContractMaxSuccess cms) ->
    testGroup
      "tests"
      [ certificationTests cms
      , priceOracleTests cms
      , vaultUnitTests
      , bufferTraceTests
      , danaStakePoolTests
      , treasuryTraceTests
      , treasuryValidatorTests
      , jsonRoundtripTests
      , builtinDataRoundtripTests
      , BufferModel.contractTests cms
      , VaultModel.contractTests cms
      ]
