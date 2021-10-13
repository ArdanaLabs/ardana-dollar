module Main (main) where

import Data.Proxy (Proxy (..))
import Data.Tagged (Tagged, untag)
import Data.Typeable (Typeable)
import Options.Applicative
import System.IO (hSetEncoding, stderr, stdout, utf8)
import Prelude

import Test.ArdanaDollar.BufferAuctionTraceTest (bufferTraceTests)
import Test.ArdanaDollar.DanaStakePoolTest
import Test.ArdanaDollar.PriceOracle.OnChain.Model.Reflections
import Test.ArdanaDollar.PriceOracle.OnChain.Model.Test
import Test.ArdanaDollar.PriceOracle.OnChain.Model.Proper
import Test.ArdanaDollar.TreasuryTraceTest (treasuryTraceTests)
import Test.ArdanaDollar.Utils (vaultUnitTests)
import Test.ContractModel.Buffer qualified as BufferModel (contractTests)
import Test.ContractModel.Vault qualified as VaultModel (contractTests)
import Test.Roundtrips.BuiltinData (builtinDataRoundtripTests)
import Test.Roundtrips.JSON (jsonRoundtripTests)
import Test.Tasty (
  askOption,
  defaultIngredients,
  defaultMainWithIngredients,
  includingOptions,
  testGroup,
 )
import Test.Tasty.Options (IsOption (..), OptionDescription (Option), safeRead)

main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8

  priceOracleTest
  _ <- testReflections
  _ <- testValidatorProperties

  let ings =
        includingOptions [Option (Proxy :: Proxy ContractMaxSuccess)] :
        defaultIngredients

  defaultMainWithIngredients ings $
    askOption $ \(ContractMaxSuccess cms) ->
      testGroup
        "tests"
        [ vaultUnitTests
        , bufferTraceTests
        , danaStakePoolTests
        , treasuryTraceTests
        , jsonRoundtripTests
        , builtinDataRoundtripTests
        , BufferModel.contractTests cms
        , VaultModel.contractTests cms
        ]

newtype ContractMaxSuccess = ContractMaxSuccess Int
  deriving (Eq, Ord, Typeable)

instance IsOption ContractMaxSuccess where
  defaultValue = ContractMaxSuccess 100
  parseValue = fmap ContractMaxSuccess . safeRead
  optionName = "contractMaxSuccess"
  optionHelp = "QuickCheck's `maxSuccess` parameter for contract-model testing"
  optionCLParser =
    fmap ContractMaxSuccess $
      option
        auto
        ( long (untag (optionName :: Tagged ContractMaxSuccess String))
            <> help (untag (optionHelp :: Tagged ContractMaxSuccess String))
        )
