module TastyDefaultMain (ContractMaxSuccess (..), defaultMain) where

import Data.Proxy (Proxy (..))
import Data.Tagged (Tagged, untag)
import Data.Typeable (Typeable)
import Options.Applicative
import Prelude

import Test.Tasty (
  TestTree,
  askOption,
  defaultIngredients,
  defaultMainWithIngredients,
  includingOptions,
 )
import Test.Tasty.Options (IsOption (..), OptionDescription (Option), safeRead)

defaultMain :: (ContractMaxSuccess -> TestTree) -> IO ()
defaultMain tt =
  let ings =
        includingOptions [Option (Proxy :: Proxy ContractMaxSuccess)] :
        defaultIngredients
   in defaultMainWithIngredients ings $ askOption tt

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
