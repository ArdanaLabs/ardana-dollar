module Test.ArdanaDollar.PriceOracle.OnChain.Model.Reflections (
  testReflections,
) where

import Test.ArdanaDollar.PriceOracle.OnChain.Model.Constraints
import Test.ArdanaDollar.PriceOracle.OnChain.Model.Space

import Control.Monad.Trans.Reader
import Data.String (IsString (..))
import Hedgehog
import Prelude

propReflectAll :: Property
propReflectAll =
  property $ do
    violations <- forAll genConstraintList
    params <- forAll $ runReaderT genTestParameters violations
    constraintViolations params === violations

propReflectConstraintViolations :: [Constraint] -> Property
propReflectConstraintViolations violations =
  property $ do
    params <- forAll $ runReaderT genTestParameters violations
    constraintViolations params === violations

testReflections :: IO Bool
testReflections = do
  checkParallel $
    Group "Test.ArdanaDollar.PriceOracle.OnChain.Model.Reflections" $
      [ ("propReflectAll", propReflectAll)
      , ("propReflectSuccesses", propReflectConstraintViolations [])
      ]
        <> [ ( fromString ("propReflectSingleViolation_" <> show violation)
             , propReflectConstraintViolations [violation]
             )
           | violation <- [minBound .. maxBound]
           ]
