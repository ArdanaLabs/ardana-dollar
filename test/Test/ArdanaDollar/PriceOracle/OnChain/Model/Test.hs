module Test.ArdanaDollar.PriceOracle.OnChain.Model.Test (
  testValidatorProperties
) where

import Test.ArdanaDollar.PriceOracle.OnChain.Model.Space
import Test.ArdanaDollar.PriceOracle.OnChain.Model.Constraints
import Test.ArdanaDollar.PriceOracle.OnChain.Model.Reification

import Hedgehog
import Control.Monad.Trans.Reader
import Prelude
import Data.String (IsString(..))


validatorPropTestAll :: Property
validatorPropTestAll =
  property $ do
    violations <- forAll genConstraintList
    params <- forAll $ runReaderT genTestParameters violations
    result <- runValidatorTest params
    result === True

validatorPropTestConstraintViolations :: [Constraint] -> Property
validatorPropTestConstraintViolations violations =
  property $ do
    params <- forAll $ runReaderT genTestParameters violations
    result <- runValidatorTest params
    result === True

testValidatorProperties :: IO Bool
testValidatorProperties = do
  checkParallel $ Group "Test.ArdanaDollar.PriceOracle.OnChain.Model.Properties" $
    [ ("validatorPropTestAll", validatorPropTestAll)
    , ("validatorPropTestSuccesses", validatorPropTestConstraintViolations [])
    ] <>
    [ (fromString ("validatorPropTestSingleViolation_" <> show violation)
      , validatorPropTestConstraintViolations [violation]
      )
    | violation <- [minBound .. maxBound]
    ]



