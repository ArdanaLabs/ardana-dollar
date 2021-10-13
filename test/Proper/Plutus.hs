{-# LANGUAGE TypeFamilies #-}
module Proper.Plutus (
  Proper(..),
  ) where
import Hedgehog
  ( MonadGen,
    Property,
    MonadTest,
    forAll,
    property,
    (===),
    Group (..),
  )
import qualified Hedgehog.Gen as Gen
import Prelude
  ( Bool(..),
    Bounded(..),
    Enum,
    Eq,
    Ord,
    Show (..),
    filter,
    not,
    (.),
    ($),
    (<>),
  )
import Data.Kind
  ( Type,
  )
import Data.String
  ( fromString,
  )

class (Enum c, Eq c, Ord c, Bounded c, Show c) => IsCheck c

class Proper model where
  data Model model :: Type
  data Check model :: Type
  check :: Model model -> Check model -> Bool
  genModel :: MonadGen m => [Check model] -> m (Model model)
  reify :: MonadTest t => Model model -> t () -- this is where you put the plutus core testing
                                              -- left super general for now - I think there is a cleaner way though

  checkViolations :: IsCheck (Check model)
                  => Model model -> [Check model]
  checkViolations x = filter (not . check x) [minBound .. maxBound]

  genChecks :: MonadGen m
            => IsCheck (Check model)
            => model -> m [Check model]
  genChecks _ = Gen.subsequence [minBound .. maxBound]

  selfTestAll :: IsCheck (Check model)
              => Show (Model model)
              => model -> Property
  selfTestAll m =
    property $ do
      violations <- forAll $ genChecks m
      model <- forAll $ genModel violations
      checkViolations model === violations

  selfTestGivenViolations :: IsCheck (Check model)
                          => Show (Model model)
                          => [Check model] -> Property
  selfTestGivenViolations violations =
    property $ do
      model <- forAll $ genModel violations
      checkViolations model === violations

  selfTestGroup :: IsCheck (Check model)
                => Show (Model model)
                => Show model
                => model -> Group
  selfTestGroup model = Group (fromString $ show model) $
    [ (fromString $ "selfTestAll_" <> show model, selfTestAll model)
    , (fromString $ "selfTestSuccesses_" <> show model, selfTestGivenViolations ([] :: [Check model]))
    ] <>
    [ (fromString $ "selfTestSingleViolation_" <> show model <> "_" <> show violation
      , selfTestGivenViolations [violation])
    | violation <- ([minBound .. maxBound] :: [Check model])
    ]

  validatorTestAll :: IsCheck (Check model)
                   => Show (Model model)
                   => model -> Property
  validatorTestAll m =
    property $ do
      violations <- forAll $ genChecks m
      model <- forAll $ genModel violations
      reify model

  validatorTestGivenViolations :: IsCheck (Check model)
                          => Show (Model model)
                          => [Check model] -> Property
  validatorTestGivenViolations violations =
    property $ do
      model <- forAll $ genModel violations
      reify model

  validatorTestGroup :: IsCheck (Check model)
                => Show (Model model)
                => Show model
                => model -> Group
  validatorTestGroup model = Group (fromString $ show model) $
    [ (fromString $ "validatorTestAll_" <> show model, validatorTestAll model)
    , (fromString $ "validatorTestSuccesses_" <> show model, validatorTestGivenViolations ([] :: [Check model]))
    ] <>
    [ (fromString $ "validatorTestSingleViolation_" <> show model <> "_" <> show violation
      , validatorTestGivenViolations [violation])
    | violation <- ([minBound .. maxBound] :: [Check model])
    ]


