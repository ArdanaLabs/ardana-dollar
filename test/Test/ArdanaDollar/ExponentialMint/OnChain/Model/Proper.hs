{-# LANGUAGE TypeFamilies #-}

module Test.ArdanaDollar.ExponentialMint.OnChain.Model.Proper (
  certificationTests,
) where

import Control.Monad.Trans.Reader (
  ReaderT (runReaderT),
 )
import Data.Kind (Type)
import Data.Set qualified as Set
import Hedgehog (
  Group (..),
  MonadGen,
 )
import Proper.Plutus
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (fromGroup)
import Prelude (
  Bool (..),
  Bounded,
  Enum,
  Eq,
  Int,
  Ord,
  Show,
  elem,
  flip,
  pure,
  ($),
  (.),
  (<$>),
  (<>),
 )

certificationTests :: Int -> TestTree
certificationTests _contractMaxSuccesses =
  let shortTests =
        [ Group "ExponentialMint quick check" [("model", quickCheckModelTest Model)] --, ("plutus", quickCheckPlutusTest Model)]
        ]
      longTests = []
   in --        join
      --          [ [ testEnumeratedScenarios Model "ExponentialMint validation failure scenarios" combinedTestGivenProperties (Not expect)
      --            , testEnumeratedScenarios Model "ExponentialMint validation success scenarios" combinedTestGivenProperties expect
      --            ]
      --          | 75 <= contractMaxSuccesses
      --          ]
      testGroup "ExponentialMint" $ fromGroup <$> (shortTests <> longTests)

data ExponentialMintModel = Model deriving (Show)

instance IsProperty (Property ExponentialMintModel)

instance Proper ExponentialMintModel where
  data Model ExponentialMintModel
    = Initialising
    | Seeding
    | Copying
    | Destroying
    deriving (Show)
  data Property ExponentialMintModel
    = InitialisingContext
    | --    | HasOneShotUTXOInput
      --    | HasExponentialMintAuthorityTokenInOutput

      SeedingContext
    | CopyingContext
    | DestroyingContext
    deriving stock (Enum, Eq, Ord, Bounded, Show)

  logic =
    All
      [ ExactlyOne (Var <$> [InitialisingContext, SeedingContext, CopyingContext, DestroyingContext])
      --    , Some (Var <$> [HasOneShotUTXOInput,HasExponentialMintAuthorityTokenInOutput]) :->: Var InitialisingContext
      ]

  --  expect = All
  --    [ Var InitialisingContext :->: All (Var <$> [HasOneShotUTXOInput,HasExponentialMintAuthorityTokenInOutput])
  --    ]
  satisfiesProperty = flip satisfiesProperty'
  genModel = genModel' . Set.toList

-- Model Properties
-------------------------------------------------------------------------------

satisfiesProperty' :: Property ExponentialMintModel -> Model ExponentialMintModel -> Bool
satisfiesProperty' InitialisingContext = isInInitialisingContext
satisfiesProperty' SeedingContext = isInSeedingContext
satisfiesProperty' CopyingContext = isInCopyingContext
satisfiesProperty' DestroyingContext = isInDestroyingContext

type ModelProperty = Model ExponentialMintModel -> Bool

isInInitialisingContext :: ModelProperty
isInInitialisingContext Initialising = True
isInInitialisingContext _ = False

isInSeedingContext :: ModelProperty
isInSeedingContext Seeding = True
isInSeedingContext _ = False

isInCopyingContext :: ModelProperty
isInCopyingContext Copying = True
isInCopyingContext _ = False

isInDestroyingContext :: ModelProperty
isInDestroyingContext Destroying = True
isInDestroyingContext _ = False

-- Model Generators
-------------------------------------------------------------------------------

genModel' :: MonadGen m => [Property ExponentialMintModel] -> m (Model ExponentialMintModel)
genModel' props | InitialisingContext `elem` props = runReaderT genInitialisingModel props
genModel' props | SeedingContext `elem` props = runReaderT genSeedingModel props
genModel' props | CopyingContext `elem` props = runReaderT genCopyingModel props
genModel' props = runReaderT genDestroyingModel props

genInitialisingModel ::
  forall (m :: Type -> Type).
  MonadGen m =>
  ReaderT [Property ExponentialMintModel] m (Model ExponentialMintModel)
genInitialisingModel = pure Initialising

genSeedingModel ::
  forall (m :: Type -> Type).
  MonadGen m =>
  ReaderT [Property ExponentialMintModel] m (Model ExponentialMintModel)
genSeedingModel = pure Seeding

genCopyingModel ::
  forall (m :: Type -> Type).
  MonadGen m =>
  ReaderT [Property ExponentialMintModel] m (Model ExponentialMintModel)
genCopyingModel = pure Copying

genDestroyingModel ::
  forall (m :: Type -> Type).
  MonadGen m =>
  ReaderT [Property ExponentialMintModel] m (Model ExponentialMintModel)
genDestroyingModel = pure Destroying
