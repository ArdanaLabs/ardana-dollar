{-# LANGUAGE TypeFamilies #-}

module Test.ArdanaDollar.Certification.OnChain.Model.Proper (
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
        [ Group "Certification quick check" [("model", quickCheckModelTest Model)] --, ("plutus", quickCheckPlutusTest Model)]
        ]
      longTests = []
   in --        join
      --          [ [ testEnumeratedScenarios Model "Certification validation failure scenarios" combinedTestGivenProperties (Not expect)
      --            , testEnumeratedScenarios Model "Certification validation success scenarios" combinedTestGivenProperties expect
      --            ]
      --          | 75 <= contractMaxSuccesses
      --          ]
      testGroup "Certification" $ fromGroup <$> (shortTests <> longTests)

data CertificationModel = Model deriving (Show)

instance IsProperty (Property CertificationModel)

instance Proper CertificationModel where
  data Model CertificationModel
    = Initialising
    | Updating
    | Copying
    | Destroying
    deriving (Show)
  data Property CertificationModel
    = InitialiseContext
    | UpdateContext
    | CopyCertificationTokenContext
    | DestroyCertificationTokenContext
    deriving stock (Enum, Eq, Ord, Bounded, Show)

  logic = ExactlyOne (Var <$> [InitialiseContext, UpdateContext, CopyCertificationTokenContext, DestroyCertificationTokenContext])
  satisfiesProperty = flip satisfiesProperty'
  genModel = genModel' . Set.toList

-- Model Properties
-------------------------------------------------------------------------------

satisfiesProperty' :: Property CertificationModel -> Model CertificationModel -> Bool
satisfiesProperty' InitialiseContext = isInInitialiseContext
satisfiesProperty' UpdateContext = isInUpdateContext
satisfiesProperty' CopyCertificationTokenContext = isInCopyCertificationTokenContext
satisfiesProperty' DestroyCertificationTokenContext = isInDestroyCertificationTokenContext

type ModelProperty = Model CertificationModel -> Bool

isInInitialiseContext :: ModelProperty
isInInitialiseContext Initialising = True
isInInitialiseContext _ = False

isInUpdateContext :: ModelProperty
isInUpdateContext Updating = True
isInUpdateContext _ = False

isInCopyCertificationTokenContext :: ModelProperty
isInCopyCertificationTokenContext Copying = True
isInCopyCertificationTokenContext _ = False

isInDestroyCertificationTokenContext :: ModelProperty
isInDestroyCertificationTokenContext Destroying = True
isInDestroyCertificationTokenContext _ = False

-- Model Generators
-------------------------------------------------------------------------------

genModel' :: MonadGen m => [Property CertificationModel] -> m (Model CertificationModel)
genModel' props | InitialiseContext `elem` props = runReaderT genInitialisingModel props
genModel' props | UpdateContext `elem` props = runReaderT genUpdatingModel props
genModel' props | CopyCertificationTokenContext `elem` props = runReaderT genCopyingModel props
genModel' props = runReaderT genDestroyingModel props

genInitialisingModel ::
  forall (m :: Type -> Type).
  MonadGen m =>
  ReaderT [Property CertificationModel] m (Model CertificationModel)
genInitialisingModel = pure Initialising

genUpdatingModel ::
  forall (m :: Type -> Type).
  MonadGen m =>
  ReaderT [Property CertificationModel] m (Model CertificationModel)
genUpdatingModel = pure Updating

genCopyingModel ::
  forall (m :: Type -> Type).
  MonadGen m =>
  ReaderT [Property CertificationModel] m (Model CertificationModel)
genCopyingModel = pure Copying

genDestroyingModel ::
  forall (m :: Type -> Type).
  MonadGen m =>
  ReaderT [Property CertificationModel] m (Model CertificationModel)
genDestroyingModel = pure Destroying
