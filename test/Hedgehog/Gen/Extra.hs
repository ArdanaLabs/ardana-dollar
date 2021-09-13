module Hedgehog.Gen.Extra (integer) where

import Data.Kind (Type)
import Hedgehog (MonadGen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Prelude

integer :: forall (m :: Type -> Type). MonadGen m => m Integer
integer = Gen.integral (Range.linearFrom 0 (-10000) 10000)
