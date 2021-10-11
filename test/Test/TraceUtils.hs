module Test.TraceUtils (getBus) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Semigroup qualified as Semigroup
import Prelude

import Plutus.PAB.OutputBus (OutputBus (getOutputBus))
import Plutus.Trace.Emulator (
  ContractConstraints,
  ContractHandle,
  EmulatorRuntimeError (GenericError),
  EmulatorTrace,
  observableState,
  throwError,
 )

getBus ::
  forall w s e.
  ( ContractConstraints s
  , FromJSON e
  , FromJSON w
  , ToJSON w
  ) =>
  ContractHandle (OutputBus w) s e ->
  EmulatorTrace w
getBus cId = do
  ob <- observableState cId
  case getOutputBus ob of
    Just (Semigroup.Last s) -> return s
    Nothing -> throwError $ GenericError "error in getOutputBus"
