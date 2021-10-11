module Test.AssertUtils (logChecker, logErrorAndThrow) where

--------------------------------------------------------------------------------

import Control.Lens ((^.))
import Control.Monad.Freer.Error (throwError)
import Control.Monad.Freer.Extras qualified as Extras
import Data.Aeson qualified as Aeson
import Prelude

--------------------------------------------------------------------------------

import Plutus.Trace.Emulator (EmulatorTrace)
import Plutus.Trace.Emulator.Types (
  ContractInstanceLog,
  ContractInstanceMsg (ContractLog),
  EmulatorRuntimeError (GenericError),
  cilMessage,
 )
import Wallet.Emulator.MultiAgent (EmulatorTimeEvent, eteEvent)

--------------------------------------------------------------------------------

logChecker :: String -> [EmulatorTimeEvent ContractInstanceLog] -> Bool
logChecker expectedLog = any $ \e -> case e ^. (eteEvent . cilMessage) of
  ContractLog aesonValue -> case (Aeson.fromJSON aesonValue :: Aeson.Result String) of
    Aeson.Error _ -> False
    Aeson.Success l -> l == expectedLog
  _ -> False

logErrorAndThrow :: String -> EmulatorTrace ()
logErrorAndThrow err = do
  _ <- Extras.logError err
  throwError $ GenericError err
