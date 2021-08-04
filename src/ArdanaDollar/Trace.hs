module ArdanaDollar.Trace (
  vaultTrace,
  runVaultTrace
) where

import Prelude
import qualified Data.Map.Strict as Map
import Plutus.Trace.Emulator (EmulatorTrace)
import qualified Plutus.Trace.Emulator as Emulator
import qualified Ledger.Ada as Ada
import Wallet.Emulator.Types (Wallet (..))
import Data.Default (Default (..))
import ArdanaDollar.Vault


vaultTrace :: EmulatorTrace ()
vaultTrace = do
  h1 <- Emulator.activateContractWallet (Wallet 1) vaultContract
  _ <- Emulator.waitNSlots 10
  Emulator.callEndpoint @"initializeVault" h1 ()
  _ <- Emulator.waitNSlots 10
  Emulator.callEndpoint @"depositCollateral" h1 100_000_000
  _ <- Emulator.waitNSlots 10
  Emulator.callEndpoint @"mintDUSD" h1 5000
  _ <- Emulator.waitNSlots 10
  Emulator.callEndpoint @"repayDUSD" h1 5000
  _ <- Emulator.waitNSlots 10
  Emulator.callEndpoint @"withdrawCollateral" h1 100_000_000
  _ <- Emulator.waitNSlots 10
  return ()

config :: Emulator.EmulatorConfig
config = Emulator.EmulatorConfig (Left (Map.singleton (Wallet 1) (Ada.lovelaceValueOf 1_000_000_000)))

runVaultTrace :: IO ()
runVaultTrace = Emulator.runEmulatorTraceIO' def config def vaultTrace
