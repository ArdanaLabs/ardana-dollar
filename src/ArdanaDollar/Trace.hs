module ArdanaDollar.Trace (
  vaultTrace,
  runVaultTrace,
) where

import ArdanaDollar.Vault
import Data.Default (Default (..))
import Data.Map.Strict qualified as Map
import Ledger.Ada qualified as Ada
import Plutus.Trace.Emulator (EmulatorTrace)
import Plutus.Trace.Emulator qualified as Emulator
import Wallet.Emulator.Wallet (knownWallet)
import Prelude

vaultTrace :: EmulatorTrace ()
vaultTrace = do
  h1 <- Emulator.activateContractWallet (knownWallet 1) vaultContract
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
config =
  Emulator.EmulatorConfig
    (Left (Map.singleton (knownWallet 1) (Ada.lovelaceValueOf 1_000_000_000)))
    def
    def

runVaultTrace :: IO ()
runVaultTrace = Emulator.runEmulatorTraceIO' def config vaultTrace
