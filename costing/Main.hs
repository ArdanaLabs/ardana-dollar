module Main (main) where

import ArdanaDollar.Vault
import Control.Monad (void)
import Data.Default (Default (..))
import Ledger.Index (ValidatorMode (..))
import Plutus.Trace (Command (..), ScriptsConfig (..), writeScriptsTo)
import Plutus.Trace.Emulator
import Wallet.Emulator.Wallet
import Prelude (IO, ($))

main :: IO ()
main = do
  vaultBenchmark

vaultBenchmark :: IO ()
vaultBenchmark =
  void $
    writeScriptsTo
      (ScriptsConfig "./costing-output" (Scripts UnappliedValidators))
      "vault"
      vaultTrace
      def

vaultTrace :: EmulatorTrace ()
vaultTrace = do
  let i = 2_000
  let j = 1_000
  w1 <- activateContractWallet (knownWallet 1) vaultContract
  callEndpoint @"initializeVault" w1 ()
  void $ waitNSlots 10
  callEndpoint @"depositCollateral" w1 i
  void $ waitNSlots 10
  callEndpoint @"withdrawCollateral" w1 i
  void $ waitNSlots 10
  callEndpoint @"mintDUSD" w1 j
  void $ waitNSlots 10
  callEndpoint @"repayDUSD" w1 j
  void $ waitNSlots 10
