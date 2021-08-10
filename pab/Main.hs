module Main (main) where

--------------------------------------------------------------------------------

import Data.Kind (Type)
import GHC.Generics (Generic)
import Prelude

--------------------------------------------------------------------------------

import Control.Monad (void, when, (>=>))
import Control.Monad.Freer (Eff, Member, interpret, type (~>))
import Control.Monad.Freer.Error (Error)
import Control.Monad.Freer.Extras.Log (LogMsg)
import Data.Aeson (
  FromJSON (..),
  Options (tagSingleConstructors),
  ToJSON (..),
  defaultOptions,
  genericParseJSON,
  genericToJSON,
 )
import Data.ByteString (ByteString)
import Data.Default (Default (def))
import Data.Foldable (traverse_)
import Data.List (intercalate)
import Data.Map qualified as Map
import Data.Text qualified as Text (unpack)
import Data.Text.Encoding (decodeUtf8')
import Data.Text.Encoding.Error (UnicodeException)
import Data.Text.Prettyprint.Doc (Pretty (..), viaShow)

--------------------------------------------------------------------------------

import Playground.Contract (FormSchema, FunctionSchema)
import Plutus.PAB.Core qualified as PAB
import Plutus.PAB.Effects.Contract (ContractEffect (..))
import Plutus.PAB.Effects.Contract.Builtin (Builtin, SomeBuiltin (..))
import Plutus.PAB.Effects.Contract.Builtin qualified as Builtin
import Plutus.PAB.Monitoring.PABLogMsg (PABMultiAgentMsg)
import Plutus.PAB.Simulator (SimulatorEffectHandlers)
import Plutus.PAB.Simulator qualified as Simulator
import Plutus.PAB.Types (PABError (..))
import Plutus.PAB.Webserver.Server qualified as PAB.Server
import Plutus.V1.Ledger.Value qualified as Ledger
import Plutus.V1.Ledger.Value qualified as Value
import PlutusTx.AssocMap qualified as AssocMap
import Wallet.Emulator.Types (Wallet (..))
import Wallet.Emulator.Wallet qualified as Wallet

--------------------------------------------------------------------------------

import ArdanaDollar.Vault
import Plutus.PAB.PrettyLogger

main :: IO ()
main = void $
  Simulator.runSimulationWith handlers $ do
    Simulator.logString @(Builtin ArdanaContracts)
      "Starting Ardana demo PAB webserver on port 8080. Press enter to exit."
    shutdown <- PAB.Server.startServerDebug

    logTitleSequence

    cVaultId <- Simulator.activateContract (Wallet 1) VaultContract <* Simulator.waitNSlots 2
    logCurrentBalances_

    let callEndpoint endpoint i =
          Simulator.callEndpointOnInstance @Integer cVaultId endpoint i >> Simulator.waitNSlots 3

    -- Init a vault
    logBlueString "Init a vault"
    Simulator.callEndpointOnInstance cVaultId "initializeVault" () >> Simulator.waitNSlots 10
    logCurrentBalances_

    -- MintUSD from the vault (depositing Ada)
    Simulator.waitForEndpoint cVaultId "depositCollateral"
    logBlueString "Deposit Ada as a collateral"
    callEndpoint "depositCollateral" 113_000_000

    Simulator.waitForEndpoint cVaultId "mintDUSD"
    logBlueString "Mint dUSD from the vault"
    callEndpoint "mintDUSD" 100
    logCurrentBalances_

    -- Repay USD to the vault
    Simulator.waitForEndpoint cVaultId "repayDUSD"
    logBlueString "Repay dUSD to the vault"
    callEndpoint "repayDUSD" 100
    logCurrentBalances_

    -- -- Withdraw Ada from the vault
    Simulator.waitForEndpoint cVaultId "withdrawCollateral"
    logBlueString "Withdraw Ada from the vault"
    callEndpoint "withdrawCollateral" 113_000_000 >> Simulator.waitNSlots 10
    logCurrentBalances_

    logBlueString "Balances at the end of the simulation:"
    logCurrentBalances_

    shutdown
  where
    wallets :: [Wallet]
    wallets = Wallet <$> [1]

    logBlueString :: String -> Eff (PAB.PABEffects t (Simulator.SimulatorState t)) ()
    logBlueString s = logPrettyColor (Vibrant Blue) ("[INFO] " ++ s) >> logNewLine

    logCurrentBalances_ :: Eff (PAB.PABEffects t (Simulator.SimulatorState t)) ()
    logCurrentBalances_ = do
      balancesMap <- Simulator.currentBalances
      let balancesList = Map.toList balancesMap
      traverse_ (uncurry $ logWalletBalance wallets) balancesList

data ArdanaContracts = VaultContract deriving stock (Show, Generic)

instance ToJSON ArdanaContracts where
  toJSON =
    genericToJSON
      defaultOptions
        { tagSingleConstructors = True
        }
instance FromJSON ArdanaContracts where
  parseJSON =
    genericParseJSON
      defaultOptions
        { tagSingleConstructors = True
        }

instance Pretty ArdanaContracts where
  pretty = viaShow

handleArdanaContract ::
  forall (effs :: [Type -> Type]).
  ( Member (Error PABError) effs
  , Member (LogMsg (PABMultiAgentMsg (Builtin ArdanaContracts))) effs
  ) =>
  ContractEffect (Builtin ArdanaContracts)
    ~> Eff effs
handleArdanaContract = Builtin.handleBuiltin getSchema getContract
  where
    getSchema :: ArdanaContracts -> [FunctionSchema FormSchema]
    getSchema = \case
      VaultContract -> Builtin.endpointsToSchemas @VaultSchema

    getContract :: ArdanaContracts -> SomeBuiltin
    getContract = \case
      VaultContract -> SomeBuiltin vaultContract

handlers :: SimulatorEffectHandlers (Builtin ArdanaContracts)
handlers =
  Simulator.mkSimulatorHandlers @(Builtin ArdanaContracts) def [VaultContract] $
    interpret handleArdanaContract

-- helper functions
logTitleSequence :: forall (t :: Type). Eff (PAB.PABEffects t (Simulator.SimulatorState t)) ()
logTitleSequence = do
  logWithBg ""
  logWithBg $ spaceStr ++ ardanaName ++ spaceStr
  logWithBg ""
  where
    ardanaName = "Ardana"
    titleWidth = 60
    spaceWidth = (titleWidth - length ardanaName) `div` 2
    spaceStr = [' ' | _ <- [0 .. spaceWidth - 1]]

    logWithBg = logPrettyBgColor titleWidth (Vibrant Blue) (Standard White) >=> const logNewLine

logWalletBalance ::
  forall (t :: Type).
  [Wallet] ->
  Wallet.Entity ->
  Value.Value ->
  Eff (PAB.PABEffects t (Simulator.SimulatorState t)) ()
logWalletBalance availableWallets w val = case w of
  Wallet.WalletEntity w' -> when (w' `elem` availableWallets) logWalletBalance'
  Wallet.ScriptEntity _ -> logWalletBalance'
  _ -> return ()
  where
    logWalletBalance' = do
      logNewLine
      logPrettyBgColor 0 (Standard Blue) (Standard Black) ("  " ++ show w ++ "  ")
      logNewLine
      logPrettyColor (Standard Blue) (formatValue val)
      logNewLine
      logNewLine

formatValue :: Value.Value -> String
formatValue (Value.Value m) =
  intercalate "\n" . filter (not . null) $ showMap . snd <$> AssocMap.toList m
  where
    showMap :: AssocMap.Map Ledger.TokenName Integer -> String
    showMap = formatFirstToken . AssocMap.toList

    formatFirstToken :: [(Value.TokenName, Integer)] -> String
    formatFirstToken (token : _) = formatTokenValue token
    formatFirstToken [] = "\n"

    formatTokenValue :: (Value.TokenName, Integer) -> String
    formatTokenValue (name, value)
      | value == 0 = ""
      | otherwise = case name of
        "" -> padRight ' ' 10 "ADA" ++ " : " ++ show value
        tn -> padRight ' ' 10 (safeTokenNameToString tn) ++ " : " ++ show value

    safeTokenNameToString :: Value.TokenName -> String
    safeTokenNameToString tn@(Value.TokenName n) = case bsToString n of
      Right str -> str
      Left _ -> trimHash (show tn) -- it is a hash of a generated token
    bsToString :: ByteString -> Either UnicodeException String
    bsToString = fmap Text.unpack . decodeUtf8'

    trimHash :: String -> String
    trimHash ('0' : 'x' : rest) = "0x" ++ take 7 rest
    trimHash hash = hash
