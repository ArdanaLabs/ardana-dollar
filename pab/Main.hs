{-# LANGUAGE DeriveAnyClass #-}

module Main (main) where

--------------------------------------------------------------------------------

import Data.Kind (Type)
import Data.Semigroup qualified as Semigroup
import GHC.Generics (Generic)
import Prelude

--------------------------------------------------------------------------------

import Control.Monad (void, when, (>=>))
import Control.Monad.Freer (Eff, Member, interpret, type (~>))
import Control.Monad.Freer.Error (Error)
import Control.Monad.Freer.Extras.Log (LogMsg)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson (
  FromJSON,
  Result (Success),
  ToJSON,
  fromJSON,
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
import Data.Vector (Vector)

--------------------------------------------------------------------------------

import Ledger qualified
import Playground.Contract (FormSchema, FunctionSchema)
import Plutus.Contract (ContractError, ContractInstanceId, EmptySchema)
import Plutus.PAB.Core qualified as PAB
import Plutus.PAB.Effects.Contract (ContractEffect (..))
import Plutus.PAB.Effects.Contract.Builtin (Builtin, SomeBuiltin (..))
import Plutus.PAB.Effects.Contract.Builtin qualified as Builtin
import Plutus.PAB.Monitoring.PABLogMsg (PABMultiAgentMsg)
import Plutus.PAB.Simulator (SimulatorEffectHandlers)
import Plutus.PAB.Simulator qualified as Simulator
import Plutus.PAB.Types (PABError (..))
import Plutus.PAB.Webserver.Server qualified as PAB.Server
import Plutus.V1.Ledger.Value qualified as Value
import PlutusTx.AssocMap qualified as AssocMap
import Wallet.Emulator.Types (Wallet (..))
import Wallet.Emulator.Wallet qualified as Wallet

--------------------------------------------------------------------------------

import ArdanaDollar.Buffer.Endpoints
import ArdanaDollar.Treasury.Endpoints
import ArdanaDollar.Treasury.Types (Treasury, TreasuryDepositParams (..))
import ArdanaDollar.Vault

import Plutus.PAB.OutputBus
import Plutus.PAB.PrettyLogger

getBus ::
  forall w.
  (FromJSON w) =>
  ContractInstanceId ->
  Simulator.Simulation (Builtin ArdanaContracts) w
getBus cId = flip Simulator.waitForState cId $ \json -> case fromJSON json of
  Success ob -> case getOutputBus ob of
    Just (Semigroup.Last s) -> Just s
    _ -> Nothing
  _ -> Nothing

waitAndCallEndpoint ::
  forall (t :: Type).
  ContractInstanceId ->
  String ->
  Integer ->
  Eff (PAB.PABEffects t (Simulator.SimulatorState t)) ()
waitAndCallEndpoint cid endpoint i = do
  Simulator.waitForEndpoint cid endpoint
  _ <- Simulator.callEndpointOnInstance @Integer cid endpoint i
  Simulator.waitNSlots 10

main :: IO ()
main = void $
  Simulator.runSimulationWith handlers $ do
    Simulator.logString @(Builtin ArdanaContracts)
      "Starting Ardana demo PAB webserver on port 8080. Press enter to exit."
    shutdown <- PAB.Server.startServerDebug

    logTitleSequence

    cVaultId <- Simulator.activateContract (Wallet 2) VaultContract <* Simulator.waitNSlots 2
    logCurrentBalances_

    -- Init a vault
    let callVaultEndpoint = waitAndCallEndpoint cVaultId
    logBlueString "Mint dUSD"
    Simulator.callEndpointOnInstance cVaultId "initializeVault" () >> Simulator.waitNSlots 10
    callVaultEndpoint "depositCollateral" 113_000_000
    callVaultEndpoint "mintDUSD" 200
    logCurrentBalances_

    -- Treasury
    logBlueString "Init treasury"
    cTreasuryId <- Simulator.activateContract (Wallet 1) TreasuryStart
    Simulator.waitNSlots 10
    treasury <- getBus @Treasury cTreasuryId
    logCurrentBalances_

    -- Deposit funds in cost centers
    logBlueString "Deposit funds in cost centers"
    cTreasuryUserId <- Simulator.activateContract (Wallet 2) (TreasuryContract treasury)
    Simulator.waitNSlots 10
    let callDepositEndpoint cc = do
          let params =
                TreasuryDepositParams
                  { treasuryDepositAmount = 10
                  , treasuryDepositCurrency = dUSDAsset
                  , treasuryDepositCostCenter = cc
                  }
          _ <- Simulator.callEndpointOnInstance cTreasuryUserId "depositFundsWithCostCenter" params
          Simulator.waitNSlots 10
    callDepositEndpoint "TestCostCenter1"
    callDepositEndpoint "TestCostCenter2"
    callDepositEndpoint "TestCostCenter1"
    _ <- Simulator.callEndpointOnInstance cTreasuryUserId "queryCostCenters" ()
    Simulator.waitNSlots 10
    queriedCosts <- getBus @(Vector (ByteString, Value.Value)) cTreasuryUserId
    logBlueString $ "Deposited currently: " ++ show queriedCosts
    logCurrentBalances_
    Simulator.waitNSlots 20

    -- Start buffer
    logBlueString "Start buffer contract"
    _ <- Simulator.activateContract (Wallet 1) (BufferStart treasury)
    Simulator.waitNSlots 10
    logCurrentBalances_

    cBufferUserId <- Simulator.activateContract (Wallet 2) (BufferContract treasury)
    Simulator.waitNSlots 2
    logCurrentBalances_

    let callBufferEndpoint = waitAndCallEndpoint cBufferUserId

    logBlueString "Debt auction"
    callBufferEndpoint "debtAuction" 2
    logCurrentBalances_

    logBlueString "Surplus auction"
    callBufferEndpoint "surplusAuction" 50
    logCurrentBalances_

    void $ liftIO getLine

    logBlueString "Balances at the end of the simulation:"
    logCurrentBalances_

    shutdown
  where
    wallets :: [Wallet]
    wallets = Wallet <$> [1 .. 2]

    logBlueString :: String -> Eff (PAB.PABEffects t (Simulator.SimulatorState t)) ()
    logBlueString s = logPrettyColor (Vibrant Blue) ("[INFO] " ++ s) >> logNewLine

    logCurrentBalances_ :: Eff (PAB.PABEffects t (Simulator.SimulatorState t)) ()
    logCurrentBalances_ = do
      balancesMap <- Simulator.currentBalances
      let balancesList = Map.toList balancesMap
      traverse_ (uncurry $ logWalletBalance wallets) balancesList

data ArdanaContracts
  = VaultContract
  | TreasuryContract Treasury
  | TreasuryStart
  | BufferStart Treasury
  | BufferContract Treasury
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

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
      TreasuryStart -> Builtin.endpointsToSchemas @EmptySchema
      TreasuryContract _ -> Builtin.endpointsToSchemas @TreasurySchema
      BufferStart _ -> Builtin.endpointsToSchemas @EmptySchema
      BufferContract _ -> Builtin.endpointsToSchemas @BufferSchema

    getContract :: ArdanaContracts -> SomeBuiltin
    getContract = \case
      VaultContract -> SomeBuiltin vaultContract
      TreasuryStart -> SomeBuiltin treasuryStartContract
      TreasuryContract t -> SomeBuiltin (treasuryContract @ContractError t)
      BufferStart t -> SomeBuiltin (bufferStartContract @() @ContractError t)
      BufferContract t -> SomeBuiltin (bufferAuctionContract @() @ContractError t)

handlers :: SimulatorEffectHandlers (Builtin ArdanaContracts)
handlers =
  Simulator.mkSimulatorHandlers
    @(Builtin ArdanaContracts)
    def
    [VaultContract, TreasuryStart]
    (interpret handleArdanaContract)

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
    formatFirstToken [] = ""

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
