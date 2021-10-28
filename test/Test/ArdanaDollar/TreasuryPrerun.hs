module Test.ArdanaDollar.TreasuryPrerun (
  emCfg,
  startAdminTrace,
  treasuryStartContract',
) where

--------------------------------------------------------------------------------

import Control.Lens
import Control.Monad.Freer qualified as Freer
import Control.Monad.Freer.Error qualified as FrError
import Data.Default (Default (..))
import Data.Foldable (asum)
import Data.Map qualified as Map
import Data.OpenUnion.Internal (FindElem)
import Data.Text qualified as Text
import Streaming.Prelude qualified as S
import Prelude (String, show)

--------------------------------------------------------------------------------

import Ledger qualified
import Ledger.Ada as Ada
import Ledger.Value as Value
import Plutus.Contract (Contract, ContractError, EmptySchema)
import Plutus.Contract qualified as Contract
import Plutus.Contract.Test hiding (not)
import Plutus.PAB.OutputBus
import Plutus.Trace.Emulator as Emulator hiding (chainState)
import PlutusTx.Prelude hiding (asum)
import Wallet.Emulator.Folds qualified as Folds
import Wallet.Emulator.MultiAgent
import Wallet.Emulator.Stream qualified as Stream
import Wallet.Emulator.Stream.Extra (takeUntilSlot')
import Wallet.Emulator.Wallet qualified as Wallet

--------------------------------------------------------------------------------

import ArdanaDollar.MockAdmin (startAdmin)
import ArdanaDollar.Treasury.Endpoints (treasuryStartContract)
import ArdanaDollar.Treasury.Types (
  Treasury (..),
  danaAssetClass,
 )
import ArdanaDollar.Vault (dUSDAsset)

--------------------------------------------------------------------------------

lastValidatorHash :: EmulatorState -> Either String Ledger.ValidatorHash
lastValidatorHash es =
  let blocks = es ^. chainState . chainNewestFirst
   in case getValidatorFromBlocks blocks of
        Nothing -> Left . show $ blocks
        Just vh -> Right vh
  where
    getValidatorFromBlocks :: [Ledger.Block] -> Maybe Ledger.ValidatorHash
    getValidatorFromBlocks blocks =
      asum
        [ Ledger.toValidatorHash (Ledger.txOutAddress txOut)
        | block <- blocks
        , htx <- block
        , txOut <- Ledger.txOutputs (Ledger.eitherTx id id htx)
        ]

getStartAdmin :: Either String Ledger.ValidatorHash
getStartAdmin = case run of
  Left err -> Left $ show err
  Right sOf -> lastValidatorHash . snd . S.snd' $ sOf
  where
    con :: Contract () EmptySchema ContractError ()
    con = startAdmin 2 <* Contract.waitNSlots 10

    fld ::
      (FindElem (FrError.Error Folds.EmulatorFoldErr) effs) =>
      Folds.EmulatorEventFoldM effs ()
    fld = Folds.instanceAccumState con (Emulator.walletInstanceTag w2)

    run :: Either Folds.EmulatorFoldErr (S.Of () (Maybe EmulatorErr, EmulatorState))
    run =
      Freer.run $
        FrError.runError @Folds.EmulatorFoldErr $
          Stream.foldEmulatorStreamM fld $
            takeUntilSlot' 10 $
              Emulator.runEmulatorStream emCfg startAdminTrace

startAdminContract :: Integer -> Contract () EmptySchema ContractError ()
startAdminContract i = startAdmin i <* Contract.waitNSlots 10

startAdminTrace' :: Wallet.Wallet -> Integer -> EmulatorTrace ()
startAdminTrace' w i = do
  _ <- activateContractWallet w (startAdminContract i)
  _ <- waitNSlots 5
  return ()

startAdminTrace :: EmulatorTrace ()
startAdminTrace = startAdminTrace' w2 2

treasuryStartContract' :: Contract (OutputBus Treasury) EmptySchema ContractError ()
treasuryStartContract' = case getStartAdmin of
  Left err -> Contract.throwError $ Contract.OtherError (Text.pack err)
  Right vh -> treasuryStartContract (vh, "USD", Value.assetClass "abcd" "TODO", Value.assetClass "abcd" "TODO") <* Contract.waitNSlots 5

emCfg :: EmulatorConfig
emCfg = EmulatorConfig (Left $ Map.fromList [(w, v) | w <- knownWallets]) def def
  where
    v :: Value.Value
    v =
      Ada.lovelaceValueOf 1_000_000_000_000_000
        <> assetClassValue dUSDAsset 1000
        <> assetClassValue danaAssetClass 10
