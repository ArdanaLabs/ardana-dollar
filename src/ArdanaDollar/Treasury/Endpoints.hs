module ArdanaDollar.Treasury.Endpoints (
  type TreasurySchema,
  treasuryContract,
  treasuryStartContract,
) where

--------------------------------------------------------------------------------

import Control.Monad (forever)
import Data.Kind (Type)
import Data.Vector (Vector)

--------------------------------------------------------------------------------

import Ledger.Value qualified as Value
import Plutus.Contract
import PlutusTx.Prelude

--------------------------------------------------------------------------------

import ArdanaDollar.Treasury.OffChain
import ArdanaDollar.Treasury.Types (
  Treasury,
  TreasuryDepositParams,
  TreasurySpendParams,
 )
import Plutus.PAB.OutputBus

type TreasurySchema =
  Endpoint "depositFundsWithCostCenter" TreasuryDepositParams
    .\/ Endpoint "spendFromCostCenter" TreasurySpendParams
    .\/ Endpoint "queryCostCenters" ()
    .\/ Endpoint "initUpgrade" ()

treasuryStartContract ::
  Contract (OutputBus Treasury) EmptySchema ContractError ()
treasuryStartContract = startTreasury >>= maybe (return ()) sendBus

treasuryContract ::
  forall (e :: Type).
  (AsContractError e) =>
  Treasury ->
  Contract (OutputBus (Vector (BuiltinByteString, Value.Value))) TreasurySchema e ()
treasuryContract treasury =
  forever $
    selectList
      [ endpoint @"depositFundsWithCostCenter" (depositFundsWithCostCenter treasury)
      , endpoint @"spendFromCostCenter" spendFromCostCenter
      , endpoint @"queryCostCenters" (const $ queryCostCenters treasury)
      , endpoint @"initUpgrade" (const initiateUpgrade)
      ]
