module ArdanaDollar.Treasury.Endpoints (
  type TreasurySchema,
  treasuryContract,
  treasuryStartContract,
) where

--------------------------------------------------------------------------------

import Control.Monad (forever, (>=>))
import Data.Kind (Type)
import Data.Vector (Vector)

--------------------------------------------------------------------------------

import Ledger qualified
import Ledger.Value qualified as Value
import Plutus.Contract
import PlutusTx.Prelude

--------------------------------------------------------------------------------

import ArdanaDollar.Treasury.OffChain
import ArdanaDollar.Treasury.Types (
  NewContract,
  Treasury,
  TreasuryDepositParams,
  TreasurySpendEndpointParams,
 )
import Plutus.PAB.OutputBus

type TreasurySchema =
  Endpoint "depositFundsWithCostCenter" TreasuryDepositParams
    .\/ Endpoint "spendFromCostCenter" TreasurySpendEndpointParams
    .\/ Endpoint "queryCostCenters" ()
    .\/ Endpoint "initUpgrade" NewContract

treasuryStartContract ::
  (Ledger.ValidatorHash, BuiltinByteString) ->
  Contract (OutputBus Treasury) EmptySchema ContractError ()
treasuryStartContract = uncurry startTreasury >=> maybe (return ()) sendBus

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
      , endpoint @"initUpgrade" (initiateUpgrade treasury)
      ]
