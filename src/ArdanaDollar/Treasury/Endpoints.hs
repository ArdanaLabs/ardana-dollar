module ArdanaDollar.Treasury.Endpoints (
  type TreasurySchema,
  treasuryContract,
  treasuryStartContract,
) where

--------------------------------------------------------------------------------

import Control.Monad (forever, (>=>))

-- import Data.Aeson (FromJSON)
import Data.Kind (Type)
import Data.Vector (Vector)

--------------------------------------------------------------------------------

import Ledger qualified
import Ledger.Value qualified as Value
import Plutus.Contract

-- import PlutusTx (ToData)
import PlutusTx.Prelude

--------------------------------------------------------------------------------

import ArdanaDollar.Treasury.OffChain
import ArdanaDollar.Treasury.Types (
  NewContract,
  Treasury,
  -- TreasuryDepositParams,
  -- TreasurySpendEndpointParams,
 )
import Plutus.PAB.OutputBus

type TreasurySchema =
  -- Endpoint "depositFundsWithCostCenter" TreasuryDepositParams
  --   .\/ Endpoint "spendFromCostCenter" (TreasurySpendEndpointParams d)
  --   .\/ Endpoint "queryCostCenters" ()
  Endpoint "initUpgrade" NewContract

treasuryStartContract ::
  (Ledger.ValidatorHash, BuiltinByteString, Value.AssetClass, Value.AssetClass) ->
  Contract (OutputBus Treasury) EmptySchema ContractError ()
treasuryStartContract = uncurry4 startTreasury >=> maybe (return ()) sendBus
  where
    uncurry4 :: (a -> b -> c -> d -> e) -> (a, b, c, d) -> e
    uncurry4 f ~(a, b, c, d) = f a b c d

treasuryContract ::
  forall (e :: Type).
  (AsContractError e) =>
  Treasury ->
  Contract (OutputBus (Vector (BuiltinByteString, Value.Value))) TreasurySchema e ()
treasuryContract treasury =
  forever $
    selectList
      -- [ endpoint @"depositFundsWithCostCenter" (depositFundsWithCostCenter treasury)
      -- , endpoint @"spendFromCostCenter" (spendFromCostCenter treasury)
      -- , endpoint @"queryCostCenters" (const $ queryCostCenters treasury)
      [ endpoint @"initUpgrade" (initiateUpgrade treasury)
      ]
