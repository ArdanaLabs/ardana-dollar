module ArdanaDollar.Treasury.Endpoints (
  type TreasurySchema,
  treasuryContract,
  treasuryStartContract,
) where

--------------------------------------------------------------------------------

import Control.Monad (forever, (>=>))
import Data.Kind (Type)
import Prelude (foldr1)

--------------------------------------------------------------------------------

import Ledger.Value qualified as Value
import Plutus.Contract
import PlutusTx.Prelude

--------------------------------------------------------------------------------

import ArdanaDollar.Treasury.OffChain
import ArdanaDollar.Treasury.Types (Treasury)
import Plutus.PAB.OutputBus

type TreasurySchema =
  Endpoint "debtAuction" Integer
    .\/ Endpoint "surplusAuction" Integer
    .\/ Endpoint "depositAuctionFloat" ()
    .\/ Endpoint "upgrade" ()

treasuryStartContract :: Value.AssetClass -> Contract (OutputBus Treasury) EmptySchema ContractError ()
treasuryStartContract = startTreasury >=> sendBus

treasuryContract :: forall (w :: Type) (e :: Type). (AsContractError e) => Treasury -> Contract w TreasurySchema e ()
treasuryContract treasury =
  forever $
    foldr1
      select
      [ endpoint @"debtAuction" >>= debtAuction treasury
      , endpoint @"surplusAuction" >>= surplusAuction treasury
      , endpoint @"depositAuctionFloat" >> depositAuctionFloat
      , endpoint @"upgrade" >> upgrade
      ]
