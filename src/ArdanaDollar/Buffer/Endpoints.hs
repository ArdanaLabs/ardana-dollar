module ArdanaDollar.Buffer.Endpoints (
  type BufferSchema,
  bufferAuctionContract,
  bufferStartContract,
) where

--------------------------------------------------------------------------------

import Control.Monad (forever)
import Data.Kind (Type)

--------------------------------------------------------------------------------

import Plutus.Contract
import PlutusTx.Prelude

--------------------------------------------------------------------------------

import ArdanaDollar.Buffer.OffChain (debtAuction, startBuffer, surplusAuction)
import ArdanaDollar.Treasury.Types (Treasury)

type BufferSchema =
  Endpoint "debtAuction" Integer
    .\/ Endpoint "surplusAuction" Integer

bufferStartContract ::
  forall (w :: Type) (e :: Type).
  (AsContractError e) =>
  Treasury ->
  Contract w EmptySchema e ()
bufferStartContract = startBuffer

bufferAuctionContract ::
  forall (w :: Type) (e :: Type).
  (AsContractError e) =>
  Treasury ->
  Contract w BufferSchema e ()
bufferAuctionContract treasury =
  forever $
    selectList
      [ endpoint @"debtAuction" (debtAuction treasury)
      , endpoint @"surplusAuction" (surplusAuction treasury)
      ]
