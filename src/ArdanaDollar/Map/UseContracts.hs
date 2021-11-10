{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module ArdanaDollar.Map.UseContracts (use) where

import Prelude

import Ledger qualified
import Ledger.Constraints qualified as Constraints

import Plutus.Contract (
  Contract,
  awaitTxConfirmed,
  logInfo,
  submitTxConstraintsWith,
 )
import Plutus.V1.Ledger.Api qualified as Ledger

import Control.Lens ((^.))
import Control.Monad (void)
import Data.Kind (Type)
import Data.Map qualified as M
import Data.Row (Row)
import Data.Text (Text)

import ArdanaDollar.Map.ContractUtils (
  findKey,
  lookups',
  mkMapLookup,
 )
import ArdanaDollar.Map.MapTerms (
  MapTerms,
  MapTerms' (K', V', ValidatorTypes'),
 )
import ArdanaDollar.Map.Types (
  Datum (NodeDatum),
  MapInstance (..),
  Redeemer (Use),
 )
import ArdanaDollar.Map.Types qualified as T

use ::
  forall (t :: Type) (s :: Row Type) (w :: Type).
  MapTerms t =>
  MapInstance ->
  K' t ->
  (V' t -> V' t) ->
  Contract w s Text ()
use mapInstance key update = do
  lkp <- mkMapLookup @t mapInstance
  case findKey @t lkp key of
    Just (tpl, node) ->
      do
        let toSpend = M.fromList [tpl]
            lookups = lookups' @t mapInstance toSpend
            updated = node {T.node'value = update (T.node'value node)}
            tx =
              Constraints.mustPayToTheScript (NodeDatum updated) (snd tpl ^. Ledger.ciTxOutValue)
                <> Constraints.mustSpendScriptOutput
                  (fst tpl)
                  (Ledger.Redeemer $ Ledger.toBuiltinData Use)

        logInfo @String $ "Map: use entry"

        ledgerTx <- submitTxConstraintsWith @(ValidatorTypes' t) lookups tx
        void $ awaitTxConfirmed $ Ledger.txId ledgerTx
    _ -> return ()
