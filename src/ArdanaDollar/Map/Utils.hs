{-# LANGUAGE TypeApplications #-}

module ArdanaDollar.Map.Utils (
  dataAndValueAtAddress,
) where

import Control.Arrow ((>>>))
import Control.Foldl qualified as L
import Data.Map qualified as M
import Ledger
import Ledger.Value as V
import Plutus.Contract.Test (TracePredicate)
import PlutusTx

import Data.Maybe (mapMaybe)
import Wallet.Emulator.Folds (postMapM)
import Wallet.Emulator.Folds qualified as Folds
import Prelude

-- | Get a datum of a given type 'd' out of a Transaction Output.
getTxOutDatum ::
  forall d.
  (FromData d) =>
  Ledger.TxOutRef ->
  Ledger.TxOutTx ->
  Maybe d
getTxOutDatum _ (Ledger.TxOutTx _ (Ledger.TxOut _ _ Nothing)) = Nothing
getTxOutDatum _ (Ledger.TxOutTx tx' (Ledger.TxOut _ _ (Just dh))) =
  Ledger.lookupDatum tx' dh >>= (Ledger.getDatum >>> fromBuiltinData @d)

dataAndValueAtAddress :: forall d. FromData d => Address -> ([(d, V.Value)] -> Bool) -> TracePredicate
dataAndValueAtAddress address check =
  flip postMapM (L.generalize $ Folds.utxoAtAddress address) $ \utxo -> do
    let datums = mapMaybe (\(ref, tx) -> (,Ledger.txOutValue $ Ledger.txOutTxOut tx) <$> getTxOutDatum @d ref tx) $ M.toList utxo
        result = check datums
    pure result