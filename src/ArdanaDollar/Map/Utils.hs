{-# LANGUAGE TypeApplications #-}

module ArdanaDollar.Map.Utils (
  dataAndValueAtAddress,
) where

import Control.Arrow ((>>>))
import Control.Foldl qualified as L
import Data.Map qualified as M
import Ledger qualified
import Ledger.Value qualified as V
import Plutus.Contract.Test (TracePredicate)
import PlutusTx qualified

import Data.Maybe qualified as Maybe
import Wallet.Emulator.Folds qualified as Folds
import Prelude

-- | Get a datum of a given type 'd' out of a Transaction Output.
getTxOutDatum ::
  forall d.
  (PlutusTx.FromData d) =>
  Ledger.TxOutRef ->
  Ledger.TxOutTx ->
  Maybe d
getTxOutDatum _ (Ledger.TxOutTx _ (Ledger.TxOut _ _ Nothing)) = Nothing
getTxOutDatum _ (Ledger.TxOutTx tx' (Ledger.TxOut _ _ (Just dh))) =
  Ledger.lookupDatum tx' dh >>= (Ledger.getDatum >>> PlutusTx.fromBuiltinData @d)

dataAndValueAtAddress :: forall d. PlutusTx.FromData d => Ledger.Address -> ([(d, V.Value)] -> Bool) -> TracePredicate
dataAndValueAtAddress address check =
  flip Folds.postMapM (L.generalize $ Folds.utxoAtAddress address) $ \utxo -> do
    let datums = Maybe.mapMaybe (\(ref, tx) -> (,Ledger.txOutValue $ Ledger.txOutTxOut tx) <$> getTxOutDatum @d ref tx) $ M.toList utxo
        result = check datums
    pure result