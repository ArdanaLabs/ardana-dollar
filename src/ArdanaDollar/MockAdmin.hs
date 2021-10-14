{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module ArdanaDollar.MockAdmin (
  adminAddress,
  adminValidator,
  findAdmin,
  startAdmin,
) where

--------------------------------------------------------------------------------

import Control.Lens ((^.))
import Control.Monad (void)
import Data.Kind (Type)
import Data.Map qualified as Map
import Data.Row (Row)

--------------------------------------------------------------------------------

import Ledger qualified
import Ledger.Constraints qualified as Constraints
import Ledger.Contexts qualified as Contexts
import Ledger.Typed.Scripts qualified as Scripts
import Ledger.Value qualified as Value
import Plutus.Contract
import PlutusTx qualified
import PlutusTx.Prelude

--------------------------------------------------------------------------------

import ArdanaDollar.Treasury.Types (Treasury (treasury'upgradeTokenSymbol))

data MockAdmining
instance Scripts.ValidatorTypes MockAdmining where
  type DatumType MockAdmining = ()
  type RedeemerType MockAdmining = ()

{-# INLINEABLE mkAdminValidator #-}
mkAdminValidator :: Integer -> () -> () -> Contexts.ScriptContext -> Bool
mkAdminValidator _ () () _ = True

adminInst :: Integer -> Scripts.TypedValidator MockAdmining
adminInst i =
  Scripts.mkTypedValidator @MockAdmining
    ( $$(PlutusTx.compile [||mkAdminValidator||])
        `PlutusTx.applyCode` PlutusTx.liftCode i
    )
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.wrapValidator @() @()

adminValidator :: Integer -> Ledger.Validator
adminValidator = Scripts.validatorScript . adminInst

adminAddress :: Integer -> Ledger.Address
adminAddress = Ledger.scriptAddress . adminValidator

startAdmin :: forall (w :: Type) (e :: Type). (AsContractError e) => Integer -> Contract w EmptySchema e ()
startAdmin i = do
  let tx = Constraints.mustPayToTheScript () mempty
  ledgerTx <- submitTxConstraints (adminInst i) tx
  void $ awaitTxConfirmed $ Ledger.txId ledgerTx

findAdmin ::
  forall (w :: Type) (s :: Row Type) (e :: Type).
  (AsContractError e) =>
  Treasury ->
  Ledger.Address ->
  Contract w s e (Maybe (Contexts.TxOutRef, Ledger.ChainIndexTxOut))
findAdmin treasury address = do
  utxos <- Map.filter f <$> utxosAt address
  return $ case Map.toList utxos of
    [os] -> Just os
    _ -> Nothing
  where
    tokenAC :: Value.AssetClass
    tokenAC = treasury'upgradeTokenSymbol treasury

    f :: Ledger.ChainIndexTxOut -> Bool
    f o = Value.assetClassValueOf (o ^. Ledger.ciTxOutValue) tokenAC == 1
