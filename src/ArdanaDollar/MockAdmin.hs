{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module ArdanaDollar.MockAdmin (
  adminAddress,
  adminValidator,
  startAdmin,
) where

--------------------------------------------------------------------------------

import Control.Monad (void)
import Data.Kind (Type)

--------------------------------------------------------------------------------

import Ledger qualified
import Ledger.Constraints qualified as Constraints
import Ledger.Contexts qualified as Contexts
import Ledger.Typed.Scripts qualified as Scripts
import Plutus.Contract
import PlutusTx qualified
import PlutusTx.Prelude

--------------------------------------------------------------------------------

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
