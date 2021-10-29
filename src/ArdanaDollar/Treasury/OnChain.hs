{-# OPTIONS_GHC -fno-specialize #-}

module ArdanaDollar.Treasury.OnChain (
  mkTreasuryValidator,
) where

--------------------------------------------------------------------------------

import Ledger qualified
import Ledger.Value qualified as Value
import Plutus.V1.Ledger.Contexts qualified as Contexts

-- import Plutus.V1.Ledger.Value.Extra (valueSubsetOf)
import PlutusTx.Prelude

--------------------------------------------------------------------------------

import ArdanaDollar.Treasury.Types

-- import ArdanaDollar.Utils (allNonEmpty, datumForOnchain, validateDatumImmutable)

--------------------------------------------------------------------------------

{-# INLINEABLE mkTreasuryValidator #-}
mkTreasuryValidator ::
  Treasury ->
  Value.AssetClass ->
  TreasuryState ->
  TreasuryAction ->
  Contexts.ScriptContext ->
  Bool
mkTreasuryValidator treasury _canSpendAC _ts _redeemer ctx =
  traceIfFalse "treasury token missing from input" (hasTreasuryToken ownInput)
    && traceIfFalse "treasury token missing from output" (hasTreasuryToken ownOutput)
    && False -- TODO: rest of the validators
  where
    ownInput :: Ledger.TxOut
    ownInput = case Contexts.findOwnInput ctx of
      Nothing -> traceError "treasury input missing"
      Just i -> Contexts.txInInfoResolved i

    ownOutput :: Ledger.TxOut
    ownOutput = case Contexts.getContinuingOutputs ctx of
      [o] -> o
      _ -> traceError "expected exactly one treasury output"

    hasTreasuryToken :: Ledger.TxOut -> Bool
    hasTreasuryToken = hasToken (treasury'stateTokenAssetClass treasury)

    hasToken :: Value.AssetClass -> Ledger.TxOut -> Bool
    hasToken ac o = Value.assetClassValueOf (Ledger.txOutValue o) ac == 1

------------------------------ HELPER VALIDATORS -------------------------------
-- {-# INLINEABLE validateAuctionDanaAmountUnchanged #-}
-- validateAuctionDanaAmountUnchanged :: TreasuryState -> Contexts.ScriptContext -> Bool
-- validateAuctionDanaAmountUnchanged =
--   validateDatumFieldUnchanged "attempt to change amount of auction DANA" auctionDanaAmount

-- {-# INLINEABLE validateCostCentersUnchanged #-}
-- validateCostCentersUnchanged :: TreasuryState -> Contexts.ScriptContext -> Bool
-- validateCostCentersUnchanged =
--   validateDatumFieldUnchanged "attempt to change current contract" costCenters

-- {-# INLINEABLE validateDatumFieldUnchanged #-}
-- validateDatumFieldUnchanged ::
--   (Eq a) =>
--   BuiltinString ->
--   (TreasuryState -> a) ->
--   TreasuryState ->
--   Contexts.ScriptContext ->
--   Bool
-- validateDatumFieldUnchanged errMsg getter ts ctx =
--   traceIfFalse errMsg $ case outputDatum of
--     Nothing -> False
--     Just ts' -> getter ts == getter ts'
--   where
--     info :: Contexts.TxInfo
--     info = Contexts.scriptContextTxInfo ctx

--     ownOutput :: Ledger.TxOut
--     ownOutput = case Contexts.getContinuingOutputs ctx of
--       [o] -> o
--       _ -> traceError "expected exactly one treasury output"

--     outputDatum :: Maybe TreasuryState
--     outputDatum = datumForOnchain info ownOutput

-- {-# INLINEABLE isCostCenterChangeListed #-}

-- {- | Check if deposit is correctly listed in the cost centers map and
--     that the operation does not change other cost centers in any way
-- -}
-- isCostCenterChangeListed ::
--   TreasuryState ->
--   TreasuryState ->
--   BuiltinByteString ->
--   (Value.Value -> Value.Value -> Bool) ->
--   Bool
-- isCostCenterChangeListed oldDatum newDatum costCenterName sameCostCenterCheck =
--   let oldCostCenters = costCenters oldDatum
--       newCostCenters = costCenters newDatum

--       oldDatumAsset = costCenterValue oldCostCenters
--       newDatumAsset = costCenterValue newCostCenters

--       otherCostCentersImmutable =
--         otherCostCenters oldCostCenters == otherCostCenters newCostCenters
--       assetValueSameCostCenterChanged = sameCostCenterCheck oldDatumAsset newDatumAsset
--    in traceIfFalse "modifies other cost centers" otherCostCentersImmutable
--         && traceIfFalse "is not listed" assetValueSameCostCenterChanged
--   where
--     otherCostCenters ::
--       UniqueMap.Map BuiltinByteString Value.Value ->
--       UniqueMap.Map BuiltinByteString Value.Value
--     otherCostCenters = UniqueMap.delete costCenterName

--     costCenterValue :: UniqueMap.Map BuiltinByteString Value.Value -> Value.Value
--     costCenterValue = fromMaybe mempty . UniqueMap.lookup costCenterName

-- ---------------------------------- VALIDATORS ----------------------------------
-- {-# INLINEABLE validateDepositFunds #-}
-- validateDepositFunds ::
--   TreasuryState ->
--   Ledger.TxOut ->
--   Ledger.TxOut ->
--   Contexts.ScriptContext ->
--   TreasuryDepositParams ->
--   Bool
-- validateDepositFunds
--   ts
--   ownInput
--   ownOutput
--   ctx
--   TreasuryDepositParams
--     { treasuryDeposit'value = deposit
--     , treasuryDeposit'costCenter = costCenterName
--     } =
--     traceIfFalse "deposit is not positive" isDepositPositive
--       && traceIfFalse "deposit tries withdrawing funds" isDeposited
--       && case outputDatum of
--         Nothing -> traceError "cannot find output datum"
--         Just ts' ->
--           isCostCenterChangeListed
--             ts
--             ts'
--             costCenterName
--             (\old new -> old <> deposit == new)
--     where
--       info :: Contexts.TxInfo
--       info = Contexts.scriptContextTxInfo ctx

--       inputValue :: Value.Value
--       inputValue = Ledger.txOutValue ownInput

--       outputValue :: Value.Value
--       outputValue = Ledger.txOutValue ownOutput

--       outputDatum :: Maybe TreasuryState
--       outputDatum = datumForOnchain info ownOutput

--       isDepositPositive :: Bool
--       isDepositPositive = allNonEmpty (\(_, _, i) -> i > 0) (Value.flattenValue deposit)

--       isDeposited :: Bool
--       isDeposited = inputValue <> deposit == outputValue

-- {-# INLINEABLE validateSpendFunds #-}
-- validateSpendFunds ::
--   TreasuryState ->
--   Ledger.TxOut ->
--   Ledger.TxOut ->
--   Value.AssetClass ->
--   Contexts.ScriptContext ->
--   TreasurySpendParams ->
--   Bool
-- validateSpendFunds
--   ts
--   ownInput
--   ownOutput
--   canSpendAC
--   ctx
--   TreasurySpendParams
--     { treasurySpend'value = spending
--     , treasurySpend'costCenter = costCenterName
--     , treasurySpend'beneficiary = beneficiary
--     } =
--     traceIfFalse "spending is not positive" isSpendingPositive
--       && traceIfFalse "spending tries depositing funds" isSpent
--       && traceIfFalse "spending is not paying to the beneficiary" isPayingBeneficiary
--       && traceIfFalse "missing CanSpend token from input" (not . null $ canSpendInputs)
--       && case outputDatum of
--         Nothing -> traceError "cannot find output datum"
--         Just ts' ->
--           isCostCenterChangeListed
--             ts
--             ts'
--             costCenterName
--             (\old new -> old == new <> spending)
--     where
--       info :: Contexts.TxInfo
--       info = Contexts.scriptContextTxInfo ctx

--       inputValue :: Value.Value
--       inputValue = Ledger.txOutValue ownInput

--       outputValue :: Value.Value
--       outputValue = Ledger.txOutValue ownOutput

--       outputDatum :: Maybe TreasuryState
--       outputDatum = datumForOnchain info ownOutput

--       canSpendValue :: Value.Value
--       canSpendValue = Value.assetClassValue canSpendAC 1

--       beneficiaryOutputs :: [Ledger.TxOut]
--       beneficiaryOutputs =
--         [ o | o <- Ledger.txInfoOutputs info, Ledger.txOutAddress o == beneficiary
--         ]

--       canSpendInputs :: [Ledger.TxOut]
--       canSpendInputs =
--         [ o | i <- Ledger.txInfoInputs info, let o = Ledger.txInInfoResolved i, canSpendValue `valueSubsetOf` Ledger.txOutValue o
--         ]

--       isSpendingPositive :: Bool
--       isSpendingPositive = allNonEmpty (\(_, _, i) -> i > 0) (Value.flattenValue spending)

--       isSpent :: Bool
--       isSpent = inputValue == outputValue <> spending

--       isPayingBeneficiary :: Bool
--       isPayingBeneficiary = flip any beneficiaryOutputs $
--         \Contexts.TxOut {txOutValue = v} -> spending `valueSubsetOf` v

-- {-# INLINEABLE validateUpgrade #-}
-- validateUpgrade ::
--   NewContract ->
--   Treasury ->
--   TreasuryState ->
--   Ledger.TxOut ->
--   Contexts.ScriptContext ->
--   Bool
-- validateUpgrade (NewContract nc) treasury ts ownOutput ctx = case outputDatum of
--   Nothing -> False
--   Just TreasuryState {currentContract = cc'} ->
--     let cc = currentContract ts
--      in traceIfFalse "new contract not saved" (nc == cc')
--           && traceIfFalse "missing upgrade token in old contract input" (any hasUpgradeToken (validatorInputs cc))
--           && traceIfFalse "unexpected upgrade token in new contract input" (not $ any hasUpgradeToken (validatorInputs cc'))
--           && traceIfFalse "unexpected upgrade token in old contract output" (not $ any hasUpgradeToken (validatorOutputs cc))
--           && traceIfFalse "missing upgrade token in new contract output" (any hasUpgradeToken (validatorOutputs cc'))
--   where
--     info :: Contexts.TxInfo
--     info = Contexts.scriptContextTxInfo ctx

--     outputDatum :: Maybe TreasuryState
--     outputDatum = datumForOnchain info ownOutput

--     validatorInputs :: Ledger.ValidatorHash -> [Ledger.TxOut]
--     validatorInputs vh =
--       [ o | i <- Ledger.txInfoInputs info, let o = Ledger.txInInfoResolved i, Ledger.txOutAddress o == Ledger.scriptHashAddress vh
--       ]

--     validatorOutputs :: Ledger.ValidatorHash -> [Ledger.TxOut]
--     validatorOutputs vh =
--       [ o | o <- Ledger.txInfoOutputs info, Ledger.txOutAddress o == Ledger.scriptHashAddress vh
--       ]

--     hasUpgradeToken :: Ledger.TxOut -> Bool
--     hasUpgradeToken o =
--       Value.assetClassValueOf (Ledger.txOutValue o) (treasury'upgradeTokenSymbol treasury) == 1
