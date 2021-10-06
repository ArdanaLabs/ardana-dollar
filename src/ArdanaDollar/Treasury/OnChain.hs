{-# OPTIONS_GHC -fno-specialize #-}

module ArdanaDollar.Treasury.OnChain (
  mkTreasuryValidator,
) where

--------------------------------------------------------------------------------

import Ledger qualified
import Ledger.Value qualified as Value
import Plutus.V1.Ledger.Contexts qualified as Contexts
import PlutusTx.Prelude
import PlutusTx.UniqueMap qualified as UniqueMap

--------------------------------------------------------------------------------

import ArdanaDollar.Treasury.Types
import ArdanaDollar.Utils (datumForOnchain, validateDatumImmutable)

{-# INLINEABLE mkTreasuryValidator #-}
mkTreasuryValidator ::
  Treasury ->
  TreasuryDatum ->
  TreasuryAction ->
  Contexts.ScriptContext ->
  Bool
mkTreasuryValidator treasury td redeemer ctx =
  traceIfFalse "treasury token missing from input" (hasTreasuryToken ownInput)
    && traceIfFalse "treasury token missing from output" (hasTreasuryToken ownOutput)
    && traceIfFalse "upgrade token missing from input" (hasUpgradeToken ownInput)
    && traceIfFalse "upgrade token missing from output" (hasUpgradeToken ownOutput)
    && case redeemer of
      BorrowForAuction -> validateDatumImmutable td ctx
      DepositFundsWithCostCenter params ->
        validateAuctionDanaAmountUnchanged td ctx
          && validateCurrentContractUnchanged td ctx
          && validateDepositFunds params td ownInput ownOutput ctx
      InitiateUpgrade nc ->
        validateAuctionDanaAmountUnchanged td ctx
          && validateCostCentersUnchanged td ctx
          && validateUpgrade nc treasury td ownOutput ctx
      _ -> False -- TODO: rest of the validators
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
    hasTreasuryToken = hasToken (treasury'stateTokenSymbol treasury)

    hasUpgradeToken :: Ledger.TxOut -> Bool
    hasUpgradeToken = hasToken (treasury'upgradeTokenSymbol treasury)

    hasToken :: Value.AssetClass -> Ledger.TxOut -> Bool
    hasToken ac o = Value.assetClassValueOf (Ledger.txOutValue o) ac == 1

{-# INLINEABLE validateAuctionDanaAmountUnchanged #-}
validateAuctionDanaAmountUnchanged :: TreasuryDatum -> Contexts.ScriptContext -> Bool
validateAuctionDanaAmountUnchanged =
  validateDatumFieldUnchanged "attempt to change amount of auction DANA" auctionDanaAmount

{-# INLINEABLE validateCurrentContractUnchanged #-}
validateCurrentContractUnchanged :: TreasuryDatum -> Contexts.ScriptContext -> Bool
validateCurrentContractUnchanged =
  validateDatumFieldUnchanged "attempt to change current contract" currentContract

{-# INLINEABLE validateCostCentersUnchanged #-}
validateCostCentersUnchanged :: TreasuryDatum -> Contexts.ScriptContext -> Bool
validateCostCentersUnchanged =
  validateDatumFieldUnchanged "attempt to change current contract" costCenters

{-# INLINEABLE validateDatumFieldUnchanged #-}
validateDatumFieldUnchanged ::
  (Eq a) =>
  BuiltinString ->
  (TreasuryDatum -> a) ->
  TreasuryDatum ->
  Contexts.ScriptContext ->
  Bool
validateDatumFieldUnchanged errMsg getter td ctx =
  traceIfFalse errMsg $ case outputDatum of
    Nothing -> False
    Just td' -> getter td == getter td'
  where
    info :: Contexts.TxInfo
    info = Contexts.scriptContextTxInfo ctx

    ownOutput :: Ledger.TxOut
    ownOutput = case Contexts.getContinuingOutputs ctx of
      [o] -> o
      _ -> traceError "expected exactly one treasury output"

    outputDatum :: Maybe TreasuryDatum
    outputDatum = datumForOnchain info ownOutput

{-# INLINEABLE validateDepositFunds #-}
validateDepositFunds ::
  TreasuryDepositParams ->
  TreasuryDatum ->
  Ledger.TxOut ->
  Ledger.TxOut ->
  Contexts.ScriptContext ->
  Bool
validateDepositFunds params td ownInput ownOutput ctx =
  traceIfFalse "deposit is not positive" isDepositPositive
    && traceIfFalse "deposit tries withdrawing funds" isDeposited
    && case outputDatum of
      Nothing -> traceError "cannot find output datum"
      Just td' -> isDepositListed td'
  where
    deposit :: Value.Value
    deposit = treasuryDeposit'value params

    costCenterName :: BuiltinByteString
    costCenterName = treasuryDeposit'costCenter params

    info :: Contexts.TxInfo
    info = Contexts.scriptContextTxInfo ctx

    inputValue :: Value.Value
    inputValue = Ledger.txOutValue ownInput

    outputValue :: Value.Value
    outputValue = Ledger.txOutValue ownOutput

    outputDatum :: Maybe TreasuryDatum
    outputDatum = datumForOnchain info ownOutput

    otherCostCenters ::
      UniqueMap.Map BuiltinByteString Value.Value ->
      UniqueMap.Map BuiltinByteString Value.Value
    otherCostCenters = UniqueMap.delete costCenterName

    costCenterValue :: UniqueMap.Map BuiltinByteString Value.Value -> Value.Value
    costCenterValue = fromMaybe mempty . UniqueMap.lookup costCenterName

    isDepositPositive :: Bool
    isDepositPositive = all (\(_, _, i) -> i > 0) (Value.flattenValue deposit)

    isDeposited :: Bool
    isDeposited = inputValue <> deposit == outputValue

    {- Check if deposit is correctly listed in the cost centers map and
    that the operation does not change other cost centers in any way -}
    isDepositListed :: TreasuryDatum -> Bool
    isDepositListed newDatum =
      let oldCostCenters = costCenters td
          newCostCenters = costCenters newDatum

          oldDatumAsset = costCenterValue oldCostCenters
          newDatumAsset = costCenterValue newCostCenters

          otherCostCentersImmutable =
            otherCostCenters oldCostCenters == otherCostCenters newCostCenters
          assetValueSameCostCenterChanged = oldDatumAsset <> deposit == newDatumAsset
       in traceIfFalse "deposit modifies other cost centers" otherCostCentersImmutable
            && traceIfFalse "deposit is not listed" assetValueSameCostCenterChanged

{-# INLINEABLE validateUpgrade #-}
validateUpgrade ::
  NewContract ->
  Treasury ->
  TreasuryDatum ->
  Ledger.TxOut ->
  Contexts.ScriptContext ->
  Bool
validateUpgrade (NewContract nc) treasury td ownOutput ctx = case outputDatum of
  Nothing -> False
  Just TreasuryDatum {currentContract = cc'} ->
    let cc = currentContract td
     in traceIfFalse "new contract not saved" (nc == cc')
          && traceIfFalse "missing upgrade token in old contract input" (any hasUpgradeToken (validatorInputs cc))
          && traceIfFalse "unexpected upgrade token in new contract input" (not $ any hasUpgradeToken (validatorInputs cc'))
          && traceIfFalse "unexpected upgrade token in old contract output" (not $ any hasUpgradeToken (validatorOutputs cc))
          && traceIfFalse "missing upgrade token in new contract output" (any hasUpgradeToken (validatorOutputs cc'))
  where
    info :: Contexts.TxInfo
    info = Contexts.scriptContextTxInfo ctx

    outputDatum :: Maybe TreasuryDatum
    outputDatum = datumForOnchain info ownOutput

    validatorInputs :: Ledger.ValidatorHash -> [Ledger.TxOut]
    validatorInputs vh =
      [ o | i <- Ledger.txInfoInputs info, let o = Ledger.txInInfoResolved i, Ledger.txOutAddress o == Ledger.scriptHashAddress vh
      ]

    validatorOutputs :: Ledger.ValidatorHash -> [Ledger.TxOut]
    validatorOutputs vh =
      [ o | o <- Ledger.txInfoOutputs info, Ledger.txOutAddress o == Ledger.scriptHashAddress vh
      ]

    hasUpgradeToken :: Ledger.TxOut -> Bool
    hasUpgradeToken o =
      Value.assetClassValueOf (Ledger.txOutValue o) (treasury'upgradeTokenSymbol treasury) == 1
