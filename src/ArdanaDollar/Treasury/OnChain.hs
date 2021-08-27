{-# OPTIONS_GHC -fno-specialize #-}

module ArdanaDollar.Treasury.OnChain (
  mkTreasuryValidator,
) where

--------------------------------------------------------------------------------

import Ledger qualified
import Ledger.Value qualified as Value
import Plutus.V1.Ledger.Contexts qualified as Contexts
import PlutusTx.AssocMap qualified as Assoc
import PlutusTx.AssocMap.Extra qualified as Assoc
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
  traceIfFalse "treasury token missing from input" (hasToken ownInput)
    && traceIfFalse "treasury token missing from output" (hasToken ownOutput)
    && case redeemer of
      BorrowForAuction -> validateDatumImmutable td ctx
      DepositFundsWithCostCenter params ->
        validateAuctionDanaAmountUnchanged td ctx
          && validateDepositFunds params td ownInput ownOutput ctx
      _ -> True -- TODO: rest of the validators
  where
    ownInput :: Ledger.TxOut
    ownInput = case Contexts.findOwnInput ctx of
      Nothing -> traceError "treasury input missing"
      Just i -> Contexts.txInInfoResolved i

    ownOutput :: Ledger.TxOut
    ownOutput = case Contexts.getContinuingOutputs ctx of
      [o] -> o
      _ -> traceError "expected exactly one treasury output"

    hasToken :: Ledger.TxOut -> Bool
    hasToken o = Value.assetClassValueOf (Ledger.txOutValue o) (stateTokenSymbol treasury) == 1

{-# INLINEABLE validateAuctionDanaAmountUnchanged #-}
validateAuctionDanaAmountUnchanged :: TreasuryDatum -> Contexts.ScriptContext -> Bool
validateAuctionDanaAmountUnchanged =
  validateDatumFieldUnchanged "attempt to change amount of auction DANA" auctionDanaAmount

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
  traceIfFalse "deposit is not positive" (depositAmount > 0)
    && traceIfFalse "deposit tries withdrawing funds" isDeposited
    && case outputDatum of
      Nothing -> traceError "cannot find output datum"
      Just td' -> isDepositListed td'
  where
    ac :: Value.AssetClass
    ac = treasuryDepositCurrency params

    costCenterName :: ByteString
    costCenterName = treasuryDepositCostCenter params

    depositAmount :: Integer
    depositAmount = treasuryDepositAmount params

    info :: Contexts.TxInfo
    info = Contexts.scriptContextTxInfo ctx

    inputValue :: Value.Value
    inputValue = Ledger.txOutValue ownInput

    outputValue :: Value.Value
    outputValue = Ledger.txOutValue ownOutput

    outputDatum :: Maybe TreasuryDatum
    outputDatum = datumForOnchain info ownOutput

    otherCostCenters ::
      UniqueMap.Map ByteString Value.Value ->
      UniqueMap.Map ByteString Value.Value
    otherCostCenters = UniqueMap.delete costCenterName

    costCenterValue :: UniqueMap.Map ByteString Value.Value -> Value.Value
    costCenterValue = fromMaybe mempty . UniqueMap.lookup costCenterName

    {- Check if deposit is correctly added to the output and
    that the operation does not change other assets in any way -}
    isDeposited :: Bool
    isDeposited =
      let inAsset = Value.assetClassValueOf inputValue ac
          outAsset = Value.assetClassValueOf outputValue ac
       in otherValues ac inputValue == otherValues ac outputValue
            && inAsset + depositAmount == outAsset
            && (outAsset - inAsset) > 0

    {- Check if deposit is correctly listed in the cost centers map and
    that the operation does not change other cost centers in any way -}
    isDepositListed :: TreasuryDatum -> Bool
    isDepositListed newDatum =
      let oldCostCenters = costCenters td
          newCostCenters = costCenters newDatum

          oldDatumAsset = Value.assetClassValueOf (costCenterValue oldCostCenters) ac
          newDatumAsset = Value.assetClassValueOf (costCenterValue newCostCenters) ac

          otherCostCentersImmutable =
            otherCostCenters oldCostCenters == otherCostCenters newCostCenters
          otherValuesSameCostCenterImmutable =
            otherValues ac (costCenterValue oldCostCenters)
              == otherValues ac (costCenterValue newCostCenters)
          assetValueSameCostCenterChanged = oldDatumAsset + depositAmount == newDatumAsset

          depositListed = otherValuesSameCostCenterImmutable && assetValueSameCostCenterChanged
       in traceIfFalse "deposit modifies other cost centers" otherCostCentersImmutable
            && traceIfFalse "deposit is not listed" depositListed

-- | Return `Value.Value` without asset of given asset class
{-# INLINEABLE otherValues #-}
otherValues :: Value.AssetClass -> Value.Value -> Value.Value
otherValues (Value.AssetClass (cs, tn)) (Value.Value valueMap) =
  Value.Value $ Assoc.alter cs (fmap (Assoc.delete tn)) valueMap
