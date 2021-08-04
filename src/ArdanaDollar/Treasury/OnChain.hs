module ArdanaDollar.Treasury.OnChain (
  mkTreasuryValidator,
) where

--------------------------------------------------------------------------------

import Ledger qualified
import Ledger.Value qualified as Value
import Plutus.V1.Ledger.Contexts qualified as Contexts
import PlutusTx qualified
import PlutusTx.Prelude

--------------------------------------------------------------------------------

import ArdanaDollar.Treasury.Types

{-# INLINEABLE mkTreasuryValidator #-}
mkTreasuryValidator :: Treasury -> Value.AssetClass -> TreasuryDatum -> TreasuryAction -> Contexts.ScriptContext -> Bool
mkTreasuryValidator treasury danaAC td redeemer ctx =
  traceIfFalse "treasury token missing from input" (hasToken ownInput)
    && traceIfFalse "treasury token missing from output" (hasToken ownOutput)
    && case redeemer of
      MkDebtBid amount ->
        validateDebtAuction treasury danaAC td ctx ownInput ownOutput amount
      MkSurplusBid amount ->
        validateSurplusAuction treasury danaAC td ctx ownInput ownOutput amount
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
    hasToken o = Value.assetClassValueOf (Ledger.txOutValue o) (tSymbol treasury) == 1

{-# INLINEABLE validateDebtAuction #-}
validateDebtAuction ::
  Treasury ->
  Value.AssetClass ->
  TreasuryDatum ->
  Contexts.ScriptContext ->
  Ledger.TxOut ->
  Ledger.TxOut ->
  Integer ->
  Bool
validateDebtAuction treasury danaAC td ctx ownInput ownOutput danaAmount =
  validateDatumImmutable td ctx ownOutput
    && traceIfFalse "non-positive amount of DANA to forge" (danaAmount > 0)
    && traceIfFalse "DANA price not paid" isDebtPaid
    && traceIfFalse "DANA not forged" isDanaForged
  where
    -- TODO: check if there's more DUSD in circulation than total collateral value in Ada

    info :: Contexts.TxInfo
    info = Contexts.scriptContextTxInfo ctx

    isDebtPaid :: Bool
    isDebtPaid =
      let dusdPrice = currentDebtAuctionPrice td * danaAmount
          inDusd = dusdValue treasury ownInput
          outDusd = dusdValue treasury ownOutput
       in traceIfFalse "non-positive amount of dUSD to pay" (dusdPrice > 0)
            && outDusd >= inDusd + dusdPrice

    forged :: Value.Value
    forged = Ledger.txInfoForge info

    isDanaForged :: Bool
    isDanaForged = forged == Value.assetClassValue danaAC danaAmount

{-# INLINEABLE validateSurplusAuction #-}
validateSurplusAuction ::
  Treasury ->
  Value.AssetClass ->
  TreasuryDatum ->
  Contexts.ScriptContext ->
  Ledger.TxOut ->
  Ledger.TxOut ->
  Integer ->
  Bool
validateSurplusAuction treasury danaAC td ctx ownInput ownOutput dusdAmount =
  validateDatumImmutable td ctx ownOutput
    && traceIfFalse "non-positive amount of dUSD to withdraw" (dusdAmount > 0)
    && traceIfFalse "non-positive amount of DANA to pay" (danaPrice > 0)
    && traceIfFalse "indivisible amount of DANA to pay" (danaRem == 0)
    && traceIfFalse "DANA not burned" isDanaBurned
    && traceIfFalse "dUSD price not withdrawed" isSurplusWithdrawed
  where
    -- TODO: check if there's more collateral than dUSD in circulation
    currSurplus :: Integer
    currSurplus = currentSurplusAuctionPrice td

    danaPrice :: Integer
    danaPrice = dusdAmount `divide` currSurplus

    danaRem :: Integer
    danaRem = dusdAmount `remainder` currSurplus

    info :: Contexts.TxInfo
    info = Contexts.scriptContextTxInfo ctx

    forged :: Value.Value
    forged = Ledger.txInfoForge info

    isDanaBurned :: Bool
    isDanaBurned = forged == Value.assetClassValue danaAC (negate danaPrice)

    isSurplusWithdrawed :: Bool
    isSurplusWithdrawed =
      let inDusd = dusdValue treasury ownInput
          outDusd = dusdValue treasury ownOutput
       in inDusd == outDusd + dusdAmount

{-# INLINEABLE validateDatumImmutable #-}
validateDatumImmutable :: TreasuryDatum -> Contexts.ScriptContext -> Ledger.TxOut -> Bool
validateDatumImmutable td ctx ownOutput =
  traceIfFalse "datum has changed" (Just td == outputDatum)
  where
    info :: Contexts.TxInfo
    info = Contexts.scriptContextTxInfo ctx

    outputDatum :: Maybe TreasuryDatum
    outputDatum = treasuryDatum ownOutput (`Contexts.findDatum` info)

{-# INLINEABLE treasuryDatum #-}
treasuryDatum ::
  Contexts.TxOut ->
  (Ledger.DatumHash -> Maybe Ledger.Datum) ->
  Maybe TreasuryDatum
treasuryDatum o f = do
  dh <- Ledger.txOutDatum o
  Ledger.Datum d <- f dh
  PlutusTx.fromBuiltinData d

{-# INLINEABLE dusdValue #-}
dusdValue :: Treasury -> Ledger.TxOut -> Integer
dusdValue treasury o =
  Value.assetClassValueOf (Ledger.txOutValue o) (dusdAssetClass treasury)
