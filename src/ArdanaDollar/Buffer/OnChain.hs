module ArdanaDollar.Buffer.OnChain (
  mkBufferValidator,
) where

--------------------------------------------------------------------------------

import Ledger qualified
import Ledger.Contexts qualified as Contexts
import Ledger.Value qualified as Value

--------------------------------------------------------------------------------

import PlutusTx.Prelude

--------------------------------------------------------------------------------

import ArdanaDollar.Buffer.Types
import ArdanaDollar.Treasury.Types (Treasury (treasury'stateTokenAssetClass))
import ArdanaDollar.Utils (safeDivide, safeRemainder, validateDatumImmutable)

{-# INLINEABLE mkBufferValidator #-}
mkBufferValidator ::
  Treasury ->
  Ledger.Address ->
  Value.AssetClass ->
  Value.AssetClass ->
  BufferDatum ->
  BufferAction ->
  Contexts.ScriptContext ->
  Bool
mkBufferValidator treasury treasuryAddr danaAC dusdAsset bd redeemer ctx =
  traceIfFalse "treasury token missing from input" (hasTreasuryToken treasuryInput)
    && case redeemer of
      MkDebtBid amount ->
        validateDebtAuction danaAC dusdAsset bd ctx treasuryInput treasuryOutput amount
      MkSurplusBid amount ->
        validateSurplusAuction danaAC dusdAsset bd ctx treasuryInput treasuryOutput amount
  where
    info :: Contexts.TxInfo
    info = Contexts.scriptContextTxInfo ctx

    treasuryInput :: Ledger.TxOut
    treasuryInput =
      let ins =
            [ o | i <- Ledger.txInfoInputs info, let o = Ledger.txInInfoResolved i, Ledger.txOutAddress o == treasuryAddr
            ]
       in case ins of
            [o] -> o
            _ -> traceError "expected exactly one treasury input"

    treasuryOutput :: Ledger.TxOut
    treasuryOutput =
      let outs =
            [ o | o <- Ledger.txInfoOutputs info, Ledger.txOutAddress o == treasuryAddr
            ]
       in case outs of
            [o] -> o
            _ -> traceError "expected exactly one treasury output"

    hasTreasuryToken :: Ledger.TxOut -> Bool
    hasTreasuryToken o =
      Value.assetClassValueOf (Ledger.txOutValue o) (treasury'stateTokenAssetClass treasury) == 1

{-# INLINEABLE validateDebtAuction #-}
validateDebtAuction ::
  Value.AssetClass ->
  Value.AssetClass ->
  BufferDatum ->
  Contexts.ScriptContext ->
  Ledger.TxOut ->
  Ledger.TxOut ->
  Integer ->
  Bool
validateDebtAuction danaAC dusdAsset bd ctx treasuryInput treasuryOutput danaAmount =
  validateDatumImmutable bd ctx
    && traceIfFalse "non-positive amount of DANA to forge" (danaAmount > 0)
    && traceIfFalse "DANA price not paid" isDebtPaid
    && traceIfFalse "DANA not forged" isDanaDrawed
  where
    -- TODO: check if there's more DUSD in circulation than total collateral value in Ada

    isDebtPaid :: Bool
    isDebtPaid =
      let dusdPrice = currentDebtAuctionPrice bd * danaAmount
          inDusd = dusdValue dusdAsset treasuryInput
          outDusd = dusdValue dusdAsset treasuryOutput
       in traceIfFalse "non-positive amount of dUSD to pay" (dusdPrice > 0)
            && outDusd == inDusd + dusdPrice

    isDanaDrawed :: Bool
    isDanaDrawed =
      let inDana = dusdValue danaAC treasuryInput
          outDana = dusdValue danaAC treasuryOutput
       in outDana + danaAmount == inDana

{-# INLINEABLE validateSurplusAuction #-}
validateSurplusAuction ::
  Value.AssetClass ->
  Value.AssetClass ->
  BufferDatum ->
  Contexts.ScriptContext ->
  Ledger.TxOut ->
  Ledger.TxOut ->
  Integer ->
  Bool
validateSurplusAuction danaAC dusdAsset bd ctx treasuryInput treasuryOutput dusdAmount =
  validateDatumImmutable bd ctx
    && traceIfFalse "non-positive amount of dUSD to withdraw" (dusdAmount > 0)
    && traceIfFalse "non-positive amount of DANA to pay" (maybe False (> 0) danaPrice)
    && traceIfFalse "indivisible amount of DANA to pay" (Just 0 == danaRem)
    && traceIfFalse "DANA not burned" isDanaPaid
    && traceIfFalse "dUSD price not withdrawed" isSurplusWithdrawed
  where
    -- TODO: check if there's more collateral than dUSD in circulation
    currSurplus :: Integer
    currSurplus = currentSurplusAuctionPrice bd

    danaPrice :: Maybe Integer
    danaPrice = dusdAmount `safeDivide` currSurplus

    danaRem :: Maybe Integer
    danaRem = dusdAmount `safeRemainder` currSurplus

    isDanaPaid :: Bool
    isDanaPaid = case danaPrice of
      Nothing -> False
      Just dp ->
        let inDana = dusdValue danaAC treasuryInput
            outDana = dusdValue danaAC treasuryOutput
         in outDana == inDana + dp

    isSurplusWithdrawed :: Bool
    isSurplusWithdrawed =
      let inDusd = dusdValue dusdAsset treasuryInput
          outDusd = dusdValue dusdAsset treasuryOutput
       in inDusd == outDusd + dusdAmount

{-# INLINEABLE dusdValue #-}
dusdValue :: Value.AssetClass -> Ledger.TxOut -> Integer
dusdValue dusdAsset o =
  Value.assetClassValueOf (Ledger.txOutValue o) dusdAsset
