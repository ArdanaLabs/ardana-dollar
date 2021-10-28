{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-specialise #-}

module ArdanaDollar.ExponentialMint.OnChain (
  mkExponentialMintingPolicy,
  exponentialMintingPolicy,
  exponentialCurrencySymbol,
) where

import ArdanaDollar.ExponentialMint.Types
import ArdanaDollar.Utils (getAllScriptInputsWithDatum, getContinuingScriptOutputsWithDatum)
import Ledger (
  TxOutRef,
  member,
  ownCurrencySymbol,
  txInInfoOutRef,
  txInfoInputs,
  txInfoOutputs,
  txOutValue,
 )
import Ledger qualified
import Ledger.Typed.Scripts qualified as Scripts
import Ledger.Value qualified as Value
import Plutus.V1.Ledger.Ada qualified as Ada
import Plutus.V1.Ledger.Interval.Extra (width)
import Plutus.V1.Ledger.Value (
  assetClass,
  assetClassValueOf,
 )
import PlutusTx qualified
import PlutusTx.Prelude

{-# INLINEABLE mkExponentialMintingPolicy #-}
mkExponentialMintingPolicy ::
  TxOutRef ->
  ExponentialMintingRedeemer ->
  Ledger.ScriptContext ->
  Bool
mkExponentialMintingPolicy
  oneShotTx
  Initialise
  sc@Ledger.ScriptContext {scriptContextTxInfo = txInfo} =
    correctMinting && hasControlTokenMintingUTXO
    where
      hasControlTokenMintingUTXO :: Bool
      hasControlTokenMintingUTXO = any (\i -> txInInfoOutRef i == oneShotTx) $ txInfoInputs txInfo
      minted :: Ledger.Value
      minted = Ledger.txInfoMint txInfo
      controlToken :: Ledger.Value
      controlToken = controlTokenValue (Ledger.ownCurrencySymbol sc)
      correctMinting =
        traceIfFalse
          "incorrect minted amount"
          (minted == controlToken)
mkExponentialMintingPolicy
  _
  (SeedExponentiallyMintableToken adaReturnAddress ExponentialMint {..})
  sc@Ledger.ScriptContext {scriptContextTxInfo = txInfo} =
    correctMinting
      && outputCarriesControlToken
      && continuingExponentialMintsAreValid
      && atLeastOneExponentialMintProduced
    where
      minted :: Ledger.Value
      minted = Ledger.txInfoMint txInfo
      exponentialTokensMinted :: Ledger.Value
      exponentialTokensMinted = Value.singleton (Ledger.ownCurrencySymbol sc) seedTokenName numExponentialMints
      captiveTokensMinted :: Ledger.Value
      captiveTokensMinted = captiveTokenValue (Ledger.ownCurrencySymbol sc) numExponentialMints
      proliferationParameters :: ProliferationParameters
      proliferationParameters =
        ProliferationParameters
          adaReturnAddress
          minReplications
          canBeDestroyedAtTime
          narrowIntervalWidth
      correctMinting :: Bool
      correctMinting =
        traceIfFalse
          "incorrect minted amount"
          (minted == exponentialTokensMinted <> captiveTokensMinted)
      atLeastOneExponentialMintProduced :: Bool
      atLeastOneExponentialMintProduced = traceIfFalse "at least one exponential must be produced" (numExponentialMints > 0)
      continuingExponentialMintsAreValid :: Bool
      numExponentialMints :: Integer
      (continuingExponentialMintsAreValid, numExponentialMints) = case getContinuingScriptOutputsWithDatum @ProliferationParameters sc of
        outputs -> (all (validateContinuingProliferationParameters sc proliferationParameters seedTokenName) outputs, length outputs)
      outputCarriesControlToken :: Bool
      outputCarriesControlToken =
        let outputValue = mconcat (txOutValue <$> txInfoOutputs txInfo)
         in traceIfFalse
              "ControlToken not present"
              (valueCarriesControlToken (ownCurrencySymbol sc) outputValue)
mkExponentialMintingPolicy
  _
  (CopyExponentiallyMintableToken adaReturnAddress exponentialTokenName)
  sc@Ledger.ScriptContext {scriptContextTxInfo = txInfo} =
    correctMinting
      && repaysCopyCreator
      && createsAtLeastNCopies
      && continuingExponentialMintsAreValid
    where
      minted :: Ledger.Value
      minted = Ledger.txInfoMint txInfo
      exponentialTokensMinted :: Ledger.Value
      exponentialTokensMinted = Value.singleton (Ledger.ownCurrencySymbol sc) exponentialTokenName numExponentialMints
      captiveTokensMinted :: Ledger.Value
      captiveTokensMinted = captiveTokenValue (Ledger.ownCurrencySymbol sc) (numExponentialMints - 1)
      proliferatedParameters :: ProliferationParameters
      proliferatedParameters = p {adaReturnAddress' = adaReturnAddress}
      correctMinting :: Bool
      correctMinting =
        traceIfFalse
          "incorrect minted amount"
          (minted == exponentialTokensMinted <> captiveTokensMinted)
      createsAtLeastNCopies :: Bool
      createsAtLeastNCopies = traceIfFalse "does not create required replications" (numExponentialMints >= minReplications')
      repaysCopyCreator :: Bool
      repaysCopyCreator = paysAdaInValueToAddr txInfo exponentialValue adaReturnAddress'
      exponentialValue :: Ledger.Value
      p :: ProliferationParameters
      (exponentialValue, p@ProliferationParameters {..}) =
        case getAllScriptInputsWithDatum @ProliferationParameters sc of
          [(output, _, pp)] ->
            (Ledger.txOutValue output, pp)
          _ -> traceError "can only copy one at a time"
      continuingExponentialMintsAreValid :: Bool
      numExponentialMints :: Integer
      (continuingExponentialMintsAreValid, numExponentialMints) =
        let outputs = getContinuingScriptOutputsWithDatum @ProliferationParameters sc
         in (all (validateContinuingProliferationParameters sc proliferatedParameters exponentialTokenName) outputs, length outputs)
mkExponentialMintingPolicy
  _
  (DestroyExponentiallyMintableToken exponentialTokenName)
  sc@Ledger.ScriptContext {scriptContextTxInfo = txInfo} =
    narrowInterval && correctMinting && expiryInRange && repaysCopyCreator
    where
      range :: Ledger.POSIXTimeRange
      range = Ledger.txInfoValidRange txInfo
      expiryInRange :: Bool
      expiryInRange = canBeDestroyedAtTime' `Ledger.member` range
      narrowInterval :: Bool
      narrowInterval = traceIfFalse "timestamp outwith interval" $ withinInterval narrowIntervalWidth' txInfo
      minted :: Ledger.Value
      minted = Ledger.txInfoMint txInfo
      exponentialTokensMinted :: Ledger.Value
      exponentialTokensMinted = Value.singleton (Ledger.ownCurrencySymbol sc) exponentialTokenName (-1)
      captiveTokensMinted :: Ledger.Value
      captiveTokensMinted = captiveTokenValue (Ledger.ownCurrencySymbol sc) (-1)
      correctMinting =
        traceIfFalse
          "incorrect minted amount"
          (minted == exponentialTokensMinted <> captiveTokensMinted)
      repaysCopyCreator :: Bool
      repaysCopyCreator = paysAdaInValueToAddr txInfo exponentialValue adaReturnAddress'
      exponentialValue :: Ledger.Value
      (exponentialValue, ProliferationParameters {..}) =
        case getAllScriptInputsWithDatum @ProliferationParameters sc of
          [(output, _, pp)] ->
            (Ledger.txOutValue output, pp)
          _ -> traceError "can only destroy one at a time"

{-# INLINEABLE withinInterval #-}
withinInterval :: Integer -> Ledger.TxInfo -> Bool
withinInterval interval txInfo = case width (Ledger.txInfoValidRange txInfo) of
  Nothing -> False
  Just ms -> ms <= interval

{-# INLINEABLE controlTokenValue #-}
controlTokenValue :: Value.CurrencySymbol -> Ledger.Value
controlTokenValue cs = Value.singleton cs (Value.TokenName "ControlToken") 1

{-# INLINEABLE valueCarriesControlToken #-}
valueCarriesControlToken :: Value.CurrencySymbol -> Ledger.Value -> Bool
valueCarriesControlToken cs v = assetClassValueOf v (assetClass cs (Value.TokenName "ControlToken")) == 1

{-# INLINEABLE captiveTokenValue #-}
captiveTokenValue :: Value.CurrencySymbol -> Integer -> Ledger.Value
captiveTokenValue cs = Value.singleton cs (Value.TokenName "CaptiveToken")

{-# INLINEABLE valueCarriesCaptiveToken #-}
valueCarriesCaptiveToken :: Value.CurrencySymbol -> Ledger.Value -> Bool
valueCarriesCaptiveToken cs v = assetClassValueOf v (assetClass cs (Value.TokenName "CaptiveToken")) == 1

{-# INLINEABLE valueCarriesExponentiallyMintableToken #-}
valueCarriesExponentiallyMintableToken :: Value.CurrencySymbol -> Value.TokenName -> Ledger.Value -> Bool
valueCarriesExponentiallyMintableToken cs tok v = assetClassValueOf v (assetClass cs tok) == 1

{-# INLINEABLE paysAdaInValueToAddr #-}
paysAdaInValueToAddr :: Ledger.TxInfo -> Ledger.Value -> Ledger.Address -> Bool
paysAdaInValueToAddr txInfo rv addr =
  case filter (\txo -> addr == Ledger.txOutAddress txo) (Ledger.txInfoOutputs txInfo) of
    [Ledger.TxOut _ wv _] -> traceIfFalse "does not return ada to copy creator" $ wv == Ada.toValue (Ada.fromValue rv)
    _ -> traceIfFalse "does not return ada to copy creator in single tx" False

{-# INLINEABLE validateContinuingProliferationParameters #-}
validateContinuingProliferationParameters ::
  Ledger.ScriptContext ->
  ProliferationParameters ->
  Ledger.TokenName ->
  (Ledger.TxOut, b, ProliferationParameters) ->
  Bool
validateContinuingProliferationParameters sc pt exponentialTokenName (output, _, dat) =
  traceIfFalse
    "copy output is not continuing"
    (Ledger.toValidatorHash (Ledger.txOutAddress output) == Just (Ledger.ownHash sc))
    && traceIfFalse
      "output does not carry exponential token"
      (valueCarriesExponentiallyMintableToken (Ledger.ownCurrencySymbol sc) exponentialTokenName (Ledger.txOutValue output))
    && traceIfFalse
      "output does not carry captive token"
      (valueCarriesCaptiveToken (Ledger.ownCurrencySymbol sc) (Ledger.txOutValue output))
    && traceIfFalse
      "pricetracking copy incorrect"
      (dat == pt)

{-# INLINEABLE exponentialMintingPolicy #-}
exponentialMintingPolicy ::
  TxOutRef ->
  Ledger.MintingPolicy
exponentialMintingPolicy params =
  Ledger.mkMintingPolicyScript $
    $$( PlutusTx.compile
          [||Scripts.wrapMintingPolicy . mkExponentialMintingPolicy||]
      )
      `PlutusTx.applyCode` PlutusTx.liftCode params

{-# INLINEABLE exponentialCurrencySymbol #-}
exponentialCurrencySymbol ::
  TxOutRef ->
  Value.CurrencySymbol
exponentialCurrencySymbol params =
  Ledger.scriptCurrencySymbol (exponentialMintingPolicy params)
