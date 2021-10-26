{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-specialise #-}

module ArdanaDollar.PriceOracle.OnChain (
  mkOracleMintingPolicy,
  oracleMintingPolicy,
  OracleMintingParams (..),
  oracleCurrencySymbol,
  PriceTracking (..),
) where

import ArdanaDollar.PriceOracle.Types
import ArdanaDollar.Utils (getAllScriptInputsWithDatum, getContinuingScriptOutputsWithDatum)
import Ledger (
  DatumHash (..),
  member,
  ownCurrencySymbol,
  scriptHashAddress,
  txInInfoOutRef,
  txInfoInputs,
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
import PlutusTx.AssocMap qualified as AssocMap
import PlutusTx.Prelude

{-# INLINEABLE withinInterval #-}
withinInterval :: Integer -> Ledger.TxInfo -> Bool
withinInterval interval txInfo = case width (Ledger.txInfoValidRange txInfo) of
  Nothing -> False
  Just ms -> ms <= interval

{-# INLINEABLE stateTokenValue #-}
stateTokenValue :: Value.CurrencySymbol -> Ledger.Value
stateTokenValue cs = Value.singleton cs (Value.TokenName "PriceTracking") 1

{-# INLINEABLE valueCarriesStateToken #-}
valueCarriesStateToken :: Value.CurrencySymbol -> Ledger.Value -> Bool
valueCarriesStateToken cs v = assetClassValueOf v (assetClass cs (Value.TokenName "PriceTracking")) == 1

{-# INLINEABLE mkOracleMintingPolicy #-}
mkOracleMintingPolicy ::
  OracleMintingParams ->
  OracleMintingRedeemer ->
  Ledger.ScriptContext ->
  Bool
mkOracleMintingPolicy
  params
  Initialise
  sc@Ledger.ScriptContext {scriptContextTxInfo = txInfo} =
    correctMinting && hasStateTokenMintingUTXO && sendsStateTokenToControllingAddress
    where
      hasStateTokenMintingUTXO :: Bool
      hasStateTokenMintingUTXO = any (\i -> txInInfoOutRef i == stateTokenTxOutRef params) $ txInfoInputs txInfo
      minted :: Ledger.Value
      minted = Ledger.txInfoMint txInfo
      stateToken :: Ledger.Value
      stateToken = stateTokenValue (Ledger.ownCurrencySymbol sc)
      correctMinting =
        traceIfFalse
          "incorrect minted amount"
          (minted == stateToken)
      sendsStateTokenToControllingAddress =
        case getAllScriptInputsWithDatum @() sc of
          [(output, _, _)] ->
            traceIfFalse
              "output does not go to controlling address"
              (Ledger.toValidatorHash (Ledger.txOutAddress output) == Just (initialControllingValidator params))
              && traceIfFalse
                "incorrect output value"
                (Ledger.txOutValue output == stateToken)
          _ ->
            traceIfFalse "no unique state token carrying UTXO found in output" False
mkOracleMintingPolicy
  _
  Update
  sc@Ledger.ScriptContext {scriptContextTxInfo = txInfo} =
    narrowInterval && correctMinting && lastUpdateInRange && inputCarriesStateToken
      && continuingCertificationsAreValid
      && atLeastOneCertificationProduced
      && sendsStateTokenToControllingAddress
    where
      range :: Ledger.POSIXTimeRange
      range = Ledger.txInfoValidRange txInfo
      narrowInterval :: Bool
      narrowInterval = traceIfFalse "timestamp outwith interval" $ withinInterval narIntW txInfo
      minted :: Ledger.Value
      minted = Ledger.txInfoMint txInfo
      certificationToken :: Ledger.Value
      certificationToken = Value.singleton (Ledger.ownCurrencySymbol sc) (Value.TokenName priceTrackingDatumHash) 1
      certificationTokensMinted :: Ledger.Value
      certificationTokensMinted = Value.singleton (Ledger.ownCurrencySymbol sc) (Value.TokenName priceTrackingDatumHash) numCertifications
      certificationOutput =
        PriceTrackingCertification
          (scriptHashAddress contScr)
          certExp
          reqRep
          narIntW
      correctMinting =
        traceIfFalse
          "incorrect minted amount"
          (minted == certificationTokensMinted)
      atLeastOneCertificationProduced = traceIfFalse "at least one certification must be produced" (numCertifications > 0)
      (continuingCertificationsAreValid, numCertifications) = case getContinuingScriptOutputsWithDatum @PriceTrackingCertification sc of
        outputs -> (all (validateContinuingPriceTrackingCertification sc certificationOutput certificationToken) outputs, length outputs)
      sendsStateTokenToControllingAddress =
        case getAllScriptInputsWithDatum @() sc of
          [(output, _, _)] ->
            traceIfFalse
              "output does not go to controlling address"
              (Ledger.toValidatorHash (Ledger.txOutAddress output) == Just contScr)
              && traceIfFalse
                "incorrect output value"
                (valueCarriesStateToken (ownCurrencySymbol sc) (Ledger.txOutValue output))
          _ ->
            traceIfFalse "no unique state token carrying UTXO found in output" False
      inputCarriesStateToken =
        traceIfFalse
          "incorrect state token input value"
          (valueCarriesStateToken (ownCurrencySymbol sc) (Ledger.txOutValue input))
      lastUpdateInRange = traceIfFalse "lastUpdate not in range" (lastUp `member` range)
      (input, priceTrackingDatumHash, PriceTracking _ _ lastUp contScr reqRep certExp narIntW) =
        case getAllScriptInputsWithDatum @PriceTracking sc of
          [(i, DatumHash hsh, pt)] -> (i, hsh, pt)
          _ -> traceError "no unique PriceTracking carrying UTXO found in input"
mkOracleMintingPolicy
  _
  (CopyCertification returnAddress)
  sc@Ledger.ScriptContext {scriptContextTxInfo = txInfo} =
    narrowInterval && correctMinting && expiryNotInRange
      && repaysCopyCreator
      && createsAtLeastNCopies
      && continuingCertificationsAreValid
    where
      range :: Ledger.POSIXTimeRange
      range = Ledger.txInfoValidRange txInfo
      expiryNotInRange = not $ certExp `Ledger.member` range
      narrowInterval :: Bool
      narrowInterval = traceIfFalse "timestamp outwith interval" $ withinInterval narWidth txInfo
      minted :: Ledger.Value
      minted = Ledger.txInfoMint txInfo
      certificationToken :: Ledger.Value
      certificationToken = Value.singleton (Ledger.ownCurrencySymbol sc) certificationTokenName 1
      certificationTokensMinted :: Ledger.Value
      certificationTokensMinted = Value.singleton (Ledger.ownCurrencySymbol sc) certificationTokenName numCertifications
      certificationTokenName = certificationTokenNameInValue certificationValue (Ledger.ownCurrencySymbol sc)
      continuingOutput = PriceTrackingCertification returnAddress certExp requiredReplications narWidth
      correctMinting =
        traceIfFalse
          "incorrect minted amount"
          (minted == certificationTokensMinted)
      createsAtLeastNCopies = traceIfFalse "does not create required replications" (numCertifications >= requiredReplications)
      repaysCopyCreator = paysAdaInValueToAddr txInfo certificationValue repayAddr
      (certificationValue, repayAddr, certExp, requiredReplications, narWidth) =
        case getAllScriptInputsWithDatum @PriceTrackingCertification sc of
          [(output, _, PriceTrackingCertification payToAddr expiry replications narWidth')] ->
            (Ledger.txOutValue output, payToAddr, expiry, replications, narWidth')
          _ -> traceError "can only copy one at a time"
      (continuingCertificationsAreValid, numCertifications) =
        let outputs = getContinuingScriptOutputsWithDatum @PriceTrackingCertification sc
         in (all (validateContinuingPriceTrackingCertification sc continuingOutput certificationToken) outputs, length outputs)
mkOracleMintingPolicy
  _
  DestroyCertification
  sc@Ledger.ScriptContext {scriptContextTxInfo = txInfo} =
    narrowInterval && correctMinting && expiryInRange && repaysCopyCreator
    where
      range :: Ledger.POSIXTimeRange
      range = Ledger.txInfoValidRange txInfo
      expiryInRange = certExp `Ledger.member` range
      narrowInterval :: Bool
      narrowInterval = traceIfFalse "timestamp outwith interval" $ withinInterval narWidth txInfo
      minted :: Ledger.Value
      minted = Ledger.txInfoMint txInfo
      certificationTokensMinted :: Ledger.Value
      certificationTokensMinted = Value.singleton (Ledger.ownCurrencySymbol sc) certificationTokenName (-1)
      certificationTokenName = certificationTokenNameInValue certificationValue (Ledger.ownCurrencySymbol sc)
      correctMinting =
        traceIfFalse
          "incorrect minted amount"
          (minted == certificationTokensMinted)
      repaysCopyCreator = paysAdaInValueToAddr txInfo certificationValue repayAddr
      (certificationValue, repayAddr, certExp, _, narWidth) =
        case getAllScriptInputsWithDatum @PriceTrackingCertification sc of
          [(output, _, PriceTrackingCertification payToAddr expiry replications narWidth')] ->
            (Ledger.txOutValue output, payToAddr, expiry, replications, narWidth')
          _ -> traceError "can only destroy one at a time"

{-# INLINEABLE certificationTokenNameInValue #-}
certificationTokenNameInValue :: Ledger.Value -> Ledger.CurrencySymbol -> Ledger.TokenName
certificationTokenNameInValue v cs =
  case AssocMap.lookup cs (Value.getValue v) of
    Nothing -> traceError "currency symbol not found in value"
    Just so -> case AssocMap.keys so of
      [tok] -> tok
      _ -> traceError "expected single token name"

{-# INLINEABLE paysAdaInValueToAddr #-}
paysAdaInValueToAddr :: Ledger.TxInfo -> Ledger.Value -> Ledger.Address -> Bool
paysAdaInValueToAddr txInfo rv addr =
  case filter (\txo -> addr == Ledger.txOutAddress txo) (Ledger.txInfoOutputs txInfo) of
    [Ledger.TxOut _ wv _] -> traceIfFalse "does not return ada to copy creator" $ wv == Ada.toValue (Ada.fromValue rv)
    _ -> traceIfFalse "does not return ada to copy creator in single tx" False

{-# INLINEABLE validateContinuingPriceTrackingCertification #-}
validateContinuingPriceTrackingCertification ::
  Ledger.ScriptContext ->
  PriceTrackingCertification ->
  Ledger.Value ->
  (Ledger.TxOut, b, PriceTrackingCertification) ->
  Bool
validateContinuingPriceTrackingCertification sc pt certificationToken (output, _, dat) =
  traceIfFalse
    "copy output is not continuing"
    (Ledger.toValidatorHash (Ledger.txOutAddress output) == Just (Ledger.ownHash sc))
    && traceIfFalse
      "incorrect state token output value"
      (Ledger.txOutValue output == certificationToken)
    && traceIfFalse
      "pricetracking copy incorrect"
      (dat == pt)

{-# INLINEABLE oracleMintingPolicy #-}
oracleMintingPolicy ::
  OracleMintingParams ->
  Ledger.MintingPolicy
oracleMintingPolicy params =
  Ledger.mkMintingPolicyScript $
    $$( PlutusTx.compile
          [||Scripts.wrapMintingPolicy . mkOracleMintingPolicy||]
      )
      `PlutusTx.applyCode` PlutusTx.liftCode params

{-# INLINEABLE oracleCurrencySymbol #-}
oracleCurrencySymbol ::
  OracleMintingParams ->
  Value.CurrencySymbol
oracleCurrencySymbol params =
  Ledger.scriptCurrencySymbol (oracleMintingPolicy params)
