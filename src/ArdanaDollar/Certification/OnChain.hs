{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-specialise #-}

module ArdanaDollar.Certification.OnChain (
  mkCertificationMintingPolicy,
  certificationMintingPolicy,
  certificationCurrencySymbol,
  CertifiedDatum (..),
) where

import ArdanaDollar.Certification.Types
import ArdanaDollar.Utils (getAllScriptInputsWithDatum, getContinuingScriptOutputsWithDatum)
import Ledger (
  DatumHash (..),
  TxOutRef,
  member,
  ownCurrencySymbol,
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
stateTokenValue cs = Value.singleton cs (Value.TokenName "CertificationAuthority") 1

{-# INLINEABLE valueCarriesStateToken #-}
valueCarriesStateToken :: Value.CurrencySymbol -> Ledger.Value -> Bool
valueCarriesStateToken cs v = assetClassValueOf v (assetClass cs (Value.TokenName "CertificationAuthority")) == 1

{-# INLINEABLE mkCertificationMintingPolicy #-}
mkCertificationMintingPolicy ::
  TxOutRef ->
  CertificationMintingRedeemer ->
  Ledger.ScriptContext ->
  Bool
mkCertificationMintingPolicy
  oneShotTx
  Initialise
  sc@Ledger.ScriptContext {scriptContextTxInfo = txInfo} =
    correctMinting && hasStateTokenMintingUTXO
    where
      hasStateTokenMintingUTXO :: Bool
      hasStateTokenMintingUTXO = any (\i -> txInInfoOutRef i == oneShotTx) $ txInfoInputs txInfo
      minted :: Ledger.Value
      minted = Ledger.txInfoMint txInfo
      stateToken :: Ledger.Value
      stateToken = stateTokenValue (Ledger.ownCurrencySymbol sc)
      correctMinting =
        traceIfFalse
          "incorrect minted amount"
          (minted == stateToken)
mkCertificationMintingPolicy
  _
  (Update returnAddress)
  sc@Ledger.ScriptContext {scriptContextTxInfo = txInfo} =
    narrowInterval && correctMinting && lastUpdateInRange && inputCarriesStateToken
      && continuingCertificationsAreValid
      && atLeastOneCertificationProduced
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
        CertificationCopyingParameters
          returnAddress
          certExp
          reqRep
          narIntW
      correctMinting =
        traceIfFalse
          "incorrect minted amount"
          (minted == certificationTokensMinted)
      atLeastOneCertificationProduced = traceIfFalse "at least one certification must be produced" (numCertifications > 0)
      (continuingCertificationsAreValid, numCertifications) = case getContinuingScriptOutputsWithDatum @CertificationCopyingParameters sc of
        outputs -> (all (validateContinuingCertificationCopyingParameters sc certificationOutput certificationToken) outputs, length outputs)
      inputCarriesStateToken =
        traceIfFalse
          "incorrect state token input value"
          (valueCarriesStateToken (ownCurrencySymbol sc) (Ledger.txOutValue input))
      lastUpdateInRange = traceIfFalse "lastUpdate not in range" (lastUp `member` range)
      (input, priceTrackingDatumHash, CertifiedDatum _ lastUp reqRep certExp narIntW) =
        case getAllScriptInputsWithDatum @CertifiedDatum sc of
          [(i, DatumHash hsh, pt)] -> (i, hsh, pt)
          _ -> traceError "no unique CertifiedDatum carrying UTXO found in input"
mkCertificationMintingPolicy
  _
  (CopyCertificationToken returnAddress)
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
      continuingOutput = CertificationCopyingParameters returnAddress certExp requiredReplications narWidth
      correctMinting =
        traceIfFalse
          "incorrect minted amount"
          (minted == certificationTokensMinted)
      createsAtLeastNCopies = traceIfFalse "does not create required replications" (numCertifications >= requiredReplications)
      repaysCopyCreator = paysAdaInValueToAddr txInfo certificationValue repayAddr
      (certificationValue, repayAddr, certExp, requiredReplications, narWidth) =
        case getAllScriptInputsWithDatum @CertificationCopyingParameters sc of
          [(output, _, CertificationCopyingParameters payToAddr expiry' replications' narWidth')] ->
            (Ledger.txOutValue output, payToAddr, expiry', replications', narWidth')
          _ -> traceError "can only copy one at a time"
      (continuingCertificationsAreValid, numCertifications) =
        let outputs = getContinuingScriptOutputsWithDatum @CertificationCopyingParameters sc
         in (all (validateContinuingCertificationCopyingParameters sc continuingOutput certificationToken) outputs, length outputs)
mkCertificationMintingPolicy
  _
  DestroyCertificationToken
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
        case getAllScriptInputsWithDatum @CertificationCopyingParameters sc of
          [(output, _, CertificationCopyingParameters payToAddr expiry' replications' narWidth')] ->
            (Ledger.txOutValue output, payToAddr, expiry', replications', narWidth')
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

{-# INLINEABLE validateContinuingCertificationCopyingParameters #-}
validateContinuingCertificationCopyingParameters ::
  Ledger.ScriptContext ->
  CertificationCopyingParameters ->
  Ledger.Value ->
  (Ledger.TxOut, b, CertificationCopyingParameters) ->
  Bool
validateContinuingCertificationCopyingParameters sc pt certificationToken (output, _, dat) =
  traceIfFalse
    "copy output is not continuing"
    (Ledger.toValidatorHash (Ledger.txOutAddress output) == Just (Ledger.ownHash sc))
    && traceIfFalse
      "incorrect state token output value"
      (Ledger.txOutValue output == certificationToken)
    && traceIfFalse
      "pricetracking copy incorrect"
      (dat == pt)

{-# INLINEABLE certificationMintingPolicy #-}
certificationMintingPolicy ::
  TxOutRef ->
  Ledger.MintingPolicy
certificationMintingPolicy params =
  Ledger.mkMintingPolicyScript $
    $$( PlutusTx.compile
          [||Scripts.wrapMintingPolicy . mkCertificationMintingPolicy||]
      )
      `PlutusTx.applyCode` PlutusTx.liftCode params

{-# INLINEABLE certificationCurrencySymbol #-}
certificationCurrencySymbol ::
  TxOutRef ->
  Value.CurrencySymbol
certificationCurrencySymbol params =
  Ledger.scriptCurrencySymbol (certificationMintingPolicy params)
