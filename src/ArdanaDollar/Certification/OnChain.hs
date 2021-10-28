{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-specialise #-}

module ArdanaDollar.Certification.OnChain (
  mkCertificationMintingPolicy,
  certificationMintingPolicy,
  certificationCurrencySymbol,
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
    correctMinting && hasCertificationAuthorityTokenMintingUTXO
    where
      hasCertificationAuthorityTokenMintingUTXO :: Bool
      hasCertificationAuthorityTokenMintingUTXO = any (\i -> txInInfoOutRef i == oneShotTx) $ txInfoInputs txInfo
      minted :: Ledger.Value
      minted = Ledger.txInfoMint txInfo
      certificationAuthorityToken :: Ledger.Value
      certificationAuthorityToken = certificationAuthorityTokenValue (Ledger.ownCurrencySymbol sc)
      correctMinting =
        traceIfFalse
          "incorrect minted amount"
          (minted == certificationAuthorityToken)
mkCertificationMintingPolicy
  _
  (Certify adaReturnAddress Certification {..})
  sc@Ledger.ScriptContext {scriptContextTxInfo = txInfo} =
    correctMinting
      && outputCarriesCertificationAuthorityToken
      && continuingCertificationsAreValid
      && atLeastOneCertificationProduced
    where
      minted :: Ledger.Value
      minted = Ledger.txInfoMint txInfo
      certificationTokenName :: Value.TokenName
      certificationTokenName = let DatumHash name = certifiedDatumHash in Value.TokenName name
      certificationTokensMinted :: Ledger.Value
      certificationTokensMinted =
        let DatumHash name = certifiedDatumHash
         in Value.singleton (Ledger.ownCurrencySymbol sc) (Value.TokenName name) numCertifications
      captiveTokensMinted :: Ledger.Value
      captiveTokensMinted = captiveTokenValue (Ledger.ownCurrencySymbol sc) numCertifications
      certificationOutput :: ProliferationParameters
      certificationOutput =
        ProliferationParameters
          adaReturnAddress
          minReplications
          certificationExpiry
          narrowIntervalWidth
      correctMinting :: Bool
      correctMinting =
        traceIfFalse
          "incorrect minted amount"
          (minted == certificationTokensMinted <> captiveTokensMinted)
      atLeastOneCertificationProduced :: Bool
      atLeastOneCertificationProduced = traceIfFalse "at least one certification must be produced" (numCertifications > 0)
      continuingCertificationsAreValid :: Bool
      numCertifications :: Integer
      (continuingCertificationsAreValid, numCertifications) = case getContinuingScriptOutputsWithDatum @ProliferationParameters sc of
        outputs -> (all (validateContinuingProliferationParameters sc certificationOutput certificationTokenName) outputs, length outputs)
      outputCarriesCertificationAuthorityToken :: Bool
      outputCarriesCertificationAuthorityToken =
        let outputValue = mconcat (txOutValue <$> txInfoOutputs txInfo)
         in traceIfFalse
              "CertificationAuthorityToken not present"
              (valueCarriesCertificationAuthorityToken (ownCurrencySymbol sc) outputValue)
mkCertificationMintingPolicy
  _
  (CopyCertificationToken adaReturnAddress (DatumHash certificationTokenNameBS))
  sc@Ledger.ScriptContext {scriptContextTxInfo = txInfo} =
    correctMinting
      && repaysCopyCreator
      && createsAtLeastNCopies
      && continuingCertificationsAreValid
    where
      minted :: Ledger.Value
      minted = Ledger.txInfoMint txInfo
      certificationTokenName :: Ledger.TokenName
      certificationTokenName = Value.TokenName certificationTokenNameBS
      certificationTokensMinted :: Ledger.Value
      certificationTokensMinted = Value.singleton (Ledger.ownCurrencySymbol sc) certificationTokenName numCertifications
      captiveTokensMinted :: Ledger.Value
      captiveTokensMinted = captiveTokenValue (Ledger.ownCurrencySymbol sc) (numCertifications - 1)
      proliferatedParameters :: ProliferationParameters
      proliferatedParameters = p {adaReturnAddress' = adaReturnAddress}
      correctMinting :: Bool
      correctMinting =
        traceIfFalse
          "incorrect minted amount"
          (minted == certificationTokensMinted <> captiveTokensMinted)
      createsAtLeastNCopies :: Bool
      createsAtLeastNCopies = traceIfFalse "does not create required replications" (numCertifications >= minReplications')
      repaysCopyCreator :: Bool
      repaysCopyCreator = paysAdaInValueToAddr txInfo certificationValue adaReturnAddress'
      certificationValue :: Ledger.Value
      p :: ProliferationParameters
      (certificationValue, p@ProliferationParameters {..}) =
        case getAllScriptInputsWithDatum @ProliferationParameters sc of
          [(output, _, pp)] ->
            (Ledger.txOutValue output, pp)
          _ -> traceError "can only copy one at a time"
      continuingCertificationsAreValid :: Bool
      numCertifications :: Integer
      (continuingCertificationsAreValid, numCertifications) =
        let outputs = getContinuingScriptOutputsWithDatum @ProliferationParameters sc
         in (all (validateContinuingProliferationParameters sc proliferatedParameters certificationTokenName) outputs, length outputs)
mkCertificationMintingPolicy
  _
  (DestroyCertificationToken (DatumHash certificationTokenNameBS))
  sc@Ledger.ScriptContext {scriptContextTxInfo = txInfo} =
    narrowInterval && correctMinting && expiryInRange && repaysCopyCreator
    where
      range :: Ledger.POSIXTimeRange
      range = Ledger.txInfoValidRange txInfo
      certificationTokenName :: Ledger.TokenName
      certificationTokenName = Value.TokenName certificationTokenNameBS
      expiryInRange :: Bool
      expiryInRange = certificationExpiry' `Ledger.member` range
      narrowInterval :: Bool
      narrowInterval = traceIfFalse "timestamp outwith interval" $ withinInterval narrowIntervalWidth' txInfo
      minted :: Ledger.Value
      minted = Ledger.txInfoMint txInfo
      certificationTokensMinted :: Ledger.Value
      certificationTokensMinted = Value.singleton (Ledger.ownCurrencySymbol sc) certificationTokenName (-1)
      captiveTokensMinted :: Ledger.Value
      captiveTokensMinted = captiveTokenValue (Ledger.ownCurrencySymbol sc) (-1)
      correctMinting =
        traceIfFalse
          "incorrect minted amount"
          (minted == certificationTokensMinted <> captiveTokensMinted)
      repaysCopyCreator :: Bool
      repaysCopyCreator = paysAdaInValueToAddr txInfo certificationValue adaReturnAddress'
      certificationValue :: Ledger.Value
      (certificationValue, ProliferationParameters {..}) =
        case getAllScriptInputsWithDatum @ProliferationParameters sc of
          [(output, _, pp)] ->
            (Ledger.txOutValue output, pp)
          _ -> traceError "can only destroy one at a time"

{-# INLINEABLE withinInterval #-}
withinInterval :: Integer -> Ledger.TxInfo -> Bool
withinInterval interval txInfo = case width (Ledger.txInfoValidRange txInfo) of
  Nothing -> False
  Just ms -> ms <= interval

{-# INLINEABLE certificationAuthorityTokenValue #-}
certificationAuthorityTokenValue :: Value.CurrencySymbol -> Ledger.Value
certificationAuthorityTokenValue cs = Value.singleton cs (Value.TokenName "CertificationAuthority") 1

{-# INLINEABLE valueCarriesCertificationAuthorityToken #-}
valueCarriesCertificationAuthorityToken :: Value.CurrencySymbol -> Ledger.Value -> Bool
valueCarriesCertificationAuthorityToken cs v = assetClassValueOf v (assetClass cs (Value.TokenName "CertificationAuthority")) == 1

{-# INLINEABLE captiveTokenValue #-}
captiveTokenValue :: Value.CurrencySymbol -> Integer -> Ledger.Value
captiveTokenValue cs = Value.singleton cs (Value.TokenName "CaptiveToken")

{-# INLINEABLE valueCarriesCaptiveToken #-}
valueCarriesCaptiveToken :: Value.CurrencySymbol -> Ledger.Value -> Bool
valueCarriesCaptiveToken cs v = assetClassValueOf v (assetClass cs (Value.TokenName "CaptiveToken")) == 1

{-# INLINEABLE valueCarriesCertificationToken #-}
valueCarriesCertificationToken :: Value.CurrencySymbol -> Value.TokenName -> Ledger.Value -> Bool
valueCarriesCertificationToken cs tok v = assetClassValueOf v (assetClass cs tok) == 1

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
validateContinuingProliferationParameters sc pt certificationTokenName (output, _, dat) =
  traceIfFalse
    "copy output is not continuing"
    (Ledger.toValidatorHash (Ledger.txOutAddress output) == Just (Ledger.ownHash sc))
    && traceIfFalse
      "output does not carry certification token"
      (valueCarriesCertificationToken (Ledger.ownCurrencySymbol sc) certificationTokenName (Ledger.txOutValue output))
    && traceIfFalse
      "output does not carry captive token"
      (valueCarriesCaptiveToken (Ledger.ownCurrencySymbol sc) (Ledger.txOutValue output))
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
