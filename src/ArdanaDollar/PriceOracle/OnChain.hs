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
  OracleValidatorParams (..),
  PriceTracking (..),
) where
import ArdanaDollar.PriceOracle.Types
import ArdanaDollar.Utils (getContinuingScriptOutputsWithDatum,getAllScriptInputsWithDatum)
import Ledger qualified
import Ledger.Typed.Scripts qualified as Scripts
import Ledger.Value qualified as Value
import Plutus.V1.Ledger.Interval.Extra (width)
import PlutusTx qualified
import PlutusTx.Prelude
import PlutusTx.UniqueMap qualified as UniqueMap
import PlutusTx.AssocMap as AssocMap



{-# INLINEABLE withinInterval #-}
withinInterval :: Integer -> Ledger.TxInfo -> Bool
withinInterval interval txInfo = case width (Ledger.txInfoValidRange txInfo) of
  Nothing -> False
  Just ms -> ms <= interval

{-# INLINEABLE stateTokenValue #-}
stateTokenValue :: Value.CurrencySymbol -> Ledger.Value
stateTokenValue cs = Value.singleton cs (Value.TokenName "PriceTracking") 1

{-# INLINEABLE mkOracleMintingPolicy #-}
mkOracleMintingPolicy ::
  OracleMintingParams ->
  OracleMintingRedeemer ->
  Ledger.ScriptContext ->
  Bool
mkOracleMintingPolicy
  (OracleMintingParams _ opPkh _)
  Initialise
  sc@Ledger.ScriptContext {scriptContextTxInfo = txInfo} =
    narrowInterval && correctMinting && txSignedByOperator && hasContinuingState
    where
      range :: Ledger.POSIXTimeRange
      range = Ledger.txInfoValidRange txInfo
      narrowInterval :: Bool
      narrowInterval = traceIfFalse "timestamp outwith interval" $ withinInterval 10000 txInfo
      minted :: Ledger.Value
      minted = Ledger.txInfoMint txInfo
      stateToken :: Ledger.Value
      stateToken = stateTokenValue (Ledger.ownCurrencySymbol sc)
      correctMinting =
        traceIfFalse
          "incorrect minted amount"
          (minted == stateToken)
      txSignedByOperator =
        traceIfFalse
          "not signed by oracle operator"
          (Ledger.txSignedBy txInfo opPkh)
      hasContinuingState = case getContinuingScriptOutputsWithDatum @PriceTracking sc of
        [(output, _, dat)] ->
            traceIfFalse
              "output is not continuing"
              (Ledger.toValidatorHash (Ledger.txOutAddress output) == Just (Ledger.ownHash sc))
            && traceIfFalse
              "incorrect output value"
              (Ledger.txOutValue output == stateToken)
            && traceIfFalse
              "no PriceTracking datum"
              ( case dat of
                  (PriceTracking fiatFeed cryptoFeed upd) ->
                    UniqueMap.null fiatFeed
                      && UniqueMap.null cryptoFeed
                      && upd `Ledger.member` range
              )
        _ ->
          traceIfFalse "no unique PriceTracking carrying UTXO found in output" False
mkOracleMintingPolicy
  (OracleMintingParams _ opPkh changeAddress)
  (Update p@PriceTracking{} expiry replications)
  sc@Ledger.ScriptContext {scriptContextTxInfo = txInfo} =
    narrowInterval && correctMinting && txSignedByOperator && hasContinuingState && hasContinuingPriceTrackingCertifications
    where
      range :: Ledger.POSIXTimeRange
      range = Ledger.txInfoValidRange txInfo
      narrowInterval :: Bool
      narrowInterval = traceIfFalse "timestamp outwith interval" $ withinInterval 10000 txInfo
      minted :: Ledger.Value
      minted = Ledger.txInfoMint txInfo
      stateToken :: Ledger.Value
      stateToken = stateTokenValue (Ledger.ownCurrencySymbol sc)
      certificationToken :: Ledger.Value
      certificationToken = Value.singleton (Ledger.ownCurrencySymbol sc) (Value.TokenName priceTrackingDatumHash) 1
      certificationOutput = PriceTrackingCopy changeAddress expiry replications
      correctMinting =
            traceIfFalse
              "incorrect minted amount"
              (minted == mconcat (replicate numCopies certificationToken))
      txSignedByOperator =
        traceIfFalse
          "not signed by oracle operator"
          (Ledger.txSignedBy txInfo opPkh)
      (hasContinuingPriceTrackingCertifications,numCopies) = case getContinuingScriptOutputsWithDatum @PriceTrackingCopy sc of
        [] -> (traceIfFalse "no PriceTrackingCopy produced" False,0)
        outputs -> (all (validateContinuingPriceTrackingCertification sc certificationOutput certificationToken) outputs, length outputs)

      (hasContinuingState,Ledger.DatumHash priceTrackingDatumHash) = case getContinuingScriptOutputsWithDatum @PriceTracking sc of
        [(output, dhsh, dat)] ->
          (traceIfFalse
              "state output is not continuing"
              (Ledger.toValidatorHash (Ledger.txOutAddress output) == Just (Ledger.ownHash sc))
            && traceIfFalse
              "incorrect state token output value"
              (Ledger.txOutValue output == stateToken)
            && traceIfFalse
              "no PriceTracking datum"
              ( case dat of
                  op@(PriceTracking _ _ upd) ->
                    p == op
                    && upd `Ledger.member` range
              )
          , dhsh)
        _ ->
          traceError "no unique PriceTracking carrying UTXO found in output"
mkOracleMintingPolicy
  (OracleMintingParams _ _ _)
  (CreateCopy changeAddress)
  sc@Ledger.ScriptContext {scriptContextTxInfo = txInfo} =
    narrowInterval && correctMinting && expiryInRange && doesNotConsumeState && repaysAuthor && createsAtLeastNCopies && hasContinuingPriceTrackingCertifications
    where
      range :: Ledger.POSIXTimeRange
      range = Ledger.txInfoValidRange txInfo
      expiryInRange = copiedExpiry `Ledger.member` range
      narrowInterval :: Bool
      narrowInterval = traceIfFalse "timestamp outwith interval" $ withinInterval 10000 txInfo
      minted :: Ledger.Value
      minted = Ledger.txInfoMint txInfo
      certificationToken :: Ledger.Value
      certificationToken = Value.singleton (Ledger.ownCurrencySymbol sc) certificationTokenName 1
      certificationTokenName = certificationTokenNameInValue copiedValue (Ledger.ownCurrencySymbol sc)
      continuingOutput = PriceTrackingCopy changeAddress copiedExpiry requiredReplications
      correctMinting =
            traceIfFalse
              "incorrect minted amount"
              (minted == mconcat (replicate numCopies certificationToken))
      createsAtLeastNCopies = traceIfFalse "does not create required replications" (numCopies >= requiredReplications)
      repaysAuthor = paysAdaInValueToAddr sc copiedValue repayAddr
      (copiedValue,repayAddr,copiedExpiry,requiredReplications) = case getAllScriptInputsWithDatum @PriceTrackingCopy sc of
                  [(output,_,PriceTrackingCopy payToAddr expiry replications)] ->
                    (Ledger.txOutValue output, payToAddr, expiry,replications)
                  _ -> traceError "can only copy one at a time"
      doesNotConsumeState = case getAllScriptInputsWithDatum @PriceTracking sc of
                              [] -> True
                              _ -> traceIfFalse "cannot consume state" False
      (hasContinuingPriceTrackingCertifications,numCopies) = case getContinuingScriptOutputsWithDatum @PriceTrackingCopy sc of
        [] -> (traceIfFalse "no PriceTrackingCopy produced" False,0)
        outputs -> (all (validateContinuingPriceTrackingCertification sc continuingOutput certificationToken) outputs, length outputs)
mkOracleMintingPolicy _ _ _ = False --TODO the rest of the owl


{-# INLINEABLE certificationTokenNameInValue #-}
certificationTokenNameInValue :: Ledger.Value -> Ledger.CurrencySymbol -> Ledger.TokenName
certificationTokenNameInValue v cs =
  case AssocMap.lookup cs (Value.getValue v) of
    Nothing -> traceError "currency symbol not found in value"
    Just so -> case AssocMap.keys so of
                 [tok] -> tok
                 _ -> traceError "expected single token name"

{-# INLINEABLE paysAdaInValueToAddr #-}
paysAdaInValueToAddr :: Ledger.ScriptContext -> Ledger.Value -> Ledger.Address -> Bool
paysAdaInValueToAddr _ _ _ = False

{-# INLINEABLE replicate #-}
replicate :: Integer -> a -> [a]
replicate i a | i > 0 = a : replicate (i - 1) a
replicate _ _ = []

{-# INLINEABLE validateContinuingPriceTrackingCertification #-}
validateContinuingPriceTrackingCertification ::
  Ledger.ScriptContext ->
  PriceTrackingCopy ->
  Ledger.Value ->
  (Ledger.TxOut,b,PriceTrackingCopy) ->
  Bool
validateContinuingPriceTrackingCertification sc pt certificationToken (output,_,dat) =
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

