{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-specialise #-}
module ArdanaDollar.PriceOracle.OnChain (
  getScriptOutputsWithDatum,
  mkOracleMintingPolicy,
  mkOracleValidator,
  oracleCompiledTypedValidator,
  OracleMintingParams (..),
  oracleMintingPolicy,
  OracleValidatorParams (..),
  PriceTracking (..),
) where

import ArdanaDollar.Utils (getScriptOutputsWithDatum)
import Data.Aeson qualified as JSON
import GHC.Generics (Generic)
import Ledger qualified
import Ledger.Oracle qualified as Oracle
import Ledger.Typed.Scripts qualified as Scripts
import Ledger.Value qualified as Value
import Plutus.V1.Ledger.Interval.Extra (width)
import PlutusTx qualified
import PlutusTx.Prelude
import PlutusTx.UniqueMap qualified as UniqueMap
import Prelude qualified as Haskell

data OracleValidatorParams = OracleValidatorParams
  { oracleValidatorParams'oracleMintingCurrencySymbol :: !Value.CurrencySymbol
  , oracleValidatorParams'operator :: !Ledger.PubKey
  , oracleValidatorParams'operatorPkh :: !Ledger.PubKeyHash
  , oracleValidatorParams'peggedCurrency :: !BuiltinByteString
  }
  deriving stock (Haskell.Show, Generic)
  deriving anyclass (JSON.FromJSON, JSON.ToJSON)
PlutusTx.makeLift ''OracleValidatorParams

data PriceTracking = PriceTracking
  { priceTracking'fiatPriceFeed :: UniqueMap.Map BuiltinByteString Integer
  , priceTracking'cryptoPriceFeed :: UniqueMap.Map Value.AssetClass Integer
  , priceTracking'lastUpdate :: Ledger.POSIXTime
  }
  deriving stock (Haskell.Show, Generic)
  deriving anyclass (JSON.FromJSON, JSON.ToJSON)
PlutusTx.makeIsDataIndexed ''PriceTracking [('PriceTracking, 0)]

data OracleMintingParams = OracleMintingParams
  { oracleMintingParams'operator :: !Ledger.PubKey
  , oracleMintingParams'operatorPkh :: !Ledger.PubKeyHash
  }
  deriving stock (Haskell.Show, Generic)
  deriving anyclass (JSON.FromJSON, JSON.ToJSON)
PlutusTx.makeLift ''OracleMintingParams

{-# INLINEABLE checkMessageOutput #-}
checkMessageOutput ::
  Ledger.PubKey ->
  Ledger.ValidatorHash ->
  Ledger.POSIXTimeRange ->
  Ledger.Value ->
  Ledger.TxOut ->
  Oracle.SignedMessage PriceTracking ->
  Bool
checkMessageOutput
  oracleOperatorPubKey
  oracleValidator_
  range
  outputValue
  output
  (Oracle.SignedMessage sig hash dat) =
    traceIfFalse
      "cryptographic signature is incorrect"
      (isRight $ Oracle.checkSignature hash oracleOperatorPubKey sig)
      && traceIfFalse
        "does not go to oracle validator"
        (Ledger.toValidatorHash (Ledger.txOutAddress output) == Just oracleValidator_)
      && traceIfFalse
        "incorrect output value"
        (Ledger.txOutValue output == outputValue)
      && traceIfFalse
        "incorrect PriceTracking datum"
        ( case PlutusTx.fromBuiltinData @PriceTracking (Ledger.getDatum dat) of
            Nothing ->
              False
            Just (PriceTracking _ _ upd) ->
              upd `Ledger.member` range
        )

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
  Ledger.ValidatorHash ->
  OracleMintingParams ->
  () ->
  Ledger.ScriptContext ->
  Bool
mkOracleMintingPolicy
  oracle
  (OracleMintingParams op opPkh)
  _
  sc@Ledger.ScriptContext {scriptContextTxInfo = txInfo} =
    narrowInterval && correctMinting && txSignedByOperator && priceMessageToOracle
    where
      range :: Ledger.POSIXTimeRange
      range = Ledger.txInfoValidRange txInfo
      narrowInterval :: Bool
      narrowInterval = withinInterval 10000 txInfo
      minted :: Ledger.Value
      minted = Ledger.txInfoMint txInfo
      expected :: Ledger.Value
      expected = stateTokenValue (Ledger.ownCurrencySymbol sc)
      correctMinting =
        traceIfFalse
          "incorrect minted amount"
          (minted == expected)
      txSignedByOperator =
        traceIfFalse
          "not signed by oracle operator"
          (Ledger.txSignedBy txInfo opPkh)
      priceMessageToOracle = case getScriptOutputsWithDatum @(Oracle.SignedMessage PriceTracking) sc of
        [(output, dat)] ->
          checkMessageOutput
            op
            oracle
            range
            expected
            output
            dat
            && traceIfFalse
              "no PriceTracking datum"
              ( case PlutusTx.fromBuiltinData @PriceTracking (Ledger.getDatum $ Oracle.osmDatum dat) of
                  Nothing ->
                    False
                  Just (PriceTracking fiatFeed cryptoFeed _) ->
                    UniqueMap.null fiatFeed
                      && UniqueMap.null cryptoFeed
              )
        _ ->
          traceIfFalse "no unique PriceTracking carrying UTXO found" False

{-# INLINEABLE oracleMintingPolicy #-}
oracleMintingPolicy ::
  Ledger.ValidatorHash ->
  OracleMintingParams ->
  Ledger.MintingPolicy
oracleMintingPolicy oracle params =
  Ledger.mkMintingPolicyScript $
    $$( PlutusTx.compile
          [||
          \o p ->
            Scripts.wrapMintingPolicy
              (mkOracleMintingPolicy o p)
          ||]
      )
      `PlutusTx.applyCode` PlutusTx.liftCode oracle
      `PlutusTx.applyCode` PlutusTx.liftCode params

{-# INLINEABLE oracleCurrencySymbol #-}
oracleCurrencySymbol ::
  Ledger.ValidatorHash ->
  OracleMintingParams ->
  Value.CurrencySymbol
oracleCurrencySymbol oracle params =
  Ledger.scriptCurrencySymbol (oracleMintingPolicy oracle params)

{-# INLINEABLE mkOracleValidator #-}
mkOracleValidator ::
  OracleValidatorParams ->
  Oracle.SignedMessage PriceTracking ->
  () ->
  Ledger.ScriptContext ->
  Bool
mkOracleValidator
  (OracleValidatorParams curSymbol op opPkh _)
  _
  _
  sc@Ledger.ScriptContext {scriptContextTxInfo = txInfo} =
    narrowInterval && txSignedByOperator && priceMessageToOracle
    where
      narrowInterval :: Bool
      narrowInterval = withinInterval 10000 txInfo
      expectedOutVal :: Ledger.Value
      expectedOutVal = stateTokenValue curSymbol
      txSignedByOperator :: Bool
      txSignedByOperator = Ledger.txSignedBy txInfo opPkh
      priceMessageToOracle = case getScriptOutputsWithDatum @(Oracle.SignedMessage PriceTracking) sc of
        [(output, dat)] ->
          checkMessageOutput
            op
            (Ledger.ownHash sc)
            (Ledger.txInfoValidRange txInfo)
            expectedOutVal
            output
            dat
        _ ->
          traceIfFalse "no unique PriceTracking carrying UTXO found" False

data PriceOracling
instance Scripts.ValidatorTypes PriceOracling where
  type DatumType PriceOracling = Oracle.SignedMessage PriceTracking
  type RedeemerType PriceOracling = ()

{-# INLINEABLE oracleCompiledTypedValidator #-}
oracleCompiledTypedValidator ::
  OracleValidatorParams ->
  PlutusTx.CompiledCode (Oracle.SignedMessage PriceTracking -> () -> Ledger.ScriptContext -> Bool)
oracleCompiledTypedValidator params =
  $$(PlutusTx.compile [||mkOracleValidator||])
    `PlutusTx.applyCode` PlutusTx.liftCode params

{-# INLINEABLE oracleInst #-}
oracleInst :: OracleValidatorParams -> Scripts.TypedValidator PriceOracling
oracleInst params =
  Scripts.mkTypedValidator @PriceOracling
    (oracleCompiledTypedValidator params)
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.wrapValidator @(Oracle.SignedMessage PriceTracking) @()

{-# INLINEABLE oracleValidator #-}
oracleValidator :: OracleValidatorParams -> Ledger.Validator
oracleValidator = Scripts.validatorScript . oracleInst

{-# INLINEABLE oracleAddress #-}
oracleAddress :: OracleValidatorParams -> Ledger.Address
oracleAddress = Ledger.scriptAddress . oracleValidator
