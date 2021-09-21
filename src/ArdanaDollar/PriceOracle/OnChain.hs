{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module ArdanaDollar.PriceOracle.OnChain (
  getScriptOutputsWithDatum,
  mkOracleMintingPolicy,
  mkOracleValidator,
  OracleMintingParams(..),
  oracleMintingPolicy,
  OracleValidatorParams(..),
  PriceTracking(..)
) where

import ArdanaDollar.Utils (getScriptOutputsWithDatum)
import qualified Data.Aeson as JSON
import GHC.Generics (Generic)
import Ledger qualified
import Ledger.Oracle qualified as Oracle
import Ledger.Value qualified as Value
import Ledger.Typed.Scripts qualified as Scripts
import Plutus.V1.Ledger.Interval.Extra (width)
import PlutusTx qualified
import PlutusTx.AssocMap qualified as AssocMap
import PlutusTx.Prelude
import Prelude qualified as Haskell

data OracleValidatorParams = OracleValidatorParams
  { oracleMintingCurrencySymbol :: Value.CurrencySymbol
  , operator :: Ledger.PubKey
  , operatorPkh :: Ledger.PubKeyHash
  , peggedCurrency :: BuiltinByteString
  }
  deriving stock (Haskell.Show, Generic)
  deriving anyclass (JSON.FromJSON, JSON.ToJSON)
PlutusTx.makeLift ''OracleValidatorParams

data PriceTracking = PriceTracking
  { fiatPriceFeed :: AssocMap.Map BuiltinByteString Integer -- fiat currency id to price in lovelace
  , cryptoPriceFeed :: AssocMap.Map Value.AssetClass Integer
  , lastUpdate :: Ledger.POSIXTime
  }
  deriving stock (Haskell.Show, Generic)
  deriving anyclass (JSON.FromJSON, JSON.ToJSON)
PlutusTx.makeIsDataIndexed ''PriceTracking [('PriceTracking, 0)]

data OracleMintingParams = OracleMintingParams
  { operator :: Ledger.PubKey 
  , operatorPkh :: Ledger.PubKeyHash
  }
  deriving stock (Haskell.Show, Generic)
  deriving anyclass (JSON.FromJSON, JSON.ToJSON)
PlutusTx.makeLift ''OracleMintingParams

type OracleDatum = Oracle.SignedMessage PriceTracking

{-# INLINEABLE checkMessageOutput #-}
checkMessageOutput ::
  Ledger.PubKey ->
  Ledger.ValidatorHash ->
  Ledger.POSIXTimeRange ->
  Ledger.Value ->
  Ledger.TxOut -> 
  OracleDatum ->
  Bool
checkMessageOutput
  op
  oracle
  range
  outVal
  out
  (Oracle.SignedMessage sig hash dat) =
  traceIfFalse "cryptographic signature is incorrect"
    (isRight $ Oracle.checkSignature hash op sig)
  && traceIfFalse "does not go to oracle validator"
    (Ledger.toValidatorHash (Ledger.txOutAddress out) == Just oracle)
  && traceIfFalse "incorrect output value"
    (Ledger.txOutValue out == outVal)
  && traceIfFalse "incorrect PriceTracking datum"
      (case PlutusTx.fromBuiltinData @PriceTracking (Ledger.getDatum dat) of
         Nothing ->
           False
         Just (PriceTracking fiatFeed cryptoFeed upd) ->
           AssocMap.null fiatFeed
           && AssocMap.null cryptoFeed
           && upd `Ledger.member` range)

{-# INLINEABLE mkOracleMintingPolicy #-}
mkOracleMintingPolicy :: Ledger.ValidatorHash
                      -> OracleMintingParams
                      -> ()
                      -> Ledger.ScriptContext
                      -> Bool
mkOracleMintingPolicy
  oracle
  (OracleMintingParams op opPkh)
  _
  sc@Ledger.ScriptContext{scriptContextTxInfo=txInfo} =
  let narrowInterval :: Bool
      narrowInterval = case width (Ledger.txInfoValidRange txInfo) of
        Nothing -> False
        Just ms -> ms <= 10000 -- magic number of 10 seconds here
      minted :: Ledger.Value
      minted = Ledger.txInfoMint txInfo
      expected :: Ledger.Value
      expected = Value.singleton (Ledger.ownCurrencySymbol sc)
                                 (Value.TokenName "PriceTracking") 1
      correctMinting = traceIfFalse "incorrect minted amount"
                         (minted == expected)
      txSignedByOperator = traceIfFalse "not signed by oracle operator"
                             (Ledger.txSignedBy txInfo opPkh)
      priceMessageToOracle = case getScriptOutputsWithDatum @OracleDatum sc of
        [(output, dat)] ->
          checkMessageOutput op oracle (Ledger.txInfoValidRange txInfo)
                             expected output dat
        _ ->
          traceIfFalse "no unique PriceTracking carrying UTXO found" False
  in narrowInterval && correctMinting && txSignedByOperator && priceMessageToOracle

{-# INLINEABLE oracleMintingPolicy #-}
oracleMintingPolicy ::
  Ledger.ValidatorHash ->
  OracleMintingParams ->
  Ledger.MintingPolicy
oracleMintingPolicy oracle params =
  Ledger.mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \o p -> Scripts.wrapMintingPolicy
                                      (mkOracleMintingPolicy o p)||])
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
  OracleDatum ->
  () ->
  Ledger.ScriptContext ->
  Bool
mkOracleValidator
  (OracleValidatorParams curSymbol op opPkh _)
  _
  _
  sc@Ledger.ScriptContext{scriptContextTxInfo=txInfo} =
  let narrowInterval :: Bool
      narrowInterval = case width (Ledger.txInfoValidRange txInfo) of
        Nothing -> False
        Just ms -> ms <= 10000 -- magic number of 10 seconds here
      expectedOutVal :: Ledger.Value
      expectedOutVal = Value.singleton curSymbol
                                       (Value.TokenName "PriceTracking") 1
      txSignedByOperator :: Bool
      txSignedByOperator = Ledger.txSignedBy txInfo opPkh
      priceMessageToOracle = case getScriptOutputsWithDatum @OracleDatum sc of
        [(output, dat)] ->
          checkMessageOutput op (Ledger.ownHash sc)
            (Ledger.txInfoValidRange txInfo) expectedOutVal output dat
        _ ->
          traceIfFalse "no unique PriceTracking carrying UTXO found" False
  in narrowInterval && txSignedByOperator && priceMessageToOracle

data PriceOracling
instance Scripts.ValidatorTypes PriceOracling where
  type DatumType PriceOracling = OracleDatum 
  type RedeemerType PriceOracling = ()

{-# INLINEABLE oracleInst #-}
oracleInst :: OracleValidatorParams -> Scripts.TypedValidator PriceOracling
oracleInst params =
  Scripts.mkTypedValidator @PriceOracling
    ( $$(PlutusTx.compile [||mkOracleValidator||])
        `PlutusTx.applyCode` PlutusTx.liftCode params
    )
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.wrapValidator @OracleDatum @()

{-# INLINEABLE oracleValidator #-}
oracleValidator :: OracleValidatorParams -> Ledger.Validator
oracleValidator = Scripts.validatorScript . oracleInst

{-# INLINEABLE oracleAddress #-}
oracleAddress :: OracleValidatorParams -> Ledger.Address
oracleAddress = Ledger.scriptAddress . oracleValidator
