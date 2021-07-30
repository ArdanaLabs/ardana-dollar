{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TemplateHaskell #-}

module ArdanaDollar.Vault (
  VaultDatum(vaultCollateral, vaultDebt),
  VaultRedeemer(CollateralRedeemer, DebtRedeemer),
  Vaulting,
  VaultSchema,
  collAsset,
  dUSDTokenName,
  vaultTokenName,
  valueWithin,
  isUnity,
  inputHasToken,
  ownOutput,
  outputHasToken,
  nextDatum,
  mkDUSDMintingPolicy,
  dUSDMintingPolicy,
  dUSDCurrency,
  dUSDAsset,
  mkVaultValidator,
  vaultInst,
  vaultValidator,
  vaultAddress,
  initializeVault,
  depositCollateral,
  withdrawCollateral,
  mintDUSD,
  repayDUSD,
  vaultContract
) where

import Data.Kind (Type)
import PlutusTx.Prelude
import qualified Prelude as Haskell
import Data.Text (Text, pack)
import Text.Printf (printf)
import Control.Monad (forever, guard, void)
import Data.Monoid (First(..))
import qualified Data.Map.Strict as Map
import qualified PlutusTx
import qualified PlutusTx.AssocMap as AssocMap
import qualified PlutusTx.Ratio as R
import qualified Ledger.Value as Value
import Ledger.Ada as Ada
import Ledger hiding (Finite) -- TODO
import qualified Ledger.Typed.Scripts as Scripts
import qualified Ledger.Constraints as Constraints
import Plutus.Contract -- TODO
import qualified Plutus.Contracts.Currency as Currency
import ArdanaDollar.Types (CollaterizationRatio(Finite))
import ArdanaDollar.Utils (collaterizationRatio)


data VaultDatum = VaultDatum
  { vaultCollateral :: Integer
  , vaultDebt :: Integer
  }
PlutusTx.makeIsDataIndexed ''VaultDatum [('VaultDatum, 0)]

data VaultRedeemer
  = CollateralRedeemer
  | DebtRedeemer
  deriving stock Haskell.Show
PlutusTx.makeIsDataIndexed ''VaultRedeemer [ ('CollateralRedeemer, 0)
                                           , ('DebtRedeemer, 1)
                                           ]
collAsset :: Value.AssetClass
collAsset = Value.AssetClass (Ada.adaSymbol, Ada.adaToken)

{-# INLINABLE liqRatio #-}
liqRatio :: CollaterizationRatio
liqRatio = Finite (150 R.% 100)

{-# INLINABLE price #-}
price :: Rational
price = 140 R.% 1_000_000
      
{-# INLINABLE dUSDTokenName #-}
dUSDTokenName :: Value.TokenName
dUSDTokenName = Value.TokenName "dUSD"

{-# INLINABLE vaultTokenName #-}
vaultTokenName :: PubKeyHash -> Value.TokenName
vaultTokenName pkh = Value.TokenName (getPubKeyHash pkh)

{-# INLINABLE valueWithin #-}
valueWithin :: TxInInfo -> Value
valueWithin = txOutValue . txInInfoResolved

{-# INLINABLE isUnity #-}
isUnity :: Value -> Value.AssetClass -> Bool
isUnity v a = Value.assetClassValueOf v a == 1

{-# INLINABLE inputHasToken #-}
inputHasToken :: Value.AssetClass -> ScriptContext -> Bool
inputHasToken tok sc = case findOwnInput sc of
  Nothing -> False
  Just input -> isUnity (valueWithin input) tok

{-# INLINABLE ownOutput #-}
ownOutput :: ScriptContext -> Maybe TxOut
ownOutput sc = case [ o
                    | o <- getContinuingOutputs sc
                    , txOutDatumHash o == Just (snd $ ownHashes sc)
                    ] of
  [output] -> Just output
  _ -> Nothing

{-# INLINABLE outputHasToken #-}
outputHasToken :: Value.AssetClass -> ScriptContext -> Bool
outputHasToken tok sc = case ownOutput sc of
  Just output -> isUnity (txOutValue output) tok
  Nothing -> traceIfFalse "expected exactly one output with script's datum" False

{-# INLINABLE nextDatum #-}
nextDatum :: PlutusTx.IsData a => Value.AssetClass -> ScriptContext -> Maybe a
nextDatum tok sc = do
  output <- ownOutput sc
  guard (Value.assetClassValueOf (txOutValue output) tok == 1)
  dh <- txOutDatumHash output
  Datum datum <- AssocMap.lookup dh $ AssocMap.fromList $ txInfoData (scriptContextTxInfo sc)
  PlutusTx.fromBuiltinData datum

-- a stub before we will figure out usage of vaults' state tokens
{-# INLINABLE nextDatum' #-}
nextDatum' :: PlutusTx.IsData a => ScriptContext -> Maybe a
nextDatum' sc = do
  Datum datum <- case map snd (txInfoData (scriptContextTxInfo sc)) of
    [] -> Nothing
    (d : _) -> Just d
  PlutusTx.fromBuiltinData datum

{-# INLINABLE mkDUSDMintingPolicy #-}
mkDUSDMintingPolicy :: Value.TokenName -> () -> ScriptContext -> Bool
mkDUSDMintingPolicy dusdToken _ ScriptContext{scriptContextTxInfo=txInfo} =
  (map (\(_, tokenName, _) -> tokenName) (Value.flattenValue (txInfoForge txInfo))) == [dusdToken]

{-# INLINABLE dUSDMintingPolicy #-}
dUSDMintingPolicy :: MintingPolicy
dUSDMintingPolicy = Ledger.mkMintingPolicyScript $
  $$(PlutusTx.compile [|| \t -> Scripts.wrapMintingPolicy (mkDUSDMintingPolicy t) ||])
    `PlutusTx.applyCode` PlutusTx.liftCode dUSDTokenName

{-# INLINABLE dUSDCurrency #-}
dUSDCurrency :: Value.CurrencySymbol
dUSDCurrency = scriptCurrencySymbol dUSDMintingPolicy

{-# INLINABLE dUSDAsset #-}
dUSDAsset :: Value.AssetClass
dUSDAsset = Value.AssetClass (dUSDCurrency, dUSDTokenName)

{-# INLINABLE mkVaultValidator #-}
mkVaultValidator :: Value.AssetClass -> PubKeyHash -> VaultDatum -> VaultRedeemer -> ScriptContext -> Bool
mkVaultValidator dusd user vd vr sc@ScriptContext{scriptContextTxInfo=txInfo} =
  let outm = ownOutput sc
      ndm = nextDatum' sc
  in case (outm, ndm) of
    (Nothing, _) -> traceIfFalse "output with script's datum missing" False
    (Just _, Nothing) -> traceIfFalse "failed to parse datum" False
    (Just _, Just nd) ->
      let cRatioOk = collaterizationRatio price (vaultCollateral nd) (vaultDebt nd) >= liqRatio
      in case vr of
        CollateralRedeemer ->
          let collDiff = vaultCollateral nd - vaultCollateral vd
          in traceIfFalse "user signature missing" (txSignedBy txInfo user) &&
          traceIfFalse "CollateralRedeemer transaction modifies debt" (vaultDebt vd == vaultDebt nd) &&
          case compare collDiff 0 of
            -- withdrawCollateral, pay to the user
            LT -> traceIfFalse "wrong withdrawal amount"
              (Value.assetClassValueOf (valuePaidTo txInfo user) collAsset == (negate collDiff)) &&
              traceIfFalse "cannot withdraw this much" cRatioOk
            EQ -> traceIfFalse "CollateralRedeemer transaction does not modify collateral" False
            -- depositCollateral, pay to the script
            GT -> traceIfFalse "wrong deposit amount" $
              Value.assetClassValueOf (valueLockedBy txInfo (ownHash sc)) collAsset == collDiff
        DebtRedeemer ->
          let debtDiff = vaultDebt nd - vaultDebt vd
          in traceIfFalse "wrong dUSD mint amount" (txInfoForge txInfo == Value.assetClassValue dusd debtDiff) &&
            traceIfFalse "cannot borrow this much" cRatioOk

data Vaulting
instance Scripts.ValidatorTypes Vaulting where
  type instance DatumType Vaulting = VaultDatum
  type instance RedeemerType Vaulting = VaultRedeemer

vaultInst :: PubKeyHash -> Scripts.TypedValidator Vaulting
vaultInst user = Scripts.mkTypedValidator @Vaulting
    ($$(PlutusTx.compile [|| mkVaultValidator ||])
      `PlutusTx.applyCode` PlutusTx.liftCode dUSDAsset
      `PlutusTx.applyCode` PlutusTx.liftCode user)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @VaultDatum @VaultRedeemer

vaultValidator :: PubKeyHash -> Validator
vaultValidator = Scripts.validatorScript . vaultInst

vaultAddress :: PubKeyHash -> Ledger.Address
vaultAddress = scriptAddress . vaultValidator

type VaultSchema =
  Endpoint "initializeVault" ()
    .\/ Endpoint "depositCollateral" Integer
    .\/ Endpoint "withdrawCollateral" Integer
    .\/ Endpoint "mintDUSD" Integer
    .\/ Endpoint "repayDUSD" Integer

getDatumOffChain :: forall (a :: Type). PlutusTx.IsData a => TxOutTx -> Maybe a
getDatumOffChain outTx = do
  dh <- txOutDatumHash $ txOutTxOut outTx
  Datum datum <- Map.lookup dh $ txData $ txOutTxTx outTx
  PlutusTx.fromBuiltinData datum

findUtxo :: Ledger.Address -> Contract w s Text (Maybe (TxOutRef, TxOutTx, VaultDatum))
findUtxo addr = do
  utxos <- utxoAt addr
  -- pick the first utxo with a `VaultDatum`
  let ud = getFirst $
        foldMap (\(ref, tx) -> First $ fmap ((,,) ref tx) (getDatumOffChain tx)) $
        Map.toAscList utxos
  return ud

initializeVault :: Contract w s Text ()
initializeVault = do
  pkh <- pubKeyHash <$> ownPubKey
  let tok = vaultTokenName pkh
  cs  <- fmap Currency.currencySymbol $
         mapError (pack . Haskell.show @Currency.CurrencyError) $
         Currency.mintContract pkh [(tok, 1)]
  let initialDatum = VaultDatum 0 0
      inst = vaultInst pkh
      tx = Constraints.mustPayToTheScript
             initialDatum
             (Value.assetClassValue (Value.AssetClass (cs, tok)) 1)
  ledgerTx <- submitTxConstraints inst tx
  void $ awaitTxConfirmed $ txId ledgerTx
  logInfo @Haskell.String $ printf "started vault for user %s at address %s"
                              (Haskell.show pkh) (Haskell.show $ vaultAddress pkh)

depositCollateral :: Integer -> Contract w s Text ()
depositCollateral amount = do
  pkh <- pubKeyHash <$> ownPubKey
  let addr = vaultAddress pkh
  utxoWithDatum <- findUtxo addr
  case utxoWithDatum of
    Nothing -> throwError "no utxo at the script address"
    Just (oref, o, vaultDatum@VaultDatum{ vaultCollateral = vc }) -> do
      let newDatum = vaultDatum { vaultCollateral = vc + amount }
          valueToScript = txOutValue (txOutTxOut o) Haskell.<> Value.assetClassValue collAsset amount
          lookups = Constraints.typedValidatorLookups (vaultInst pkh) Haskell.<>
                    Constraints.otherScript (vaultValidator pkh) Haskell.<>
                    Constraints.unspentOutputs (Map.singleton oref o) Haskell.<>
                    Constraints.ownPubKeyHash pkh
          tx = Constraints.mustPayToTheScript newDatum valueToScript Haskell.<>
               Constraints.mustSpendScriptOutput oref
                 (Redeemer $ PlutusTx.toBuiltinData CollateralRedeemer)
      ledgerTx <- submitTxConstraintsWith lookups tx
      void $ awaitTxConfirmed $ txId ledgerTx
      logInfo @Haskell.String $ printf "user %s deposited %s collateral"
                                  (Haskell.show pkh) (Haskell.show amount)

withdrawCollateral :: Integer -> Contract w s Text ()
withdrawCollateral amount = do
  pkh <- pubKeyHash <$> ownPubKey
  let addr = vaultAddress pkh
  utxoWithDatum <- findUtxo addr
  case utxoWithDatum of
    Nothing -> throwError "no utxo at the script address"
    Just (oref, o, vaultDatum@VaultDatum{ vaultCollateral = vc }) -> do
      let newDatum = vaultDatum { vaultCollateral = vc - amount }
          stateTokenValue = txOutValue (txOutTxOut o)
          collateralValue = Value.assetClassValue collAsset amount
          lookups = Constraints.typedValidatorLookups (vaultInst pkh) Haskell.<>
                    Constraints.otherScript (vaultValidator pkh) Haskell.<>
                    Constraints.unspentOutputs (Map.singleton oref o) Haskell.<>
                    Constraints.ownPubKeyHash pkh
          tx = Constraints.mustPayToTheScript newDatum stateTokenValue Haskell.<>
               Constraints.mustSpendScriptOutput oref
                 (Redeemer $ PlutusTx.toBuiltinData CollateralRedeemer) Haskell.<>
               Constraints.mustPayToPubKey pkh collateralValue
      ledgerTx <- submitTxConstraintsWith lookups tx
      void $ awaitTxConfirmed $ txId ledgerTx
      logInfo @Haskell.String $ printf "user %s withdrew %s collateral"
                                  (Haskell.show pkh) (Haskell.show amount)

mintDUSD :: Integer -> Contract w s Text ()
mintDUSD amount = do
  pkh <- pubKeyHash <$> ownPubKey
  let addr = vaultAddress pkh
  utxoWithDatum <- findUtxo addr
  case utxoWithDatum of
    Nothing -> throwError "no utxo at the script address"
    Just (oref, o, vaultDatum@VaultDatum{ vaultDebt = dc }) -> do
      let newDatum = vaultDatum { vaultDebt = dc + amount }
          stateTokenValue = txOutValue (txOutTxOut o)
          dusdValue = Value.assetClassValue dUSDAsset amount
          lookups = Constraints.typedValidatorLookups (vaultInst pkh) Haskell.<>
                    Constraints.otherScript (vaultValidator pkh) Haskell.<>
                    Constraints.unspentOutputs (Map.singleton oref o) Haskell.<>
                    Constraints.mintingPolicy dUSDMintingPolicy Haskell.<>
                    Constraints.ownPubKeyHash pkh
          tx = Constraints.mustPayToTheScript newDatum stateTokenValue Haskell.<>
               Constraints.mustSpendScriptOutput oref
                 (Redeemer $ PlutusTx.toBuiltinData CollateralRedeemer) Haskell.<>
               Constraints.mustMintValue dusdValue Haskell.<>
               Constraints.mustPayToPubKey pkh dusdValue
      ledgerTx <- submitTxConstraintsWith lookups tx
      void $ awaitTxConfirmed $ txId ledgerTx
      logInfo @Haskell.String $ printf "user %s minted %s dUSD"
                                  (Haskell.show pkh) (Haskell.show amount)

repayDUSD :: Integer -> Contract w s Text ()
repayDUSD amount = do
  pkh <- pubKeyHash <$> ownPubKey
  let addr = vaultAddress pkh
  utxoWithDatum <- findUtxo addr
  case utxoWithDatum of
    Nothing -> throwError "no utxo at the script address"
    Just (oref, o, vaultDatum@VaultDatum{ vaultDebt = dc }) -> do
      let newDatum = vaultDatum { vaultDebt = dc - amount }
          stateTokenValue = txOutValue (txOutTxOut o)
          dusdValue = Value.assetClassValue dUSDAsset amount
          lookups = Constraints.typedValidatorLookups (vaultInst pkh) Haskell.<>
                    Constraints.otherScript (vaultValidator pkh) Haskell.<>
                    Constraints.unspentOutputs (Map.singleton oref o) Haskell.<>
                    Constraints.mintingPolicy dUSDMintingPolicy Haskell.<>
                    Constraints.ownPubKeyHash pkh
          tx = Constraints.mustPayToTheScript newDatum stateTokenValue Haskell.<>
               Constraints.mustSpendScriptOutput oref
                 (Redeemer $ PlutusTx.toBuiltinData CollateralRedeemer) Haskell.<>
               Constraints.mustMintValue (negate dusdValue) Haskell.<>
               Constraints.mustPayToPubKey pkh dusdValue
      ledgerTx <- submitTxConstraintsWith lookups tx
      void $ awaitTxConfirmed $ txId ledgerTx
      logInfo @Haskell.String $ printf "user %s repaid %s dUSD"
                                  (Haskell.show pkh) (Haskell.show amount)

vaultContract :: Contract w VaultSchema Text ()
vaultContract = forever $
  (endpoint @"initializeVault" >> initializeVault) `select`
  (endpoint @"depositCollateral" >>= depositCollateral) `select`
  (endpoint @"withdrawCollateral" >>= withdrawCollateral) `select`
  (endpoint @"mintDUSD" >>= mintDUSD) `select`
  (endpoint @"repayDUSD" >>= repayDUSD)
