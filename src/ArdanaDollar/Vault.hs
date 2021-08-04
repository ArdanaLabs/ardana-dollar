{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveAnyClass #-}
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
import Data.Row (Row)
import PlutusTx.Prelude
import qualified Prelude as Haskell
import Data.Text (Text, pack)
import Text.Printf (printf)
import Control.Monad (forever, guard, void)
import Data.Monoid (First(First, getFirst), Last(Last))
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)
import qualified Data.Aeson as JSON
import qualified PlutusTx
import qualified PlutusTx.AssocMap as AssocMap
import qualified PlutusTx.Ratio as R
import qualified Ledger.Value as Value
import Ledger.Ada as Ada
import qualified Ledger
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
  deriving stock (Haskell.Show, Generic)
  deriving anyclass (JSON.FromJSON, JSON.ToJSON)
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
vaultTokenName :: Ledger.PubKeyHash -> Value.TokenName
vaultTokenName pkh = Value.TokenName (Ledger.getPubKeyHash pkh)

{-# INLINABLE valueWithin #-}
valueWithin :: Ledger.TxInInfo -> Ledger.Value
valueWithin = Ledger.txOutValue . Ledger.txInInfoResolved

{-# INLINABLE isUnity #-}
isUnity :: Ledger.Value -> Value.AssetClass -> Bool
isUnity v a = Value.assetClassValueOf v a == 1

{-# INLINABLE inputHasToken #-}
inputHasToken :: Value.AssetClass -> Ledger.ScriptContext -> Bool
inputHasToken tok sc = case Ledger.findOwnInput sc of
  Nothing -> False
  Just input -> isUnity (valueWithin input) tok

{-# INLINABLE ownOutput #-}
ownOutput :: Ledger.ScriptContext -> Maybe Ledger.TxOut
ownOutput sc = case Ledger.getContinuingOutputs sc of
  [output] -> Just output
  _ -> Nothing

{-# INLINABLE outputHasToken #-}
outputHasToken :: Value.AssetClass -> Ledger.ScriptContext -> Bool
outputHasToken tok sc = case ownOutput sc of
  Just output -> isUnity (Ledger.txOutValue output) tok
  Nothing -> traceIfFalse "expected exactly one output with script's datum" False

{-# INLINABLE nextDatum #-}
nextDatum :: forall (a :: Type). PlutusTx.IsData a => Value.AssetClass -> Ledger.ScriptContext -> Maybe a
nextDatum tok sc = do
  output <- ownOutput sc
  guard (Value.assetClassValueOf (Ledger.txOutValue output) tok == 1)
  dh <- Ledger.txOutDatumHash output
  Ledger.Datum datum <- AssocMap.lookup dh $ AssocMap.fromList $
                          Ledger.txInfoData (Ledger.scriptContextTxInfo sc)
  PlutusTx.fromBuiltinData datum

-- a stub before we will figure out usage of vaults' state tokens
{-# INLINABLE nextDatum' #-}
nextDatum' :: forall (a :: Type). PlutusTx.IsData a => Ledger.ScriptContext -> Maybe a
nextDatum' sc = do
  output <- ownOutput sc
  dh <- Ledger.txOutDatumHash output
  Ledger.Datum datum <- AssocMap.lookup dh $ AssocMap.fromList $
                          Ledger.txInfoData (Ledger.scriptContextTxInfo sc)
  PlutusTx.fromBuiltinData datum

{-# INLINABLE mkDUSDMintingPolicy #-}
mkDUSDMintingPolicy :: Value.TokenName -> () -> Ledger.ScriptContext -> Bool
mkDUSDMintingPolicy dusdToken _ Ledger.ScriptContext{scriptContextTxInfo=txInfo} =
  (map (\(_, tokenName, _) -> tokenName) (Value.flattenValue (Ledger.txInfoForge txInfo))) == [dusdToken]

{-# INLINABLE dUSDMintingPolicy #-}
dUSDMintingPolicy :: Ledger.MintingPolicy
dUSDMintingPolicy = Ledger.mkMintingPolicyScript $
  $$(PlutusTx.compile [|| \t -> Scripts.wrapMintingPolicy (mkDUSDMintingPolicy t) ||])
    `PlutusTx.applyCode` PlutusTx.liftCode dUSDTokenName

{-# INLINABLE dUSDCurrency #-}
dUSDCurrency :: Value.CurrencySymbol
dUSDCurrency = Ledger.scriptCurrencySymbol dUSDMintingPolicy

{-# INLINABLE dUSDAsset #-}
dUSDAsset :: Value.AssetClass
dUSDAsset = Value.AssetClass (dUSDCurrency, dUSDTokenName)

{-# INLINABLE mkVaultValidator #-}
mkVaultValidator :: Value.AssetClass
                 -> Ledger.PubKeyHash
                 -> VaultDatum
                 -> VaultRedeemer
                 -> Ledger.ScriptContext
                 -> Bool
mkVaultValidator dusd user vd vr sc@Ledger.ScriptContext{scriptContextTxInfo=txInfo} =
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
          in traceIfFalse "user signature missing" (Ledger.txSignedBy txInfo user) &&
          traceIfFalse "CollateralRedeemer transaction modifies debt" (vaultDebt vd == vaultDebt nd) &&
          case compare collDiff 0 of
            -- withdrawCollateral, pay to the user
            LT -> traceIfFalse "wrong withdrawal amount"
              ((Value.assetClassValueOf (Ledger.valuePaidTo txInfo user) collAsset) == (negate collDiff)) &&
              traceIfFalse "cannot withdraw this much" cRatioOk
            EQ -> traceIfFalse "CollateralRedeemer transaction does not modify collateral" False
            -- depositCollateral, pay to the script
            GT -> traceIfFalse "wrong deposit amount" $
              Value.assetClassValueOf (Ledger.valueLockedBy txInfo (Ledger.ownHash sc)) collAsset == collDiff
        DebtRedeemer ->
          let debtDiff = vaultDebt nd - vaultDebt vd
          in traceIfFalse "wrong dUSD mint amount" (Ledger.txInfoForge txInfo == Value.assetClassValue dusd debtDiff) &&
            traceIfFalse "cannot borrow this much" cRatioOk

data Vaulting
instance Scripts.ValidatorTypes Vaulting where
  type instance DatumType Vaulting = VaultDatum
  type instance RedeemerType Vaulting = VaultRedeemer

vaultInst :: Ledger.PubKeyHash -> Scripts.TypedValidator Vaulting
vaultInst user = Scripts.mkTypedValidator @Vaulting
    ($$(PlutusTx.compile [|| mkVaultValidator ||])
      `PlutusTx.applyCode` PlutusTx.liftCode dUSDAsset
      `PlutusTx.applyCode` PlutusTx.liftCode user)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @VaultDatum @VaultRedeemer

vaultValidator :: Ledger.PubKeyHash -> Ledger.Validator
vaultValidator = Scripts.validatorScript . vaultInst

vaultAddress :: Ledger.PubKeyHash -> Ledger.Address
vaultAddress = Ledger.scriptAddress . vaultValidator

type VaultSchema =
  Endpoint "initializeVault" ()
    .\/ Endpoint "depositCollateral" Integer
    .\/ Endpoint "withdrawCollateral" Integer
    .\/ Endpoint "mintDUSD" Integer
    .\/ Endpoint "repayDUSD" Integer

getDatumOffChain :: forall (a :: Type). PlutusTx.IsData a => Ledger.TxOutTx -> Maybe a
getDatumOffChain outTx = do
  dh <- Ledger.txOutDatumHash $ Ledger.txOutTxOut outTx
  Ledger.Datum datum <- Map.lookup dh $ Ledger.txData $ Ledger.txOutTxTx outTx
  PlutusTx.fromBuiltinData datum

findUtxo :: forall (w :: Type) (s :: Row Type).
            Ledger.Address
         -> Contract w s Text (Maybe (Ledger.TxOutRef, Ledger.TxOutTx, VaultDatum))
findUtxo addr = do
  utxos <- utxoAt addr
  -- pick the first utxo with a `VaultDatum`
  let ud = getFirst $
        foldMap (\(ref, tx) -> First $ fmap ((,,) ref tx) (getDatumOffChain tx)) $
        Map.toAscList utxos
  return ud

initializeVault :: forall (s :: Row Type). Contract (Last VaultDatum) s Text ()
initializeVault = do
  pkh <- Ledger.pubKeyHash <$> ownPubKey
  let tok = vaultTokenName pkh
  cs  <- fmap Currency.currencySymbol $
         mapError (pack . Haskell.show @Currency.CurrencyError) $
         Currency.mintContract pkh [(tok, 1)]
  let initialDatum = VaultDatum 0 0
      inst = vaultInst pkh
      lookups = Constraints.typedValidatorLookups inst
      tx = Constraints.mustPayToTheScript
             initialDatum
             (Value.assetClassValue (Value.AssetClass (cs, tok)) 1)
  ledgerTx <- submitTxConstraintsWith lookups tx
  void $ awaitTxConfirmed $ Ledger.txId ledgerTx
  logInfo @Haskell.String $ printf "started vault for user %s at address %s"
                              (Haskell.show pkh) (Haskell.show $ vaultAddress pkh)
  tell $ Last $ Just initialDatum

depositCollateral :: forall (s :: Row Type). Integer -> Contract (Last VaultDatum) s Text ()
depositCollateral amount = do
  pkh <- Ledger.pubKeyHash <$> ownPubKey
  let addr = vaultAddress pkh
  utxoWithDatum <- findUtxo addr
  case utxoWithDatum of
    Nothing -> throwError "no utxo at the script address"
    Just (oref, o, vaultDatum@VaultDatum{ vaultCollateral = vc }) -> do
      let newDatum = vaultDatum { vaultCollateral = vc + amount }
          valueToScript = Ledger.txOutValue (Ledger.txOutTxOut o) Haskell.<>
                          Value.assetClassValue collAsset amount
          lookups = Constraints.typedValidatorLookups (vaultInst pkh) Haskell.<>
                    Constraints.otherScript (vaultValidator pkh) Haskell.<>
                    Constraints.unspentOutputs (Map.singleton oref o) Haskell.<>
                    Constraints.ownPubKeyHash pkh
          tx = Constraints.mustPayToTheScript newDatum valueToScript Haskell.<>
               Constraints.mustSpendScriptOutput oref
                 (Ledger.Redeemer $ PlutusTx.toBuiltinData CollateralRedeemer)
      ledgerTx <- submitTxConstraintsWith lookups tx
      void $ awaitTxConfirmed $ Ledger.txId ledgerTx
      logInfo @Haskell.String $ printf "user %s deposited %s collateral"
                                  (Haskell.show pkh) (Haskell.show amount)
      tell $ Last $ Just newDatum

withdrawCollateral :: forall (s :: Row Type). Integer -> Contract (Last VaultDatum) s Text ()
withdrawCollateral amount = do
  pkh <- Ledger.pubKeyHash <$> ownPubKey
  let addr = vaultAddress pkh
  utxoWithDatum <- findUtxo addr
  case utxoWithDatum of
    Nothing -> throwError "no utxo at the script address"
    Just (oref, o, vaultDatum@VaultDatum{ vaultCollateral = vc }) -> do
      let newDatum = vaultDatum { vaultCollateral = vc - amount }
          stateTokenValue = Ledger.txOutValue (Ledger.txOutTxOut o)
          collateralValue = Value.assetClassValue collAsset amount
          lookups = Constraints.typedValidatorLookups (vaultInst pkh) Haskell.<>
                    Constraints.otherScript (vaultValidator pkh) Haskell.<>
                    Constraints.unspentOutputs (Map.singleton oref o) Haskell.<>
                    Constraints.ownPubKeyHash pkh
          tx = Constraints.mustPayToTheScript newDatum stateTokenValue Haskell.<>
               Constraints.mustSpendScriptOutput oref
                 (Ledger.Redeemer $ PlutusTx.toBuiltinData CollateralRedeemer) Haskell.<>
               Constraints.mustPayToPubKey pkh collateralValue
          tx' = Constraints.mkTx @Vaulting lookups tx
      logInfo @Haskell.String $ printf "sending transaction: %s" (Haskell.show tx')
      ledgerTx <- submitTxConstraintsWith lookups tx
      void $ awaitTxConfirmed $ Ledger.txId ledgerTx
      logInfo @Haskell.String $ printf "user %s withdrew %s collateral"
                                  (Haskell.show pkh) (Haskell.show amount)
      tell $ Last $ Just newDatum

mintDUSD :: forall (s :: Row Type). Integer -> Contract (Last VaultDatum) s Text ()
mintDUSD amount = do
  pkh <- Ledger.pubKeyHash <$> ownPubKey
  let addr = vaultAddress pkh
  utxoWithDatum <- findUtxo addr
  case utxoWithDatum of
    Nothing -> throwError "no utxo at the script address"
    Just (oref, o, vaultDatum@VaultDatum{ vaultDebt = dc }) -> do
      let newDatum = vaultDatum { vaultDebt = dc + amount }
          stateTokenValue = Ledger.txOutValue (Ledger.txOutTxOut o)
          dusdValue = Value.assetClassValue dUSDAsset amount
          lookups = Constraints.typedValidatorLookups (vaultInst pkh) Haskell.<>
                    Constraints.otherScript (vaultValidator pkh) Haskell.<>
                    Constraints.unspentOutputs (Map.singleton oref o) Haskell.<>
                    Constraints.mintingPolicy dUSDMintingPolicy Haskell.<>
                    Constraints.ownPubKeyHash pkh
          tx = Constraints.mustPayToTheScript newDatum stateTokenValue Haskell.<>
               Constraints.mustSpendScriptOutput oref
                 (Ledger.Redeemer $ PlutusTx.toBuiltinData DebtRedeemer) Haskell.<>
               Constraints.mustMintValue dusdValue Haskell.<>
               Constraints.mustPayToPubKey pkh dusdValue
      ledgerTx <- submitTxConstraintsWith lookups tx
      void $ awaitTxConfirmed $ Ledger.txId ledgerTx
      logInfo @Haskell.String $ printf "user %s minted %s dUSD"
                                  (Haskell.show pkh) (Haskell.show amount)
      tell $ Last $ Just newDatum

repayDUSD :: forall (s :: Row Type). Integer -> Contract (Last VaultDatum) s Text ()
repayDUSD amount = do
  pkh <- Ledger.pubKeyHash <$> ownPubKey
  let addr = vaultAddress pkh
  utxoWithDatum <- findUtxo addr
  case utxoWithDatum of
    Nothing -> throwError "no utxo at the script address"
    Just (oref, o, vaultDatum@VaultDatum{ vaultDebt = dc }) -> do
      let newDatum = vaultDatum { vaultDebt = dc - amount }
          stateTokenValue = Ledger.txOutValue (Ledger.txOutTxOut o)
          dusdValue = Value.assetClassValue dUSDAsset amount
          lookups = Constraints.typedValidatorLookups (vaultInst pkh) Haskell.<>
                    Constraints.otherScript (vaultValidator pkh) Haskell.<>
                    Constraints.unspentOutputs (Map.singleton oref o) Haskell.<>
                    Constraints.mintingPolicy dUSDMintingPolicy Haskell.<>
                    Constraints.ownPubKeyHash pkh
          tx = Constraints.mustPayToTheScript newDatum stateTokenValue Haskell.<>
               Constraints.mustSpendScriptOutput oref
                 (Ledger.Redeemer $ PlutusTx.toBuiltinData DebtRedeemer) Haskell.<>
               Constraints.mustMintValue (negate dusdValue)
      ledgerTx <- submitTxConstraintsWith lookups tx
      void $ awaitTxConfirmed $ Ledger.txId ledgerTx
      logInfo @Haskell.String $ printf "user %s repaid %s dUSD"
                                  (Haskell.show pkh) (Haskell.show amount)
      tell $ Last $ Just newDatum

vaultContract :: Contract (Last VaultDatum) VaultSchema Text ()
vaultContract = forever $
  (endpoint @"initializeVault" >> initializeVault) `select`
  (endpoint @"depositCollateral" >>= depositCollateral) `select`
  (endpoint @"withdrawCollateral" >>= withdrawCollateral) `select`
  (endpoint @"mintDUSD" >>= mintDUSD) `select`
  (endpoint @"repayDUSD" >>= repayDUSD)
