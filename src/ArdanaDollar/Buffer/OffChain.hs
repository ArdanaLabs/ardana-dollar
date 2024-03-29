{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module ArdanaDollar.Buffer.OffChain (
  debtAuction,
  startBuffer,
  surplusAuction,
) where

--------------------------------------------------------------------------------

import Control.Lens ((^.))
import Control.Monad (void)
import Data.Kind (Type)
import Data.Map qualified as Map
import Data.Row (Row)
import Data.Semigroup (Semigroup (..))
import Text.Printf (printf)
import Prelude (String, mconcat, show)

--------------------------------------------------------------------------------

import Ledger qualified
import Ledger.Constraints qualified as Constraints
import Ledger.Contexts qualified as Contexts
import Ledger.Typed.Scripts qualified as Scripts
import Ledger.Value qualified as Value
import Plutus.Contract
import PlutusTx qualified
import PlutusTx.Data.Extra (toDatum, toRedeemer)
import PlutusTx.Prelude hiding (Semigroup (..), mconcat)

--------------------------------------------------------------------------------

import ArdanaDollar.Buffer.OnChain
import ArdanaDollar.Buffer.Types
import ArdanaDollar.Treasury.OffChain (
  findTreasury,
  treasuryAddress,
  treasuryValidator,
 )
import ArdanaDollar.Treasury.Types (
  Treasuring,
  Treasury,
  TreasuryAction (..),
  TreasuryDatum,
  danaAssetClass,
 )
import ArdanaDollar.Utils (datumForOffchain, safeDivMod)
import ArdanaDollar.Vault (dUSDAsset)

data BufferAuctioning
instance Scripts.ValidatorTypes BufferAuctioning where
  type DatumType BufferAuctioning = BufferDatum
  type RedeemerType BufferAuctioning = BufferAction

bufferInst :: Treasury -> Value.AssetClass -> Scripts.TypedValidator BufferAuctioning
bufferInst t dac =
  Scripts.mkTypedValidator @BufferAuctioning
    ( $$(PlutusTx.compile [||mkBufferValidator||])
        `PlutusTx.applyCode` PlutusTx.liftCode t
        `PlutusTx.applyCode` PlutusTx.liftCode (treasuryAddress t)
        `PlutusTx.applyCode` PlutusTx.liftCode dac
        `PlutusTx.applyCode` PlutusTx.liftCode dUSDAsset
    )
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.wrapValidator @BufferDatum @BufferAction

bufferValidator :: Treasury -> Value.AssetClass -> Ledger.Validator
bufferValidator t dac = Scripts.validatorScript $ bufferInst t dac

bufferAddress :: Treasury -> Value.AssetClass -> Ledger.Address
bufferAddress t dac = Ledger.scriptAddress $ bufferValidator t dac

startBuffer :: (AsContractError e) => Treasury -> (Integer, Integer) -> Contract w EmptySchema e ()
startBuffer treasury (initialDebt, initialSurplus) = do
  -- TODO: prices should be computed
  let bd =
        BufferDatum
          { currentDebtAuctionPrice = initialDebt
          , currentSurplusAuctionPrice = initialSurplus
          }
      tx = Constraints.mustPayToTheScript bd mempty
  ledgerTx <- submitTxConstraints (bufferInst treasury danaAssetClass) tx
  void $ awaitTxConfirmed $ Ledger.getCardanoTxId ledgerTx

-- Auction usage contract
data BufferTreasuryAuctionArgs = BufferTreasuryAuctionArgs
  { treasuryDatum :: TreasuryDatum
  , bufferDatum :: BufferDatum
  , treasuryOutput :: Ledger.ChainIndexTxOut
  , bufferOutput :: Ledger.ChainIndexTxOut
  , txLookups :: Constraints.ScriptLookups BufferAuctioning
  , txConstraintsCtr ::
      TreasuryDatum ->
      Value.Value ->
      BufferDatum ->
      Value.Value ->
      BufferAction ->
      Constraints.TxConstraints BufferAction BufferDatum
  }

debtAuction ::
  forall (w :: Type) (s :: Row Type) (e :: Type).
  (AsContractError e) =>
  Treasury ->
  Integer ->
  Contract w s e ()
debtAuction treasury danaAmount = do
  pkh <- ownPubKeyHash
  logInfo @String $ printf "User %s has called debtAuction" (show pkh)
  maybeArgs <- bufferTreasuryAuction treasury
  flip (maybe $ return ()) maybeArgs $ \args -> do
    let dusdPrice = currentDebtAuctionPrice (bufferDatum args) * danaAmount
        danaValue = Value.assetClassValue danaAssetClass danaAmount
        treasuryValue =
          (treasuryOutput args ^. Ledger.ciTxOutValue)
            <> Value.assetClassValue dUSDAsset dusdPrice
            <> negate danaValue
        bufferValue = bufferOutput args ^. Ledger.ciTxOutValue
        td = treasuryDatum args
        bd = bufferDatum args

        tx =
          txConstraintsCtr args td treasuryValue bd bufferValue (MkDebtBid danaAmount)
            <> Constraints.mustPayToPubKey pkh danaValue
    ledgerTx <- submitTxConstraintsWith (txLookups args) tx
    awaitTxConfirmed $ Ledger.getCardanoTxId ledgerTx
    logInfo @String $
      printf "User %s has paid %s dUSD for %s DANA" (show pkh) (show dusdPrice) (show danaAmount)

surplusAuction ::
  forall (w :: Type) (s :: Row Type) (e :: Type).
  (AsContractError e) =>
  Treasury ->
  Integer ->
  Contract w s e ()
surplusAuction treasury dusdAmount = do
  pkh <- ownPubKeyHash
  logInfo @String $ printf "User %s has called surplusAuction " (show pkh)
  maybeArgs <- bufferTreasuryAuction treasury
  flip (maybe $ return ()) maybeArgs $ \args -> do
    let td = treasuryDatum args
        bd = bufferDatum args
    case dusdAmount `safeDivMod` currentSurplusAuctionPrice bd of -- [DANA], surplus is dUSD or DANA?
      Nothing -> return ()
      Just (danaPrice, danaRem) ->
        let dusdValue = Value.assetClassValue dUSDAsset dusdAmount
            treasuryValue =
              (treasuryOutput args ^. Ledger.ciTxOutValue)
                <> Value.assetClassValue danaAssetClass danaPrice
                <> negate dusdValue
            bufferValue = bufferOutput args ^. Ledger.ciTxOutValue

            tx =
              txConstraintsCtr args td treasuryValue bd bufferValue (MkSurplusBid dusdAmount)
                <> Constraints.mustPayToPubKey pkh dusdValue
         in if danaRem /= 0
              then
                logError @String $
                  printf "Cannot buy %s dUSD, it will require paying non-integer amount of DANA" (show dusdAmount)
              else do
                ledgerTx <- submitTxConstraintsWith (txLookups args) tx
                awaitTxConfirmed $ Ledger.getCardanoTxId ledgerTx
                logInfo @String $
                  printf "User %s has paid %s DANA for %s dUSD" (show pkh) (show danaPrice) (show dusdAmount)

bufferTreasuryAuction ::
  (AsContractError e) =>
  Treasury ->
  Contract w s e (Maybe BufferTreasuryAuctionArgs)
bufferTreasuryAuction treasury = do
  treasuryUtxo <- findTreasury treasury
  bufferUtxo <- findBuffer treasury
  case (treasuryUtxo, bufferUtxo) of
    (Nothing, _) -> logError @String "no treasury utxo at the script address" >> return Nothing
    (_, Nothing) -> logError @String "no buffer utxo at the script address" >> return Nothing
    (Just (toref, to, td), Just (boref, bo, bd)) -> do
      let treasuryHash = Ledger.validatorHash $ treasuryValidator treasury
          bufferHash = Ledger.validatorHash $ bufferValidator treasury danaAssetClass

          lookups =
            mconcat
              [ Constraints.typedValidatorLookups (bufferInst treasury danaAssetClass)
              , Constraints.otherScript (bufferValidator treasury danaAssetClass)
              , Constraints.otherScript (treasuryValidator treasury)
              , Constraints.unspentOutputs (Map.fromList [(toref, to), (boref, bo)])
              ]
          txCtr = \td' tv bd' bv br ->
            mconcat
              [ Constraints.mustSpendScriptOutput toref (toRedeemer @Treasuring BorrowForAuction)
              , Constraints.mustSpendScriptOutput boref (toRedeemer @BufferAuctioning br)
              , Constraints.mustPayToOtherScript treasuryHash (toDatum @Treasuring td') tv
              , Constraints.mustPayToOtherScript bufferHash (toDatum @BufferAuctioning bd') bv
              ]
      return . Just $
        BufferTreasuryAuctionArgs
          { treasuryDatum = td
          , bufferDatum = bd
          , treasuryOutput = to
          , bufferOutput = bo
          , txLookups = lookups
          , txConstraintsCtr = txCtr
          }

findBuffer ::
  (AsContractError e) =>
  Treasury ->
  Contract w s e (Maybe (Contexts.TxOutRef, Ledger.ChainIndexTxOut, BufferDatum))
findBuffer treasury = do
  utxos <- utxosAt (bufferAddress treasury danaAssetClass)
  case Map.toList utxos of
    [(oref, o)] -> do
      datum <- datumForOffchain o
      return $ (,,) oref o <$> datum
    _ -> return Nothing
