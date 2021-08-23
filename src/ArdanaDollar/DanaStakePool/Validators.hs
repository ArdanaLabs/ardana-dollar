{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module ArdanaDollar.DanaStakePool.Validators (
  ValidatorTypes,
  spInst,
  spValidator,
  spAddress,
  Datum (..),
  UserData (..),
  GlobalData (..),
  Redeemer (..),
  Balance (..),
  userInitProofAssetClass,
  userInitProofPolicy,
  addTotalStake,
  rewardHelper,
) where

import PlutusTx.Prelude

import Ledger qualified
import Ledger.Typed.Scripts qualified as Scripts
import Ledger.Value qualified as Value
import PlutusTx qualified
import PlutusTx.Monoid qualified
import PlutusTx.Ratio qualified as R
import PlutusTx.Semigroup qualified

import Prelude qualified as Haskell

import ArdanaDollar.DanaStakePool.DanaCurrency qualified as DanaCurrency
import ArdanaDollar.DanaStakePool.Utils (intersectionWith)
import ArdanaDollar.Utils (datumForOnchain)

import Control.Monad (join)

import GHC.Generics (Generic)

import Data.Aeson qualified as JSON

data Balance = Balance
  { dStake :: Value.Value
  , dReward :: Value.Value
  }
  deriving stock (Haskell.Show, Generic, Haskell.Eq)
  deriving anyclass (JSON.FromJSON, JSON.ToJSON)

instance PlutusTx.Semigroup.Semigroup Balance where
  (<>) x y = Balance (dStake x + dStake y) (dReward x + dReward y)

instance Haskell.Semigroup Balance where
  (<>) x y = x PlutusTx.Semigroup.<> y

instance PlutusTx.Monoid.Monoid Balance where
  mempty = Balance mempty mempty

instance Haskell.Monoid Balance where
  mempty = PlutusTx.Monoid.mempty

data UserData = UserData
  { dPkh :: Ledger.PubKeyHash
  , dBalance :: Balance
  }
  deriving stock (Haskell.Show, Generic)
  deriving anyclass (JSON.FromJSON, JSON.ToJSON)

newtype GlobalData = GlobalData
  { dTotalStake :: Value.Value
  }
  deriving stock (Haskell.Show, Generic)
  deriving anyclass (JSON.FromJSON, JSON.ToJSON)

addTotalStake :: GlobalData -> Value.Value -> GlobalData
addTotalStake (GlobalData s) v = GlobalData (s <> v)

data Datum = UserDatum UserData | GlobalDatum GlobalData
  deriving stock (Haskell.Show, Generic)
  deriving anyclass (JSON.FromJSON, JSON.ToJSON)

PlutusTx.makeIsDataIndexed ''Balance [('Balance, 0)]
PlutusTx.makeIsDataIndexed ''UserData [('UserData, 0)]
PlutusTx.makeIsDataIndexed ''GlobalData [('GlobalData, 0)]
PlutusTx.makeIsDataIndexed
  ''Datum
  [ ('UserDatum, 0)
  , ('GlobalDatum, 1)
  ]

data Redeemer
  = DepositOrWithdraw
  | ProvideRewards
  | DistributeRewards
  | WithdrawRewards
  deriving stock (Haskell.Show, Generic)
  deriving anyclass (JSON.FromJSON, JSON.ToJSON)

PlutusTx.makeIsDataIndexed
  ''Redeemer
  [ ('DepositOrWithdraw, 0)
  , ('ProvideRewards, 1)
  , ('DistributeRewards, 2)
  , ('WithdrawRewards, 3)
  ]

-------------------------------------------------------------------------------

{-# INLINEABLE mkUserInitProofPolicy #-}
mkUserInitProofPolicy :: () -> Ledger.ScriptContext -> Bool
mkUserInitProofPolicy _ ctx =
  let outputs = filter (PlutusTx.Prelude.isJust . datumForOnchain @Datum info) (Ledger.txInfoOutputs info)
      unique = head outputs
   in if length outputs == 1
        then case datumForOnchain @Datum info unique of
          Just (UserDatum (UserData key balance)) ->
            balance Haskell.== mempty
              && checkMintedAmount
              && Ledger.txSignedBy info key
              && txOutValid (Value.AssetClass (Ledger.ownCurrencySymbol ctx, userInitProofTokenName)) info unique
          _ -> False
        else False
  where
    info :: Ledger.TxInfo
    info = Ledger.scriptContextTxInfo ctx

    checkMintedAmount :: Bool
    checkMintedAmount = case Value.flattenValue (Ledger.txInfoForge info) of
      [(cs, tn', amt)] -> cs == Ledger.ownCurrencySymbol ctx && tn' == Value.TokenName emptyByteString && amt == 1
      _ -> False

{-# INLINEABLE userInitProofPolicy #-}
userInitProofPolicy :: Scripts.MintingPolicy
userInitProofPolicy =
  Ledger.mkMintingPolicyScript
    $$(PlutusTx.compile [||Scripts.wrapMintingPolicy mkUserInitProofPolicy||])

{-# INLINEABLE userInitProofSymbol #-}
userInitProofSymbol :: Value.CurrencySymbol
userInitProofSymbol = Ledger.scriptCurrencySymbol userInitProofPolicy

{-# INLINEABLE userInitProofTokenName #-}
userInitProofTokenName :: Value.TokenName
userInitProofTokenName = Value.TokenName emptyByteString

{-# INLINEABLE userInitProofAssetClass #-}
userInitProofAssetClass :: Value.AssetClass
userInitProofAssetClass = Value.AssetClass (userInitProofSymbol, userInitProofTokenName)

-------------------------------------------------------------------------------

{- ORMOLU_DISABLE -}

{-# INLINEABLE mkValidator #-}
mkValidator ::
  Value.AssetClass ->
  Value.CurrencySymbol ->
  Value.CurrencySymbol ->
  Datum ->
  Redeemer ->
  Ledger.ScriptContext ->
  Bool
mkValidator danaAsset nftSymbol userInitProofSymbol_ datum redeemer ctx =
  case datum of
    GlobalDatum _ ->
        traceIfFalse "incorrect global" (isJust maybeGlobal)
     && let ((gInTxOut, gInData), (gOutTxOut, gOutData)) = justGlobal
        in case redeemer of
          DepositOrWithdraw -> traceIfFalse "tinkering with rewards"
                               (Ledger.txOutValue gInTxOut == Ledger.txOutValue gOutTxOut)
                            && traceIfFalse "no unique user"
                               (PlutusTx.Prelude.maybe False id $ (\l -> length l == 1) <$> maybeUsers)
                            && traceIfFalse "total stake incorrect"
                               ( let ((uInTxOut, _), (uOutTxOut, _)) = uniqueUser
                                 in dTotalStake gOutData - dTotalStake gInData
                                      ==
                                    Ledger.txOutValue uOutTxOut - Ledger.txOutValue uInTxOut
                               )
                            && traceIfFalse "stake not in dana"
                               (danaOnly (dTotalStake gOutData - dTotalStake gInData))

          ProvideRewards    -> traceIfFalse "total stak has changed"
                               (dTotalStake gOutData == dTotalStake gInData)
                            && traceIfFalse "stealing rewards"
                               (positive $ Ledger.txOutValue gOutTxOut - Ledger.txOutValue gInTxOut)

          DistributeRewards -> traceIfFalse "total stak has changed"
                               (dTotalStake gOutData == dTotalStake gInData)
                            && traceIfFalse "wrong reward distribution"
                               ( distributionOk
                                  (dTotalStake gOutData)
                                  (Ledger.txOutValue gInTxOut)
                                  (Ledger.txOutValue gOutTxOut)
                               )

          WithdrawRewards   -> traceIfFalse "cannot use redeemer" False

    UserDatum dat ->
        traceIfFalse "own input is not valid"
        (isJust maybeUsers) -- that could be optimized
     && case redeemer of
          DepositOrWithdraw -> traceIfFalse "signature missing"
                               (isSigned dat)
                            && traceIfFalse "global not present"
                               (isJust maybeGlobal)
          ProvideRewards ->    False
          DistributeRewards -> traceIfFalse "global not present"
                               (isJust maybeGlobal)
                            && traceIfFalse "stake not preserved"
                               ( let ((_, inData), (_, outData)) = ownUser
                                 in (dStake $ dBalance inData) == (dStake $ dBalance outData)
                               )
          WithdrawRewards ->   traceIfFalse "signature missing"
                               (isSigned dat)
                            && traceIfFalse "stake not preserved"
                               ( let ((_, inData), (_, outData)) = ownUser
                                 in (dStake $ dBalance inData) == (dStake $ dBalance outData)
                               )
  where
    info :: Ledger.TxInfo
    info = Ledger.scriptContextTxInfo ctx

    isSigned :: UserData -> Bool
    isSigned dat = dPkh dat `elem` Ledger.txInfoSignatories info

    isValid :: Ledger.TxOut -> Bool
    isValid txOut = txOutValid (Value.AssetClass (userInitProofSymbol_, userInitProofTokenName)) info txOut

    hasNFT :: Ledger.TxOut -> Bool
    hasNFT txOut =
      1
        == Value.assetClassValueOf
          (Ledger.txOutValue txOut)
          (Value.AssetClass (nftSymbol, Value.TokenName emptyByteString))

    hasUserToken :: Ledger.TxOut -> Bool
    hasUserToken txOut = Value.assetClassValueOf (Ledger.txOutValue txOut) (Value.AssetClass (userInitProofSymbol_, userInitProofTokenName)) == 1

    globalDatum :: Ledger.TxOut -> Maybe GlobalData
    globalDatum txOut = case datumForOnchain @Datum info txOut of
      Just (GlobalDatum dat) -> Just dat
      _ -> Nothing

    userDatum :: Ledger.TxOut -> Maybe UserData
    userDatum txOut = case datumForOnchain @Datum info txOut of
      Just (UserDatum dat) -> Just dat
      _ -> Nothing

    maybeGlobal :: Maybe ((Ledger.TxOut, GlobalData), (Ledger.TxOut, GlobalData))
    maybeGlobal = do
      a <- f (Ledger.getContinuingOutputs ctx)
      b <- f (Ledger.txInInfoResolved <$> Ledger.txInfoInputs info)
      return (b, a)
      where
        f txOuts = case map (\txOut -> (txOut, globalDatum txOut)) $ filter hasNFT txOuts of
          [(txOut, Just dat)] -> Just (txOut, dat)
          _ -> Nothing

    justGlobal = case maybeGlobal of
      Just o -> o
      Nothing -> PlutusTx.Prelude.traceError "global"

    maybeUsers :: Maybe [((Ledger.TxOut, UserData), (Ledger.TxOut, UserData))]
    maybeUsers =
      let iUsers = f (Ledger.txInInfoResolved <$> Ledger.txInfoInputs info)
          oUsers = f (Ledger.getContinuingOutputs ctx)
          combined = intersectionWith (,) (g iUsers) (g oUsers)
          sync = length combined == length iUsers && length combined == length oUsers
       in if unique iUsers && unique oUsers && sync
            then Just $ snd <$> combined
            else Nothing
      where
        f txOuts =
          join $
            map (\txOut -> toList $ (txOut,) `fmap` userDatum txOut) $
              filter isValid $
              filter hasUserToken txOuts

        g users = (\t@(_, d) -> (dPkh d, t)) `fmap` users

        unique users =
          let pkhs = dPkh . snd <$> users in
          length (nub pkhs) == length pkhs

    justUsers :: [((Ledger.TxOut, UserData), (Ledger.TxOut, UserData))]
    justUsers = PlutusTx.Prelude.fromMaybe (traceError "cannot retrieve users") maybeUsers

    uniqueUser :: ((Ledger.TxOut, UserData), (Ledger.TxOut, UserData))
    uniqueUser = case maybeUsers of
      Just [u] -> u
      _ -> PlutusTx.Prelude.traceError "no unique user"

    maybeOwnUser :: Maybe ((Ledger.TxOut, UserData), (Ledger.TxOut, UserData))
    maybeOwnUser = do
      users <- maybeUsers
      own <- Ledger.txInInfoResolved <$> Ledger.findOwnInput ctx
      find (\((inTxInfo, _), _) -> inTxInfo == own) users

    ownUser :: ((Ledger.TxOut, UserData), (Ledger.TxOut, UserData))
    ownUser = case maybeOwnUser of
      Just d -> d
      Nothing -> traceError "no output for user utxo"

    danaOnly :: Value.Value -> Bool
    danaOnly value = ((\(cs, tn, _) -> (cs, tn)) <$> Value.flattenValue value) == [Value.unAssetClass danaAsset]

    distributionOk :: Value.Value -> Value.Value -> Value.Value -> Bool
    distributionOk totalStake totalReward leftover =
      let users = justUsers
       in all ok users && (leftover == totalReward - distributed users) && positive leftover
      where
        ok ((_, UserData _ inBalance), (_, UserData _ outBalance)) =
          dReward outBalance - dReward inBalance == rewardHelper danaAsset totalStake totalReward (dStake inBalance)

        distributed users =
          fold $
            (\((_, UserData _ inBalance), (_, UserData _ outBalance)) -> dReward outBalance - dReward inBalance)
              <$> users

{- ORMOLU_ENABLE -}

-------------------------------------------------------------------------------

data ValidatorTypes
instance Scripts.ValidatorTypes ValidatorTypes where
  type DatumType ValidatorTypes = Datum
  type RedeemerType ValidatorTypes = Redeemer

{-# INLINEABLE inst #-}
inst :: Value.AssetClass -> Value.CurrencySymbol -> Value.CurrencySymbol -> Scripts.TypedValidator ValidatorTypes
inst danaAsset nft symbol =
  Scripts.mkTypedValidator @ValidatorTypes
    ( $$(PlutusTx.compile [||mkValidator||])
        `PlutusTx.applyCode` PlutusTx.liftCode danaAsset
        `PlutusTx.applyCode` PlutusTx.liftCode nft
        `PlutusTx.applyCode` PlutusTx.liftCode symbol
    )
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.wrapValidator @Datum @Redeemer

--
{-# INLINEABLE spInst #-}
spInst :: Value.CurrencySymbol -> Scripts.TypedValidator ValidatorTypes
spInst nft = inst DanaCurrency.danaAsset nft userInitProofSymbol

{-# INLINEABLE spValidator #-}
spValidator :: Value.CurrencySymbol -> Ledger.Validator
spValidator nft = Scripts.validatorScript $ spInst nft

{-# INLINEABLE spAddress #-}
spAddress :: Value.CurrencySymbol -> Ledger.Address
spAddress nft = Ledger.scriptAddress $ spValidator nft

-------------------------------------------------------------------------------

{-# INLINEABLE positive #-}
positive :: Ledger.Value -> Bool
positive v = all (>= 0) $ (\(_, _, i) -> i) <$> Value.flattenValue v

{-# INLINEABLE txOutValid #-}
txOutValid :: Value.AssetClass -> Ledger.TxInfo -> Ledger.TxOut -> Bool
txOutValid tokenAssetClass info txOut = case datumForOnchain info txOut of
  Just (UserDatum dat) ->
    let value = Ledger.txOutValue txOut
        balance = dBalance dat

        syncOk = value == dReward balance <> dStake balance <> Value.assetClassValue tokenAssetClass 1
        rewardOk = (positive . dReward) balance
        stakeOk = (positive . dStake) balance
     in syncOk && rewardOk && stakeOk
  _ -> False

{-# INLINEABLE rewardHelper #-}
rewardHelper :: Value.AssetClass -> Value.Value -> Value.Value -> Value.Value -> Value.Value
rewardHelper danaAsset totalStake totalReward stake =
  reward
    (Value.assetClassValueOf totalStake danaAsset)
    totalReward
    (Value.assetClassValueOf stake danaAsset)

{-# INLINEABLE reward #-}
reward :: Integer -> Value.Value -> Integer -> Value.Value
reward totalStake totalReward userStake =
  let ratio = userStake R.% totalStake
   in fold $
        (\(cs, tn, v) -> Value.singleton cs tn (R.truncate (ratio * fromInteger v)))
          <$> Value.flattenValue totalReward