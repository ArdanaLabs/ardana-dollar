{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module ArdanaDollar.DanaStakePool.Validators (
  NFTAssetClass (..),
  ValidatorTypes,
  spInst,
  spValidator,
  spAddress,
  Datum (..),
  UserData (..),
  GlobalData (..),
  Redeemer (..),
  Balance (..),
  TraversalState (..),
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
import PlutusTx.TH qualified as TH

import Prelude qualified as Haskell

import ArdanaDollar.DanaStakePool.DanaCurrency qualified as DanaCurrency
import ArdanaDollar.DanaStakePool.Utils (intersectionWith)
import ArdanaDollar.Utils (datumForOnchain)

import Control.Monad (join)

import GHC.Generics (Generic)

import Data.Aeson qualified as JSON

newtype DanaAssetClass = DanaAssetClass {unDanaAssetClass :: Value.AssetClass}
newtype NFTAssetClass = NFTAssetClass {unNFTAssetClass :: Value.AssetClass}
  deriving stock (Haskell.Show, Generic, Haskell.Eq)
  deriving anyclass (JSON.FromJSON, JSON.ToJSON)
newtype UserInitProofAssetClass = UserInitProofAssetClass {unUserInitProofAssetClass :: Value.AssetClass}

PlutusTx.makeLift ''DanaAssetClass
PlutusTx.makeLift ''NFTAssetClass
PlutusTx.makeLift ''UserInitProofAssetClass

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
  , dId :: Integer
  }
  deriving stock (Haskell.Show, Generic)
  deriving anyclass (JSON.FromJSON, JSON.ToJSON)

data TraversalState = TraversalInactive | TraversalActive Value.Value Integer -- reward pool, number of already visited users
  deriving stock (Haskell.Show, Generic, Haskell.Eq)
  deriving anyclass (JSON.FromJSON, JSON.ToJSON)

data GlobalData = GlobalData
  { dTotalStake :: Value.Value
  , dUserDatumCount :: Integer
  , dLocked :: Bool
  , dTraversal :: TraversalState
  }
  deriving stock (Haskell.Show, Generic, Haskell.Eq)
  deriving anyclass (JSON.FromJSON, JSON.ToJSON)

addTotalStake :: GlobalData -> Value.Value -> GlobalData
addTotalStake (GlobalData s c l t) v = GlobalData (s <> v) c l t

data Datum = UserDatum UserData | GlobalDatum GlobalData
  deriving stock (Haskell.Show, Generic)
  deriving anyclass (JSON.FromJSON, JSON.ToJSON)

PlutusTx.makeIsDataIndexed ''Balance [('Balance, 0)]
PlutusTx.makeIsDataIndexed ''UserData [('UserData, 0)]
PlutusTx.makeIsDataIndexed ''TraversalState [('TraversalInactive, 0), ('TraversalActive, 1)]

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
  | InitializeUser
  deriving stock (Haskell.Show, Generic)
  deriving anyclass (JSON.FromJSON, JSON.ToJSON)

PlutusTx.makeIsDataIndexed
  ''Redeemer
  [ ('DepositOrWithdraw, 0)
  , ('ProvideRewards, 1)
  , ('DistributeRewards, 2)
  , ('WithdrawRewards, 3)
  , ('InitializeUser, 4)
  ]

-------------------------------------------------------------------------------

blah :: forall a. PlutusTx.IsData a => Ledger.TxInfo -> Ledger.TxOut -> Maybe (Ledger.TxOut, a)
blah info txOut = do
  r <- datumForOnchain @a info txOut
  return (txOut, r)

{-# INLINEABLE mkUserInitProofPolicy #-}
mkUserInitProofPolicy :: NFTAssetClass -> () -> Ledger.ScriptContext -> Bool
mkUserInitProofPolicy nftAC _ ctx =
  let outputs = join $ toList . blah @Datum info <$> Ledger.txInfoOutputs info
      inputs = join $ toList . blah @Datum info <$> (Ledger.txInInfoResolved <$> Ledger.txInfoInputs info)
   in case (inputs, outputs) of
        ([(t1, GlobalDatum d1)], [(t2, GlobalDatum d2), (t3, UserDatum d3)]) -> validate (t1, d1) (t2, d2) (t3, d3)
        ([(t1, GlobalDatum d1)], [(t2, UserDatum d2), (t3, GlobalDatum d3)]) -> validate (t1, d1) (t3, d3) (t2, d2)
        _ -> traceIfFalse "incorrect utxos provided" False
  where
    validate (t1, inputGlobal) (t2, outputGlobal) (t3, outputUser) =
      traceIfFalse "minting more than 1" checkMintedAmount
        && traceIfFalse "nonempty out balance" (dBalance outputUser Haskell.== mempty)
        && traceIfFalse "not signed" (Ledger.txSignedBy info (dPkh outputUser))
        && traceIfFalse "user out tx invalid" (txOutValid (Value.AssetClass (Ledger.ownCurrencySymbol ctx, userInitProofTokenName)) info t3)
        && traceIfFalse "incorrect id" (dId outputUser == dUserDatumCount inputGlobal)
        && traceIfFalse "global datum unexpected change" (dUserDatumCount inputGlobal + 1 == dUserDatumCount outputGlobal)
        -- && traceIfFalse "global datum unexpected change" (inputGlobal{dUserDatumCount = (dUserDatumCount inputGlobal + 1)} == outputGlobal)
        && traceIfFalse "rewards changed" (Ledger.txOutValue t1 == Ledger.txOutValue t2)
        && traceIfFalse "no nft at input" (Value.assetClassValueOf (Ledger.txOutValue t1) (unNFTAssetClass nftAC) == 1)
        && traceIfFalse "no nft at output" (Value.assetClassValueOf (Ledger.txOutValue t2) (unNFTAssetClass nftAC) == 1)

    info :: Ledger.TxInfo
    info = Ledger.scriptContextTxInfo ctx

    checkMintedAmount :: Bool
    checkMintedAmount = case Value.flattenValue (Ledger.txInfoForge info) of
      [(cs, tn', amt)] -> cs == Ledger.ownCurrencySymbol ctx && tn' == Value.TokenName emptyByteString && amt == 1
      _ -> False

{-# INLINEABLE userInitProofPolicy #-}
userInitProofPolicy :: NFTAssetClass -> Scripts.MintingPolicy
userInitProofPolicy nftAC =
  Ledger.mkMintingPolicyScript $
    $$(TH.compile [||Scripts.wrapMintingPolicy . mkUserInitProofPolicy||])
      `PlutusTx.applyCode` PlutusTx.liftCode nftAC

{-# INLINEABLE userInitProofSymbol #-}
userInitProofSymbol :: NFTAssetClass -> Value.CurrencySymbol
userInitProofSymbol = Ledger.scriptCurrencySymbol . userInitProofPolicy

{-# INLINEABLE userInitProofTokenName #-}
userInitProofTokenName :: Value.TokenName
userInitProofTokenName = Value.TokenName emptyByteString

{-# INLINEABLE userInitProofAssetClass #-}
userInitProofAssetClass :: NFTAssetClass -> Value.AssetClass
userInitProofAssetClass nftAC = Value.AssetClass (userInitProofSymbol nftAC, userInitProofTokenName)

-------------------------------------------------------------------------------

{- ORMOLU_DISABLE -}

{-# INLINEABLE mkValidator #-}
mkValidator ::
  DanaAssetClass ->
  NFTAssetClass ->
  UserInitProofAssetClass ->
  Datum ->
  Redeemer ->
  Ledger.ScriptContext ->
  Bool
mkValidator danaAC nftAC userInitProofAC datum redeemer ctx =
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
                                  gInData
                                  gOutData
                                  (Ledger.txOutValue gInTxOut)
                                  (Ledger.txOutValue gOutTxOut)
                               )

          WithdrawRewards   -> traceIfFalse "cannot use redeemer" False

          InitializeUser    -> checkUserInitProofMinted -- delegate checks to minting policy

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
          InitializeUser  ->   False
  where
    info :: Ledger.TxInfo
    info = Ledger.scriptContextTxInfo ctx

    isSigned :: UserData -> Bool
    isSigned dat = dPkh dat `elem` Ledger.txInfoSignatories info

    isValid :: Ledger.TxOut -> Bool
    isValid txOut = txOutValid (unUserInitProofAssetClass userInitProofAC) info txOut

    hasNFT :: Ledger.TxOut -> Bool
    hasNFT txOut =
      1
        == Value.assetClassValueOf
          (Ledger.txOutValue txOut)
          (unNFTAssetClass nftAC)

    hasUserToken :: Ledger.TxOut -> Bool
    hasUserToken txOut = Value.assetClassValueOf (Ledger.txOutValue txOut) (unUserInitProofAssetClass userInitProofAC) == 1

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
    danaOnly value = ((\(cs, tn, _) -> (cs, tn)) <$> Value.flattenValue value) == [Value.unAssetClass $ unDanaAssetClass danaAC]

    distributionOk :: GlobalData -> GlobalData -> Value.Value -> Value.Value -> Bool
    distributionOk gInData gOutData totalRewardBeforeT totalRewardAfterT =
      case (dUserDatumCount gInData, dTraversal gInData, dTraversal gOutData) of
         (num, _, _)                                              | num == 0
            -> traceIfFalse "0 case" False

         (_, TraversalInactive, TraversalActive r' id')           | id' == 0
            -> traceIfFalse "1 case" $
               r' == totalRewardBeforeT
            && r' == totalRewardAfterT

         (num, TraversalActive r' id', TraversalInactive)         | id' == num - 1
            -> traceIfFalse "2 case" $
               ok r'
            && userId == id'

         (num, TraversalActive r' id', TraversalActive r'' id'')  | id' < num - 1 && id'' == id' + 1
            -> traceIfFalse "3 case" $
               r' == r''
            && ok r'
            && userId == id'

         _  -> traceIfFalse "4 case" False
      where
        user = uniqueUser

        userId = dId $ snd $ fst user

        okReward totalReward ((_, UserData _ inBalance _), (_, UserData _ outBalance _)) =
          let diff = dReward outBalance - dReward inBalance
              reward' = rewardHelper (unDanaAssetClass danaAC) (dTotalStake gInData) totalReward (dStake inBalance)
           in (diff == reward', diff)

        ok totalReward =
          let (okReward', diff) = okReward totalReward user
              leftoverOk = (diff == totalRewardBeforeT - totalRewardAfterT)
           in okReward' && leftoverOk && positive diff

    checkUserInitProofMinted :: Bool
    checkUserInitProofMinted = case Value.flattenValue (Ledger.txInfoForge info) of
      [(cs, tn', amt)] -> unUserInitProofAssetClass userInitProofAC == Value.AssetClass (cs, tn') && amt == 1
      _ -> False

{- ORMOLU_ENABLE -}

-------------------------------------------------------------------------------

data ValidatorTypes
instance Scripts.ValidatorTypes ValidatorTypes where
  type DatumType ValidatorTypes = Datum
  type RedeemerType ValidatorTypes = Redeemer

{-# INLINEABLE inst #-}
inst :: DanaAssetClass -> NFTAssetClass -> UserInitProofAssetClass -> Scripts.TypedValidator ValidatorTypes
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
spInst :: NFTAssetClass -> Scripts.TypedValidator ValidatorTypes
spInst nftAC = inst (DanaAssetClass DanaCurrency.danaAsset) nftAC (UserInitProofAssetClass (userInitProofAssetClass nftAC))

{-# INLINEABLE spValidator #-}
spValidator :: NFTAssetClass -> Ledger.Validator
spValidator nftAC = Scripts.validatorScript $ spInst nftAC

{-# INLINEABLE spAddress #-}
spAddress :: NFTAssetClass -> Ledger.Address
spAddress nftAC = Ledger.scriptAddress $ spValidator nftAC

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