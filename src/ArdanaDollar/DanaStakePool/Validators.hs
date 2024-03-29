{-# LANGUAGE TypeFamilies #-}

module ArdanaDollar.DanaStakePool.Validators (
  rewardHelper,
  mkUserInitProofPolicy,
  mkValidator,
) where

import Ledger qualified
import Ledger.Value qualified as Value
import PlutusTx qualified
import PlutusTx.Prelude
import PlutusTx.Ratio qualified as R

import ArdanaDollar.DanaStakePool.Types
import ArdanaDollar.Utils (datumForOnchain)

-- TODO: All the my* functions are because their counterparts in `plutus-tx` for
-- some unknown reason cause the PlutusTx compiler to err. Perhaps our version
-- of the plugin is too old?

{-# INLINEABLE myAll #-}
myAll :: (a -> Bool) -> [a] -> Bool
myAll _ [] = True
myAll f (x : xs) = f x && myAll f xs

{-# HLINT ignore myFoldValues #-}
{-# INLINEABLE myFoldValues #-}
myFoldValues :: [Value.Value] -> Value.Value
myFoldValues [] = mempty
myFoldValues (x : xs) = x <> myFoldValues xs

-- INLINEABLE isn't enough
{-# INLINE myElem #-}
myElem :: Eq a => a -> [a] -> Bool
myElem x (x' : _) | x == x' = True
myElem x (_ : xs) = myElem x xs
myElem _ [] = False

{-# INLINEABLE positive #-}
positive :: Ledger.Value -> Bool
positive v = myAll (>= 0) $ (\(_, _, i) -> i) <$> Value.flattenValue v

{-# INLINEABLE userTxOutValid #-}
userTxOutValid :: Value.AssetClass -> Ledger.TxInfo -> Ledger.TxOut -> Bool
userTxOutValid tokenAssetClass info txOut = case datumForOnchain info txOut of
  Just (UserDatum dat) ->
    let value = Ledger.txOutValue txOut
        balance = userData'balance dat

        syncOk =
          value == balance'reward balance
            <> balance'stake balance
            <> Value.assetClassValue tokenAssetClass 1

        rewardOk = (positive . balance'reward) balance
        stakeOk = (positive . balance'stake) balance
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
   in myFoldValues $
        (\(cs, tn, v) -> Value.singleton cs tn (R.truncate (ratio * fromInteger v)))
          <$> Value.flattenValue totalReward

{-# INLINEABLE datumTxOutTuple #-}
datumTxOutTuple :: forall a. PlutusTx.FromData a => Ledger.TxInfo -> Ledger.TxOut -> Maybe (Ledger.TxOut, a)
datumTxOutTuple info txOut = do
  r <- datumForOnchain @a info txOut
  return (txOut, r)

{-# INLINEABLE mkUserInitProofPolicy #-}
mkUserInitProofPolicy :: NFTAssetClass -> () -> Ledger.ScriptContext -> Bool
mkUserInitProofPolicy nftAC _ ctx =
  let outputs = Ledger.txInfoOutputs info >>= toList . datumTxOutTuple @Datum info
      inputs = Ledger.txInfoInputs info >>= (toList . datumTxOutTuple @Datum info) . Ledger.txInInfoResolved
   in case (inputs, outputs) of
        ([(t1, GlobalDatum d1)], [(t2, GlobalDatum d2), (t3, UserDatum d3)]) -> validate (t1, d1) (t2, d2) (t3, d3)
        ([(t1, GlobalDatum d1)], [(t2, UserDatum d2), (t3, GlobalDatum d3)]) -> validate (t1, d1) (t3, d3) (t2, d2)
        _ -> traceIfFalse "exactly one user UTXO and global UTXO expected" False
  where
    ownAssetClass = Value.AssetClass (Ledger.ownCurrencySymbol ctx, Value.TokenName emptyByteString)
    {- ORMOLU_DISABLE -}
    validate (gInTxOut, gInData) (gOutTxOut, gOutData) (t3, uOutData) =
           traceIfFalse "minting more or less than one"
           checkMintedAmount
        && traceIfFalse "global UTXO is locked"
           (not $ globalData'locked gInData)
        && traceIfFalse "nonempty out balance"
           (userData'balance uOutData == mempty)
        && traceIfFalse "not signed"
           (Ledger.txSignedBy info (userData'pkh uOutData))
        && traceIfFalse "user out tx invalid"
           (userTxOutValid ownAssetClass info t3)
        && traceIfFalse "incorrect id assigned to user UTXO"
           (userData'id uOutData == globalData'count gInData)
        && traceIfFalse "global datum unexpected change"
           -- Plutus doesn't support updating records...
           (gOutData ==
             let GlobalData totalStake count locked traversal = gInData in
             GlobalData totalStake (count + 1) locked traversal
           )
        && traceIfFalse "rewards changed"
           (Ledger.txOutValue gInTxOut == Ledger.txOutValue gOutTxOut)
        && traceIfFalse "no nft at input"
           (Value.assetClassValueOf (Ledger.txOutValue gInTxOut) (unNFTAssetClass nftAC) == 1)
        && traceIfFalse "no nft at output"
           (Value.assetClassValueOf (Ledger.txOutValue gOutTxOut) (unNFTAssetClass nftAC) == 1)
    {- ORMOLU_ENABLE -}

    info :: Ledger.TxInfo
    info = Ledger.scriptContextTxInfo ctx

    checkMintedAmount :: Bool
    checkMintedAmount = case Value.flattenValue (Ledger.txInfoMint info) of
      [(cs, tn', amt)] -> cs == Ledger.ownCurrencySymbol ctx && tn' == Value.TokenName emptyByteString && amt == 1
      _ -> False

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
  let ((gInTxOut, gInData), (gOutTxOut, gOutData)) = justGlobal
      ((_, uInData),        (_, uOutData))         = justUser

      totalStakeDiff = globalData'totalStake gOutData - globalData'totalStake gInData
  in
  case datum of
    GlobalDatum _ ->
        traceIfFalse "no valid & unique global UTXO"
        (isJust maybeGlobal)
     && case redeemer of
          DepositOrWithdraw -> traceIfFalse "is locked"
                               (not $ globalData'locked gInData)
                            && traceIfFalse "no valid & unique user UTXO"
                               (isJust maybeUser)
                            && traceIfFalse "signature missing"
                               (isSigned uInData)
                            && traceIfFalse "global and user stake not in sync"
                               (
                                   totalStakeDiff
                                     ==
                                   balance'stake (userData'balance uOutData) - balance'stake (userData'balance uInData)
                               )
                            && traceIfFalse "not only total stake changed in global UTXO"
                               (
                                   gOutData == gInData {globalData'totalStake = globalData'totalStake gOutData}
                                && Ledger.txOutValue gInTxOut == Ledger.txOutValue gOutTxOut
                               )
                            && traceIfFalse "stake not in dana"
                               (danaOnly totalStakeDiff)

          ProvideRewards    -> traceIfFalse "not only reward changed in global UTXO"
                               (gOutData == gInData)
                            && traceIfFalse "reward has decreased"
                               (positive $ Ledger.txOutValue gOutTxOut - Ledger.txOutValue gInTxOut)

          DistributeRewards -> traceIfFalse "total stake has changed"
                               (totalStakeDiff == mempty)
                            && traceIfFalse "user count has changed"
                               (globalData'count gOutData == globalData'count gInData)
                            && traceIfFalse "wrong reward distribution"
                               (  distributionOk
                                   gInData
                                   gOutData
                                   (Ledger.txOutValue gInTxOut <> negate nftToken)
                                   (Ledger.txOutValue gOutTxOut <> negate nftToken)
                               )
                            && traceIfFalse "user stake has changed"
                               (
                                   isNothing maybeUser -- user UTXO is not provided when starting rewards distribution
                                || (balance'stake (userData'balance uInData) == balance'stake (userData'balance uOutData))
                               )

          -- delegate validation to minting policy
          InitializeUser    -> checkUserInitProofMinted

          -- for those redeemers global UTXO cannot be spent
          WithdrawRewards   -> traceIfFalse "cannot spend this utxo"
                               False

    UserDatum _ ->
        traceIfFalse "no valid & unique user UTXO"
        (isJust maybeUser)
     && case redeemer of
          -- delegate to global UTXO validation
          DepositOrWithdraw -> traceIfFalse "global not present"
                               (isJust maybeGlobal)
          DistributeRewards -> traceIfFalse "global not present"
                               (isJust maybeGlobal)

          -- global UTXO is not required so validation has to happen here
          WithdrawRewards   -> traceIfFalse "signature missing"
                               (isSigned uInData)
                            && traceIfFalse "stake not preserved"
                               (balance'stake (userData'balance uInData) == balance'stake (userData'balance uOutData))

          -- for those redeemers user UTXO cannot be spent
          ProvideRewards    -> traceIfFalse "cannot spend this utxo"
                               False
          InitializeUser    -> traceIfFalse "cannot spend this utxo"
                               False
  where
    nftToken :: Value.Value
    nftToken = Value.assetClassValue (unNFTAssetClass nftAC) 1

    info :: Ledger.TxInfo
    info = Ledger.scriptContextTxInfo ctx

    isSigned :: UserData -> Bool
    isSigned dat = userData'pkh dat `myElem` Ledger.txInfoSignatories info

    isValid :: Ledger.TxOut -> Bool
    isValid txOut = userTxOutValid (unUserInitProofAssetClass userInitProofAC) info txOut

    hasNFT :: Ledger.TxOut -> Bool
    hasNFT txOut = 1 == Value.assetClassValueOf
                        (Ledger.txOutValue txOut)
                        (unNFTAssetClass nftAC)

    hasUserToken :: Ledger.TxOut -> Bool
    hasUserToken txOut = 1 == Value.assetClassValueOf
                         (Ledger.txOutValue txOut)
                         (unUserInitProofAssetClass userInitProofAC)

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
        f txOuts = case fmap (\txOut -> (txOut, globalDatum txOut)) $ filter hasNFT txOuts of
          [(txOut, Just dat)] -> Just (txOut, dat)
          _ -> Nothing

    maybeUser :: Maybe ((Ledger.TxOut, UserData), (Ledger.TxOut, UserData))
    maybeUser = do
      a@(_, d1) <- f (Ledger.getContinuingOutputs ctx)
      b@(_, d2) <- f (Ledger.txInInfoResolved <$> Ledger.txInfoInputs info)
      -- id and pkh cannot ever change, common validation
      let idOk = userData'id d1 == userData'id d2
      let pkhOk = userData'pkh d1 == userData'pkh d2
      if idOk && pkhOk then
        return (b, a)
      else
        Nothing
      where
        f txOuts = case fmap (\txOut -> (txOut, userDatum txOut))
                         $ filter isValid
                         $ filter hasUserToken txOuts
                   of
          -- valid only if unique such UTXO found
          [(txOut, Just dat)] -> Just (txOut, dat)
          _ -> Nothing

    justGlobal = case maybeGlobal of
      Just o -> o
      Nothing -> PlutusTx.Prelude.traceError "global missing"

    justUser = case maybeUser of
      Just o -> o
      Nothing -> PlutusTx.Prelude.traceError "user missing"

    danaOnly :: Value.Value -> Bool
    danaOnly value = case Value.flattenValue value of
      [(cs, tn, _)] -> (cs, tn) == (Value.unAssetClass . unDanaAssetClass $ danaAC)
      _ -> False

    distributionOk :: GlobalData -> GlobalData -> Value.Value -> Value.Value -> Bool
    distributionOk gInData gOutData totalRewardBeforeT totalRewardAfterT =
      case (globalData'count gInData, globaldata'traversal gInData, globaldata'traversal gOutData) of
         (num, _, _)                                              | num == 0
            -> traceIfFalse "0 case" False

         (_, TraversalInactive, TraversalActive r' id')           | id' == 0 && globalData'locked gOutData
            -> traceIfFalse "1 case" $
               r' == totalRewardBeforeT
            && r' == totalRewardAfterT

         (num, TraversalActive r' id', TraversalInactive)         | id' == num - 1 && not (globalData'locked gOutData)
            -> traceIfFalse "2 case" $
               ok r'
            && userId == id'

         (num, TraversalActive r' id', TraversalActive r'' id'')  | id' < num - 1 && id'' == id' + 1 && globalData'locked gOutData
            -> traceIfFalse "3 case" $
               r' == r''
            && ok r'
            && userId == id'

         _  -> traceIfFalse "4 case" False
      where
        userId = userData'id $ snd $ fst justUser

        okReward totalReward inBalance outBalance =
          let diff = balance'reward outBalance - balance'reward inBalance
              reward' = rewardHelper (unDanaAssetClass danaAC)
                                     (globalData'totalStake gInData)
                                     totalReward
                                     (balance'stake inBalance)
           in (diff == reward', diff)

        ok totalReward =
          let ((_, UserData _ inBalance _), (_, UserData _ outBalance _)) = justUser
              (okReward', diff) = okReward totalReward inBalance outBalance
              leftoverOk = (diff == totalRewardBeforeT - totalRewardAfterT)
           in okReward' && leftoverOk && positive diff

    checkUserInitProofMinted :: Bool
    checkUserInitProofMinted = case Value.flattenValue (Ledger.txInfoMint info) of
      [(cs, tn', amt)] -> unUserInitProofAssetClass userInitProofAC == Value.AssetClass (cs, tn') && amt == 1
      _ -> False
{- ORMOLU_ENABLE -}
