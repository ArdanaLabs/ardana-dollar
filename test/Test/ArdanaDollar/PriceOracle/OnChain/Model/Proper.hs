{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Test.ArdanaDollar.PriceOracle.OnChain.Model.Proper (
  priceOracleTests,
  PriceOracleModel (..),
) where

import ArdanaDollar.PriceOracle.OnChain
import Control.Monad (join)
import Control.Monad.Trans.Reader (
  ReaderT (runReaderT),
  ask,
 )
import Data.Kind (Type)
import Data.Set qualified as Set
import Hedgehog (
  Group (..),
  MonadGen,
 )
import Hedgehog.Gen qualified as Gen
import Hedgehog.Gen.Plutus qualified as HP
import Hedgehog.Range qualified as Range
import Ledger (
  Address,
  AssetClass,
  CurrencySymbol,
  Extended (Finite),
  Interval (..),
  LowerBound (..),
  POSIXTime (..),
  PrivateKey,
  PubKey,
  PubKeyHash,
  TokenName,
  TxOutRef (..),
  UpperBound (..),
  Value,
  pubKeyHash,
  pubKeyHashAddress,
  scriptHashAddress,
 )
import Ledger.CardanoWallet qualified as CW
import Ledger.Oracle (
  signMessage,
 )
import Plutus.V1.Ledger.Api (
  TxOut (..),
  getValue,
 )
import Plutus.V1.Ledger.Contexts (
  ScriptPurpose (..),
 )
import Plutus.V1.Ledger.Scripts (
  Redeemer (..),
  ValidatorHash,
 )
import Plutus.V1.Ledger.Value (singleton)
import PlutusCore.Evaluation.Machine.ExMemory (ExCPU (..), ExMemory (..))
import PlutusTx (
  toBuiltinData,
 )
import PlutusTx.AssocMap qualified as AssocMap
import PlutusTx.Builtins (BuiltinData)
import PlutusTx.Prelude (
  BuiltinByteString,
  isJust,
 )
import PlutusTx.UniqueMap qualified as UniqueMap
import Proper.Plutus
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (fromGroup)
import Prelude (
  Applicative (..),
  Bool (..),
  Bounded (..),
  Enum,
  Eq,
  Int,
  Integer,
  Maybe (..),
  Monoid (..),
  Ord,
  Semigroup (..),
  Show,
  elem,
  filter,
  flip,
  fst,
  pure,
  snd,
  uncurry,
  ($),
  (&&),
  (+),
  (-),
  (.),
  (/=),
  (<),
  (<$>),
  (<=),
  (==),
 )

priceOracleTests :: Int -> TestTree
priceOracleTests contractMaxSuccesses =
  let shortTests =
        [ Group "Price Oracle quick check" [("model", quickCheckModelTest Model), ("plutus", quickCheckPlutusTest Model)]
        ]
      longTests =
        join
          [ [ testEnumeratedScenarios Model "PriceOracle validation failure scenarios" combinedTestGivenProperties (Not expect)
            , testEnumeratedScenarios Model "PriceOracle validation success scenarios" combinedTestGivenProperties expect
            ]
          | 75 <= contractMaxSuccesses
          ]
   in testGroup "PriceOracle" $ fromGroup <$> (shortTests <> longTests)

data PriceOracleModel = Model deriving (Show)

data TransactorParams = NoSigner | JustSignedBy Integer | SignedByWithValue Integer Value
  deriving (Show)

data StateUTXOParams = StateUTXOParams
  { stateTokenValue :: Value
  , stateDatumValue :: Maybe TestDatumParameters
  }
  deriving (Show)

data TestDatumParameters = TestDatumParameters
  { signedByWallet :: Integer
  , timeStamp :: Integer
  , fiatPriceFeedData :: UniqueMap.Map BuiltinByteString Integer
  , cryptoPriceFeedData :: UniqueMap.Map AssetClass Integer
  }
  deriving (Show)

instance IsProperty (Property PriceOracleModel)

instance Proper PriceOracleModel where
  data Model PriceOracleModel
    = PriceOracleStateMachineModel
        { stateNFTCurrency :: (CurrencySymbol, TokenName)
        , timeRangeLowerBound :: Integer
        , timeRangeUpperBound :: Integer
        , ownerWallet :: Integer
        , transactorParams :: TransactorParams
        , inputParams :: StateUTXOParams
        , outputParams :: StateUTXOParams
        , peggedCurrency :: BuiltinByteString
        , valueRetrieved :: Maybe ([Value], Address)
        }
    | PriceOracleMinterModel
        { stateNFTCurrency :: (CurrencySymbol, TokenName)
        , timeRangeLowerBound :: Integer
        , timeRangeUpperBound :: Integer
        , ownerWallet :: Integer
        , transactorParams :: TransactorParams
        , outputParams :: StateUTXOParams
        }
    deriving (Show)

  data Property PriceOracleModel
    = PriceOracleMintingPolicyContext
    | PriceOracleStateMachineContext
    | OutputDatumTimestampIsInRange
    | RangeWithinSizeLimit
    | OutputDatumSignedByOwner
    | TransactionSignedByOwner
    | StateTokenReturned
    | InputDatumIsCorrectType
    | OutputDatumIsCorrectType
    | OwnerIsRetrievingValue
    | InputPriceTrackingDatumIsEmpty
    | OutputPriceTrackingDatumIsEmpty
    deriving stock (Enum, Eq, Ord, Bounded, Show)

  satisfiesProperty = flip satisfiesProperty'

  logic =
    All
      [ ExactlyOne (Var <$> [PriceOracleMintingPolicyContext, PriceOracleStateMachineContext])
      , Some
          ( Var
              <$> [ InputDatumIsCorrectType
                  , OwnerIsRetrievingValue
                  ]
          )
          :->: Var PriceOracleStateMachineContext
      , -- parsing will fail before we can check properties of the datum hence these implications
        Var OutputDatumSignedByOwner :->: Var OutputDatumIsCorrectType
      , Var OutputDatumTimestampIsInRange :->: Var OutputDatumIsCorrectType
      , Var OutputPriceTrackingDatumIsEmpty :->: Var OutputDatumIsCorrectType
      , Var InputPriceTrackingDatumIsEmpty :->: Var InputDatumIsCorrectType
      ]

  expect =
    All
      [ Var PriceOracleStateMachineContext
          :->: All
            ( Var
                <$> [ OutputDatumTimestampIsInRange
                    , RangeWithinSizeLimit
                    , OutputDatumSignedByOwner
                    , TransactionSignedByOwner
                    , StateTokenReturned
                    , InputDatumIsCorrectType
                    , OutputDatumIsCorrectType
                    ]
            )
      , Var PriceOracleMintingPolicyContext
          :->: All
            ( Var
                <$> [ OutputDatumTimestampIsInRange
                    , OutputPriceTrackingDatumIsEmpty
                    , RangeWithinSizeLimit
                    , TransactionSignedByOwner
                    , StateTokenReturned
                    , OutputDatumIsCorrectType
                    , OutputDatumSignedByOwner
                    ]
            )
      ]

  genModel = genModel' . Set.toList

  modelScriptPurpose PriceOracleMinterModel {..} = Minting $ fst $ correctNFTCurrency params
    where
      params = oracleMintingParams ownerWallet
  modelScriptPurpose model = Spending . TxOutRef (modelTxId model) $ 0

  modelTxMint PriceOracleMinterModel {..} = singleton c t 1
    where
      (c, t) = correctNFTCurrency $ oracleMintingParams ownerWallet
  modelTxMint _ = mempty

  modelRedeemer PriceOracleMinterModel {} = Redeemer $ toBuiltinData ("90ab" :: ValidatorHash)
  modelRedeemer _ = Redeemer $ toBuiltinData ()

  script PriceOracleStateMachineModel {..} = Just $ CompiledValidator $ oracleValidator params
    where
      ownerPubKey :: PubKey
      ownerPubKey = CW.pubKey (CW.knownWallet ownerWallet)
      ownerPubKeyHash :: PubKeyHash
      ownerPubKeyHash = pubKeyHash ownerPubKey
      params :: OracleValidatorParams
      params = OracleValidatorParams (fst stateNFTCurrency) ownerPubKey ownerPubKeyHash peggedCurrency
  script PriceOracleMinterModel {..} = Just $ CompiledMintingPolicy $ oracleMintingPolicy params
    where
      params = oracleMintingParams ownerWallet

  -- we could compute these bounds from the model
  modelCPUBudget _ = ExCPU 3_000_000_000
  modelMemoryBudget _ = ExMemory 30_000_000

  modelTxValidRange model =
    Interval
      (LowerBound (Finite (POSIXTime (timeRangeLowerBound model))) True)
      (UpperBound (Finite (POSIXTime (timeRangeUpperBound model))) True)

  modelTxSignatories model =
    case transactorParams model of
      NoSigner -> []
      JustSignedBy signer -> [go signer]
      SignedByWithValue signer _ -> [go signer]
    where
      go = CW.pubKeyHash . CW.knownWallet

  modelInputData PriceOracleStateMachineModel {..} =
    ( stateTokenValue inputParams
    , modelDatum' $ stateDatumValue inputParams
    ) :
    ( case valueRetrieved of
        Nothing -> []
        Just so -> (,toBuiltinData ()) <$> fst so
    )
  modelInputData PriceOracleMinterModel {} = []

  modelTxOutputs model@PriceOracleStateMachineModel {..}
    | isJust valueRetrieved =
      let stateReturn = (\(v, d) -> TxOut (scriptHashAddress $ modelValidatorHash model) v (justDatumHash d)) <$> modelOutputData model
       in case valueRetrieved of
            Nothing -> stateReturn
            Just so ->
              let valueRetrieval = [TxOut (snd so) (mconcat (fst so)) (justDatumHash $ toBuiltinData ())]
               in stateReturn <> valueRetrieval
  modelTxOutputs model =
    (\(v, d) -> TxOut (scriptHashAddress $ modelValidatorHash model) v (justDatumHash d)) <$> modelOutputData model

  modelOutputData model =
    [
      ( stateTokenValue (outputParams model)
      , modelDatum' $ stateDatumValue $ outputParams model
      )
    ]

{- HLINT ignore modelDatum' "Avoid restricted function" -}
modelDatum' :: Maybe TestDatumParameters -> BuiltinData
modelDatum' Nothing = toBuiltinData ()
modelDatum' (Just TestDatumParameters {..}) =
  toBuiltinData $ signMessage (PriceTracking fiatPriceFeedData cryptoPriceFeedData (POSIXTime timeStamp)) signedByPrivK
  where
    signedByPrivK :: PrivateKey
    signedByPrivK = lookupPrivateKey signedByWallet
    lookupPrivateKey :: Integer -> PrivateKey
    lookupPrivateKey i = CW.privateKey (CW.knownWallet i)

--helpers
--

oracleMintingParams :: Integer -> OracleMintingParams
oracleMintingParams walletIdx = OracleMintingParams ownerPubKey ownerPubKeyHash
  where
    ownerPubKey :: PubKey
    ownerPubKey = CW.pubKey $ CW.knownWallet walletIdx
    ownerPubKeyHash :: PubKeyHash
    ownerPubKeyHash = pubKeyHash ownerPubKey

correctNFTCurrency :: OracleMintingParams -> (CurrencySymbol, TokenName)
correctNFTCurrency params = (oracleCurrencySymbol params, "PriceTracking")

-- Property
---------------------------------------------------------------------------------

satisfiesProperty' :: Property PriceOracleModel -> Model PriceOracleModel -> Bool
satisfiesProperty' OutputDatumTimestampIsInRange = outputDatumTimestampIsInRange
satisfiesProperty' RangeWithinSizeLimit = rangeWithinSizeLimit
satisfiesProperty' OutputDatumSignedByOwner = outputDatumSignedByOwner
satisfiesProperty' TransactionSignedByOwner = transactionSignedByOwner
satisfiesProperty' StateTokenReturned = stateTokenReturned
satisfiesProperty' InputDatumIsCorrectType = inputDatumIsCorrectType
satisfiesProperty' OutputDatumIsCorrectType = outputDatumIsCorrectType
satisfiesProperty' PriceOracleMintingPolicyContext = isMinterModel
satisfiesProperty' PriceOracleStateMachineContext = isScriptModel
satisfiesProperty' OwnerIsRetrievingValue = ownerIsRetrievingValue
satisfiesProperty' OutputPriceTrackingDatumIsEmpty = outputPriceTrackingDatumIsEmpty
satisfiesProperty' InputPriceTrackingDatumIsEmpty = inputPriceTrackingDatumIsEmpty

type ModelProperty = Model PriceOracleModel -> Bool

isMinterModel :: ModelProperty
isMinterModel PriceOracleMinterModel {} = True
isMinterModel _ = False

isScriptModel :: ModelProperty
isScriptModel PriceOracleStateMachineModel {} = True
isScriptModel _ = False

outputDatumTimestampIsInRange :: ModelProperty
outputDatumTimestampIsInRange model =
  case stateDatumValue $ outputParams model of
    Nothing -> False
    Just so -> timeRangeLowerBound model <= timeStamp so && timeStamp so <= timeRangeUpperBound model

rangeWithinSizeLimit :: ModelProperty
rangeWithinSizeLimit model =
  let rangeLen = timeRangeUpperBound model - timeRangeLowerBound model
   in 0 < rangeLen && rangeLen <= 10000

outputDatumSignedByOwner :: ModelProperty
outputDatumSignedByOwner model =
  case stateDatumValue $ outputParams model of
    Nothing -> False
    Just so -> signedByWallet so == ownerWallet model

transactionSignedByOwner :: ModelProperty
transactionSignedByOwner model =
  case transactorParams model of
    NoSigner -> False
    JustSignedBy signer -> signer == ownerWallet model
    SignedByWithValue signer _ -> signer == ownerWallet model

stateTokenReturned :: ModelProperty
stateTokenReturned model =
  let c = fst $ correctNFTCurrency $ oracleMintingParams $ ownerWallet model
   in case AssocMap.lookup c $ getValue $ stateTokenValue $ outputParams model of
        Nothing -> False
        Just so -> case AssocMap.lookup (snd $ stateNFTCurrency model) so of
          Just 1 -> True
          _ -> False

inputDatumIsCorrectType :: ModelProperty
inputDatumIsCorrectType PriceOracleStateMachineModel {..} = isJust $ stateDatumValue inputParams
inputDatumIsCorrectType _ = False

outputDatumIsCorrectType :: ModelProperty
outputDatumIsCorrectType model = isJust $ stateDatumValue $ outputParams model

ownerIsRetrievingValue :: ModelProperty
ownerIsRetrievingValue PriceOracleStateMachineModel {..} = isJust valueRetrieved
ownerIsRetrievingValue _ = False

outputPriceTrackingDatumIsEmpty :: ModelProperty
outputPriceTrackingDatumIsEmpty model =
  case stateDatumValue $ outputParams model of
    Nothing -> False
    Just so -> UniqueMap.null (fiatPriceFeedData so) && UniqueMap.null (cryptoPriceFeedData so)

inputPriceTrackingDatumIsEmpty :: ModelProperty
inputPriceTrackingDatumIsEmpty model@PriceOracleStateMachineModel {} =
  case stateDatumValue $ inputParams model of
    Nothing -> False
    Just so -> UniqueMap.null (fiatPriceFeedData so) && UniqueMap.null (cryptoPriceFeedData so)
inputPriceTrackingDatumIsEmpty _ = False

-- generators
---------------------------------------------------------------------------------

genModel' :: MonadGen m => [Property PriceOracleModel] -> m (Model PriceOracleModel)
genModel' props =
  if PriceOracleStateMachineContext `elem` props
    then runReaderT genPriceOracleStateMachineModel props
    else runReaderT genPriceOracleMinterModel props

genPriceOracleMinterModel ::
  forall (m :: Type -> Type).
  MonadGen m =>
  ReaderT [Property PriceOracleModel] m (Model PriceOracleModel)
genPriceOracleMinterModel = do
  (tlb, tub) <- genTimeRange
  w <- genKnownWalletIdx
  let mint = oracleMintingParams w
  sp <- genTransactorParams w
  op <- genOutputUTXOParams mint w (tlb, tub)
  pure $
    PriceOracleMinterModel
      { stateNFTCurrency = correctNFTCurrency $ oracleMintingParams w
      , timeRangeLowerBound = tlb
      , timeRangeUpperBound = tub
      , ownerWallet = w
      , transactorParams = sp
      , outputParams = op
      }

genPriceOracleStateMachineModel ::
  forall (m :: Type -> Type).
  MonadGen m =>
  ReaderT [Property PriceOracleModel] m (Model PriceOracleModel)
genPriceOracleStateMachineModel = do
  (tlb, tub) <- genTimeRange
  w <- genKnownWalletIdx
  let mint = oracleMintingParams w
  sp <- genTransactorParams w
  ip <- genInputUTXOParams mint
  op <- genOutputUTXOParams mint w (tlb, tub)
  pc <- HP.builtinByteString (Range.linear 0 6)
  vr <- genValueRetrieved
  pure $
    PriceOracleStateMachineModel
      { stateNFTCurrency = correctNFTCurrency mint
      , timeRangeLowerBound = tlb
      , timeRangeUpperBound = tub
      , ownerWallet = w
      , transactorParams = sp
      , inputParams = ip
      , outputParams = op
      , peggedCurrency = pc
      , valueRetrieved = vr
      }

genTransactorParams ::
  forall (m :: Type -> Type).
  MonadGen m =>
  Integer ->
  ReaderT [Property PriceOracleModel] m TransactorParams
genTransactorParams w = do
  properties' <- ask
  if TransactionSignedByOwner `elem` properties'
    then do
      b <- Gen.bool
      if b
        then pure $ JustSignedBy w
        else do
          v <- HP.value
          pure $ SignedByWithValue w v
    else do
      b <- Gen.bool
      if b
        then pure NoSigner
        else do
          b' <- Gen.bool
          if b'
            then do
              w' <- genWalletIdxOtherThan w
              pure $ JustSignedBy w'
            else do
              w' <- genWalletIdxOtherThan w
              v <- HP.value
              pure $ SignedByWithValue w' v

genInputUTXOParams ::
  forall (m :: Type -> Type).
  MonadGen m =>
  OracleMintingParams ->
  ReaderT [Property PriceOracleModel] m StateUTXOParams
genInputUTXOParams mint = do
  stok <- genStateToken mint
  d <- genInputDatumParameters
  pure $ StateUTXOParams stok d

genOutputUTXOParams ::
  forall (m :: Type -> Type).
  MonadGen m =>
  OracleMintingParams ->
  Integer ->
  (Integer, Integer) ->
  ReaderT [Property PriceOracleModel] m StateUTXOParams
genOutputUTXOParams mint w ts = do
  stok <- genStateToken mint
  d <- genOutputDatumParameters w ts
  pure $ StateUTXOParams stok d

genKnownWalletIdx ::
  forall (m :: Type -> Type).
  MonadGen m =>
  m Integer
genKnownWalletIdx = Gen.integral (Range.linear 1 9)

genWalletIdxOtherThan ::
  forall (m :: Type -> Type).
  MonadGen m =>
  Integer ->
  m Integer
genWalletIdxOtherThan w = Gen.element $ filter (/= w) [1 .. 9]

genOutputTimeStamp ::
  forall (m :: Type -> Type).
  MonadGen m =>
  (Integer, Integer) ->
  ReaderT [Property PriceOracleModel] m Integer
genOutputTimeStamp ts = do
  properties' <- ask
  if OutputDatumTimestampIsInRange `elem` properties'
    then do
      Gen.integral (uncurry Range.linear ts)
    else do
      b <- Gen.bool
      if b
        then Gen.integral (Range.linear 0 (fst ts - 1))
        else Gen.integral (Range.linear (snd ts + 1) 300_000)

genInputTimeStamp ::
  forall (m :: Type -> Type).
  MonadGen m =>
  ReaderT [Property PriceOracleModel] m Integer
genInputTimeStamp = Gen.integral (Range.linear 0 100_000)

genTimeRange ::
  forall (m :: Type -> Type).
  MonadGen m =>
  ReaderT [Property PriceOracleModel] m (Integer, Integer)
genTimeRange = do
  properties' <- ask
  if RangeWithinSizeLimit `elem` properties'
    then do
      t1 <- Gen.integral (Range.linear 1 100_000)
      t2 <- Gen.integral (Range.linear (t1 + 1) (t1 + 10_000))
      pure (t1, t2)
    else do
      t1 <- Gen.integral (Range.linear 1 100_000)
      t2 <- Gen.integral (Range.linear (t1 + 10_1000) (t1 + 200_000))
      pure (t1, t2)

genOutputDatumParameters ::
  forall (m :: Type -> Type).
  MonadGen m =>
  Integer ->
  (Integer, Integer) ->
  ReaderT [Property PriceOracleModel] m (Maybe TestDatumParameters)
genOutputDatumParameters w ts = do
  properties' <- ask
  if OutputDatumIsCorrectType `elem` properties'
    then do
      walletIdx <-
        if OutputDatumSignedByOwner `elem` properties'
          then pure w
          else genWalletIdxOtherThan w
      t <- genOutputTimeStamp ts
      (f, c) <-
        if OutputPriceTrackingDatumIsEmpty `elem` properties'
          then pure (UniqueMap.empty, UniqueMap.empty)
          else (,) <$> genFiatPriceFeedMap <*> genCryptoPriceFeedMap
      pure $ Just $ TestDatumParameters walletIdx t f c
    else pure Nothing

genInputDatumParameters ::
  forall (m :: Type -> Type).
  MonadGen m =>
  ReaderT [Property PriceOracleModel] m (Maybe TestDatumParameters)
genInputDatumParameters = do
  properties' <- ask
  if InputDatumIsCorrectType `elem` properties'
    then do
      walletIdx <- genKnownWalletIdx
      t <- genInputTimeStamp
      (f, c) <-
        if InputPriceTrackingDatumIsEmpty `elem` properties'
          then pure (UniqueMap.empty, UniqueMap.empty)
          else (,) <$> genFiatPriceFeedMap <*> genCryptoPriceFeedMap
      pure $ Just $ TestDatumParameters walletIdx t f c
    else pure Nothing

genFiatPriceFeedMap ::
  forall (m :: Type -> Type).
  MonadGen m =>
  ReaderT [Property PriceOracleModel] m (UniqueMap.Map BuiltinByteString Integer)
genFiatPriceFeedMap = do
  vals <-
    Gen.list
      (Range.linear 1 4)
      ( (,) <$> HP.builtinByteString (Range.linear 0 10)
          <*> Gen.integral (Range.linear 0 1000)
      )
  pure $ UniqueMap.fromList vals

genCryptoPriceFeedMap ::
  forall (m :: Type -> Type).
  MonadGen m =>
  ReaderT [Property PriceOracleModel] m (UniqueMap.Map AssetClass Integer)
genCryptoPriceFeedMap = do
  vals <-
    Gen.list
      (Range.linear 1 4)
      ( (,) <$> HP.assetClass
          <*> Gen.integral (Range.linear 0 1000)
      )
  pure $ UniqueMap.fromList vals

genStateTokenCurrencySymbol ::
  forall (m :: Type -> Type).
  MonadGen m =>
  OracleMintingParams ->
  ReaderT [Property PriceOracleModel] m CurrencySymbol
genStateTokenCurrencySymbol mint = do
  properties' <- ask
  if StateTokenReturned `elem` properties'
    then pure $ fst $ correctNFTCurrency mint
    else HP.currencySymbol

genStateTokenTokenName ::
  forall (m :: Type -> Type).
  MonadGen m =>
  OracleMintingParams ->
  ReaderT [Property PriceOracleModel] m TokenName
genStateTokenTokenName mint = do
  properties' <- ask
  if StateTokenReturned `elem` properties'
    then pure $ snd $ correctNFTCurrency mint
    else HP.tokenName

genStateTokenAmount ::
  forall (m :: Type -> Type).
  MonadGen m =>
  ReaderT [Property PriceOracleModel] m Integer
genStateTokenAmount = do
  properties' <- ask
  if StateTokenReturned `elem` properties'
    then pure 1
    else do
      b <- Gen.bool
      if b
        then pure 0
        else Gen.integral (Range.linear 2 5)

genStateToken ::
  forall (m :: Type -> Type).
  MonadGen m =>
  OracleMintingParams ->
  ReaderT [Property PriceOracleModel] m Value
genStateToken mint = do
  s <- genStateTokenCurrencySymbol mint
  n <- genStateTokenTokenName mint
  v <- genStateTokenAmount
  pure $ singleton s n v

genValueRetrieved ::
  forall (m :: Type -> Type).
  MonadGen m =>
  ReaderT [Property PriceOracleModel] m (Maybe ([Value], Address))
genValueRetrieved = do
  properties' <- ask
  if OwnerIsRetrievingValue `elem` properties'
    then do
      a <- HP.pubKeyHash
      vals <- Gen.list (Range.linear 1 4) HP.singletonValue
      pure $ Just (vals, pubKeyHashAddress a)
    else pure Nothing
