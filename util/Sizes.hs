{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{-# HLINT ignore #-}

import ArdanaDollar.Map.Types (MapInstance (MapInstance))
import ArdanaDollar.Map.ValidatorsTH (nodeValidPolicy)
import ArdanaDollar.Vault (vaultValidator)
import Cardano.Api qualified as C
import Cardano.Binary qualified as CBOR
import Codec.Serialise (Serialise, serialise)
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as SB
import Data.ByteString.Lazy qualified as B
import Data.Maybe (fromJust)
import Ledger.Typed.Scripts qualified as Scripts
import Plutus.V1.Ledger.Api (CurrencySymbol (CurrencySymbol), PubKeyHash (PubKeyHash), ScriptContext, TokenName (TokenName))
import Plutus.V1.Ledger.Scripts (Validator)
import Plutus.V1.Ledger.Value (AssetClass (AssetClass))
import Plutus.V2.Ledger.Api (TxOutRef, mkValidatorScript)
import PlutusTx qualified
import PlutusTx.IsData.Class (FromData, ToData, UnsafeFromData, toBuiltinData, unsafeFromBuiltinData)
import PlutusTx.Prelude (BuiltinData, check)
import Prelude

dummyAssetClass :: AssetClass
dummyAssetClass =
  AssetClass
    ( CurrencySymbol . fromJust . Aeson.decode $ "\"42E9FCF913093DFA0246DD743804E7AD437207119B0027ECD3401BB7496880E6\""
    , TokenName . fromJust . Aeson.decode $ "\"42E9FCF913093DFA0246DD743804E7AD437207119B0027ECD3401BB7496880E6\""
    )

dummyPubKeyHash :: PubKeyHash
dummyPubKeyHash = PubKeyHash . fromJust . Aeson.decode $ "\"DCA286992118F6BC911E8FED01E731A5EFF18C9914FC8E73AF55923DE8FD711D\""

data EmptyDatum = EmptyDatum
PlutusTx.makeIsDataIndexed ''EmptyDatum [('EmptyDatum, 0)]
data EmptyRedeemer = EmptyRedeemer
PlutusTx.makeIsDataIndexed ''EmptyRedeemer [('EmptyRedeemer, 0)]

data Emptying
instance Scripts.ValidatorTypes Emptying where
  type DatumType Emptying = EmptyDatum
  type RedeemerType Emptying = EmptyRedeemer

mkEmptyValidator ::
  EmptyDatum ->
  EmptyRedeemer ->
  ScriptContext ->
  Bool
mkEmptyValidator _ _ _ = True

emptyInst :: Scripts.TypedValidator Emptying
emptyInst =
  Scripts.mkTypedValidator @Emptying
    $$(PlutusTx.compile [||mkEmptyValidator||])
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.wrapValidator @EmptyDatum @EmptyRedeemer

emptyValidator :: Validator
emptyValidator = Scripts.validatorScript emptyInst

{-# INLINEABLE myWrapValidator #-}
myWrapValidator ::
  forall d r p.
  (UnsafeFromData d, UnsafeFromData r, UnsafeFromData p) =>
  (d -> r -> p -> Bool) ->
  BuiltinData ->
  BuiltinData ->
  BuiltinData ->
  ()
myWrapValidator f d r p = check (f (unsafeFromBuiltinData d) (unsafeFromBuiltinData r) (unsafeFromBuiltinData p))

data MyTxOutRef = MyTxOutRef
  { myTxOutRefId :: BuiltinData
  , myTxOutRefIdx :: Integer
  }

PlutusTx.makeIsDataIndexed ''MyTxOutRef [('MyTxOutRef, 0)]

data MyScriptPurpose = MyMinting BuiltinData | MySpending MyTxOutRef | MyRewarding BuiltinData | MyCertifying BuiltinData

PlutusTx.makeIsDataIndexed ''MyScriptPurpose [('MyMinting, 0), ('MySpending, 1), ('MyRewarding, 2), ('MyCertifying, 3)]

data MyScriptContext = MyScriptContext
  {myScriptContextTxInfo :: BuiltinData, scriptContextPurpose :: MyScriptPurpose}

PlutusTx.makeIsDataIndexed ''MyScriptContext [('MyScriptContext, 0)]

mkEmptyValidator' ::
  EmptyDatum ->
  EmptyRedeemer ->
  MyScriptContext ->
  Bool
mkEmptyValidator' _ _ _ = True

emptyValidator' :: Validator
emptyValidator' =
  mkValidatorScript $
    PlutusTx.applyCode
      $$(PlutusTx.compile [||wrap||])
      $$(PlutusTx.compile [||mkEmptyValidator'||])
  where
    wrap = myWrapValidator @EmptyDatum @EmptyRedeemer @MyScriptContext

newtype Spooky a = Spooky BuiltinData
  deriving newtype (FromData, UnsafeFromData, ToData)

unSpooky :: UnsafeFromData a => Spooky a -> a
unSpooky = unsafeFromBuiltinData . toBuiltinData

data SpookyTxOutRef' = SpookyTxOutRef
  { spookyTxOutRefId :: Spooky () -- placeholder
  , spookyTxOutRefIdx :: Spooky Integer
  }

type SpookyTxOutRef = Spooky SpookyTxOutRef'

PlutusTx.makeIsDataIndexed ''SpookyTxOutRef' [('SpookyTxOutRef, 0)]

data SpookyScriptPurpose' = SpookyMinting (Spooky ()) | SpookySpending (Spooky TxOutRef) | SpookyRewarding (Spooky ()) | SpookyCertifying (Spooky ())

type SpookyScriptPurpose = Spooky SpookyScriptPurpose'

PlutusTx.makeIsDataIndexed ''SpookyScriptPurpose' [('SpookyMinting, 0), ('SpookySpending, 1), ('SpookyRewarding, 2), ('SpookyCertifying, 3)]

data SpookyScriptContext' = SpookyScriptContext
  {spookyScriptContextTxInfo :: Spooky (), spookyScriptContextPurpose :: SpookyScriptPurpose}

type SpookyScriptContext = Spooky SpookyScriptContext'

PlutusTx.makeIsDataIndexed ''SpookyScriptContext' [('SpookyScriptContext, 0)]

mkSpookyValidator ::
  EmptyDatum ->
  EmptyRedeemer ->
  SpookyScriptContext ->
  Bool
mkSpookyValidator _ _ ctx =
  case (unSpooky . spookyScriptContextPurpose . unSpooky) ctx of
    SpookySpending _ -> True
    _ -> False

spookyValidator :: Validator
spookyValidator =
  mkValidatorScript $
    PlutusTx.applyCode
      $$(PlutusTx.compile [||wrap||])
      $$(PlutusTx.compile [||mkSpookyValidator||])
  where
    wrap = myWrapValidator @EmptyDatum @EmptyRedeemer @SpookyScriptContext

getSize :: Serialise a => a -> Int
getSize x =
  let bs = B.toStrict . serialise $ x
      -- HACK FIXME: this needs to be PlutusScriptV2, but we need a newer cardano-node source for that.
      script = fromJust $ C.deserialiseFromRawBytes (C.proxyToAsType undefined) bs :: C.PlutusScript C.PlutusScriptV1
   in SB.length . CBOR.serialize' $ script

main :: IO ()
main = do
  putStrLn $ "vaultValidator: " <> (show . getSize $ vaultValidator dummyPubKeyHash)
  putStrLn $ "nodeValidPolicy: " <> (show . getSize $ nodeValidPolicy (MapInstance dummyAssetClass))
  putStrLn $ "emptyValidator: " <> (show . getSize $ emptyValidator)
  putStrLn $ "emptyValidator': " <> (show . getSize $ emptyValidator')
  putStrLn $ "spookyValidator: " <> (show . getSize $ spookyValidator)
