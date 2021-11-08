{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{-# HLINT ignore #-}

import ArdanaDollar.Vault (vaultValidator)
import ArdanaDollar.Map.Types (MapInstance(MapInstance))
import ArdanaDollar.Map.ValidatorsTH (nodeValidPolicy)
import Plutus.V1.Ledger.Value (AssetClass(AssetClass))
import Plutus.V1.Ledger.Api (CurrencySymbol(CurrencySymbol), TokenName(TokenName), PubKeyHash(PubKeyHash), ScriptContext)
import Plutus.V2.Ledger.Api (mkValidatorScript)
import Plutus.V1.Ledger.Scripts (Validator(Validator), MintingPolicy(MintingPolicy))
import Data.Aeson qualified as Aeson
import Data.Maybe (fromJust)
import Prelude
import Codec.Serialise (serialise, Serialise)
import PlutusTx qualified
import PlutusTx.Prelude (BuiltinData, check)
import PlutusTx.IsData.Class (UnsafeFromData, unsafeFromBuiltinData)
import Data.ByteString.Lazy qualified as B
import Data.ByteString qualified as SB
import Ledger.Typed.Scripts qualified as Scripts
import Cardano.Binary qualified as CBOR
import Cardano.Api qualified as C

dummyAssetClass :: AssetClass
dummyAssetClass = AssetClass
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
    $$(PlutusTx.compile [||mkEmptyValidator||] )
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.wrapValidator @EmptyDatum @EmptyRedeemer

emptyValidator :: Validator
emptyValidator = Scripts.validatorScript emptyInst

{-# INLINABLE myWrapValidator #-}
myWrapValidator
    :: forall d r p
    . (UnsafeFromData d, UnsafeFromData r, UnsafeFromData p)
    => (d -> r -> p -> Bool)
    -> BuiltinData
    -> BuiltinData
    -> BuiltinData
    -> ()
myWrapValidator f d r p = check (f (unsafeFromBuiltinData d) (unsafeFromBuiltinData r) (unsafeFromBuiltinData p))

data MyTxOutRef = MyTxOutRef
  { myTxOutRefId :: BuiltinData
  , myTxOutRefIdx :: Integer
  }

PlutusTx.makeIsDataIndexed ''MyTxOutRef [('MyTxOutRef,0)]

data MyScriptPurpose = MyMinting BuiltinData | MySpending MyTxOutRef | MyRewarding BuiltinData | MyCertifying BuiltinData

PlutusTx.makeIsDataIndexed ''MyScriptPurpose [('MyMinting,0), ('MySpending,1), ('MyRewarding,2), ('MyCertifying,3)]

data MyScriptContext = MyScriptContext
  { myScriptContextTxInfo :: BuiltinData, scriptContextPurpose :: MyScriptPurpose }

PlutusTx.makeIsDataIndexed ''MyScriptContext [('MyScriptContext,0)]

mkEmptyValidator' ::
  EmptyDatum ->
  EmptyRedeemer ->
  MyScriptContext ->
  Bool
mkEmptyValidator' _ _ _ = True

emptyValidator' :: Validator
emptyValidator' = mkValidatorScript $
  PlutusTx.applyCode
    $$(PlutusTx.compile [||wrap||] )
    $$(PlutusTx.compile [||mkEmptyValidator'||] )
  where
    wrap = myWrapValidator @EmptyDatum @EmptyRedeemer @MyScriptContext

getSize :: Serialise a => a -> Int
getSize x =
  let
    bs = B.toStrict . serialise $ x
    -- HACK FIXME: this needs to be PlutusScriptV2, but we need a newer cardano-node source for that.
    script = fromJust $ C.deserialiseFromRawBytes (C.proxyToAsType undefined) bs :: C.PlutusScript C.PlutusScriptV1
  in
  SB.length . CBOR.serialize' $ script

main :: IO ()
main = do
  let (Validator vs) = vaultValidator dummyPubKeyHash
  let (MintingPolicy ns) = nodeValidPolicy (MapInstance dummyAssetClass)
  let (Validator es) = emptyValidator
  let (Validator es') = emptyValidator'
  putStrLn $ "vaultValidator: " <> (show . getSize $ vs)
  putStrLn $ "nodeValidPolicy: " <> (show . getSize $ ns)
  putStrLn $ "emptyValidator: " <> (show . getSize $ es)
  putStrLn $ "emptyValidator': " <> (show . getSize $ es')
