{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

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
import Codec.Serialise (serialise)
import PlutusTx qualified
import PlutusTx.Prelude (BuiltinData, check)
import PlutusTx.IsData.Class (UnsafeFromData, unsafeFromBuiltinData)
import Data.ByteString.Lazy qualified as B
import Ledger.Typed.Scripts qualified as Scripts

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

main :: IO ()
main = do
  let (Validator vs) = vaultValidator dummyPubKeyHash
  let (MintingPolicy ns) = nodeValidPolicy (MapInstance dummyAssetClass)
  let (Validator es) = emptyValidator
  let (Validator es') = emptyValidator'
  putStrLn $ "vaultValidator: " <> (show . B.length . serialise $ vs)
  putStrLn $ "nodeValidPolicy: " <> (show . B.length . serialise $ ns)
  putStrLn $ "emptyValidator: " <> (show . B.length . serialise $ es)
  putStrLn $ "emptyValidator': " <> (show . B.length . serialise $ es')
