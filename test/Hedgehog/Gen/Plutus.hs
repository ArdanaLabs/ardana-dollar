module Hedgehog.Gen.Plutus (
  address,
  assetClass,
  builtinByteString,
  currencySymbol,
  pubKey,
  pubKeyHash,
  pubKeyWithHash,
  positiveSingletonValue,
  singletonValue,
  tokenName,
  txId,
  txOutRef,
  validatorHash,
  positiveValue,
  value,
  subvalue,
) where

import Control.Monad.Identity (Identity)
import Data.Kind (Type)
import Hedgehog (GenBase, MonadGen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Gen.Extra (integer)
import Hedgehog.Range qualified as Range
import Prelude

import Ledger qualified
import Ledger.Scripts qualified as Scripts
import Ledger.Value qualified as Value
import Plutus.V1.Ledger.Address (pubKeyHashAddress, scriptHashAddress)
import Plutus.V1.Ledger.Bytes (LedgerBytes (..))
import Plutus.V1.Ledger.Crypto (PubKey (..), PubKeyHash (..))
import Plutus.V1.Ledger.Tx (TxOutRef (..))
import Plutus.V1.Ledger.TxId (TxId (..))
import PlutusTx.Builtins.Internal (BuiltinByteString (..))

builtinByteString ::
  forall (m :: Type -> Type).
  MonadGen m =>
  Range.Range Int ->
  m BuiltinByteString
builtinByteString range = BuiltinByteString <$> Gen.utf8 range Gen.unicodeAll

address :: forall (m :: Type -> Type). MonadGen m => m Ledger.Address
address =
  Gen.choice
    [ pubKeyHashAddress <$> pubKeyHash
    , scriptHashAddress <$> validatorHash
    ]

assetClass :: forall (m :: Type -> Type). MonadGen m => m Value.AssetClass
assetClass = Value.assetClass <$> currencySymbol <*> tokenName

currencySymbol :: forall (m :: Type -> Type). MonadGen m => m Value.CurrencySymbol
currencySymbol = Value.currencySymbol <$> Gen.utf8 (Range.singleton 32) Gen.unicodeAll

ledgerBytes :: forall (m :: Type -> Type). MonadGen m => m LedgerBytes
ledgerBytes = LedgerBytes <$> builtinByteString (Range.singleton 32)

pubKey :: forall (m :: Type -> Type). MonadGen m => m PubKey
pubKey = PubKey <$> ledgerBytes

pubKeyHash :: forall (m :: Type -> Type). MonadGen m => m PubKeyHash
pubKeyHash = PubKeyHash <$> builtinByteString (Range.singleton 32)

pubKeyWithHash :: forall (m :: Type -> Type). MonadGen m => m (PubKey, PubKeyHash)
pubKeyWithHash = do
  pk <- pubKey
  let pkh = Ledger.pubKeyHash pk
  pure (pk, pkh)

positiveSingletonValue :: forall (m :: Type -> Type). MonadGen m => m Value.Value
positiveSingletonValue =
  Value.singleton <$> currencySymbol <*> tokenName
    <*> Gen.integral (Range.linear 1 10000)

singletonValue :: forall (m :: Type -> Type). MonadGen m => m Value.Value
singletonValue = Value.singleton <$> currencySymbol <*> tokenName <*> integer

tokenName :: forall (m :: Type -> Type). MonadGen m => m Value.TokenName
tokenName = Value.tokenName <$> Gen.utf8 (Range.constant 0 256) Gen.unicodeAll

txId :: forall (m :: Type -> Type). MonadGen m => m TxId
txId = TxId <$> builtinByteString (Range.singleton 32)

txOutRef :: forall (m :: Type -> Type). MonadGen m => m TxOutRef
txOutRef = TxOutRef <$> txId <*> integer

validatorHash :: forall (m :: Type -> Type). MonadGen m => m Scripts.ValidatorHash
validatorHash = Scripts.ValidatorHash <$> builtinByteString (Range.singleton 32)

positiveValue :: forall (m :: Type -> Type). MonadGen m => m Value.Value
positiveValue = mconcat <$> Gen.list (Range.linear 1 32) positiveSingletonValue

value :: forall (m :: Type -> Type). MonadGen m => m Value.Value
value = mconcat <$> Gen.list (Range.linear 0 32) singletonValue

subvalue :: forall (m :: Type -> Type). (MonadGen m, GenBase m ~ Identity) => Bool -> Value.Value -> m (Maybe Value.Value)
subvalue nonEmpty v =
  let flattenV = Value.flattenValue v
   in if nonEmpty && null flattenV
        then return Nothing
        else
          Just <$> do
            sublist <-
              if nonEmpty
                then Gen.filter (not . null) (Gen.subsequence flattenV)
                else Gen.subsequence flattenV
            let subvals = (\(cs, tn, i) -> Value.singleton cs tn i) <$> sublist
            return $ mconcat subvals
