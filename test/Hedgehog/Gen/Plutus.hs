module Hedgehog.Gen.Plutus (
  assetClass,
  builtinByteString,
  currencySymbol,
  pubKey,
  pubKeyHash,
  pubKeyWithHash,
  singletonValue,
  tokenName,
  txId,
  txOutRef,
  validatorHash,
  value,
) where

import Data.Kind (Type)
import Hedgehog (MonadGen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Gen.Extra (integer)
import Hedgehog.Range qualified as Range
import Prelude

import Ledger qualified
import Ledger.Scripts qualified as Scripts
import Ledger.Value qualified as Value
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

value :: forall (m :: Type -> Type). MonadGen m => m Value.Value
value = mconcat <$> Gen.list (Range.linear 0 32) singletonValue
