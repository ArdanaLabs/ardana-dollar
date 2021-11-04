import ArdanaDollar.Vault (vaultValidator)
import Plutus.V1.Ledger.Api (PubKeyHash(PubKeyHash))
import Plutus.V1.Ledger.Scripts (Validator(Validator))
import Data.Aeson qualified as Aeson
import Data.Maybe (fromJust)
import Prelude
import Codec.Serialise (serialise)
import Data.ByteString.Lazy qualified as B

dummyPubKeyHash :: PubKeyHash
dummyPubKeyHash = PubKeyHash . fromJust . Aeson.decode $ "\"DCA286992118F6BC911E8FED01E731A5EFF18C9914FC8E73AF55923DE8FD711D\""

main :: IO ()
main = do
  let (Validator vs) = vaultValidator dummyPubKeyHash
  let vss = serialise vs
  putStrLn $ "vaultValidator: " <> show (B.length vss)
