import ArdanaDollar.Vault (vaultValidator)
import ArdanaDollar.Map.Types (MapInstance(MapInstance))
import ArdanaDollar.Map.ValidatorsTH (nodeValidPolicy)
import Plutus.V1.Ledger.Value (AssetClass(AssetClass))
import Plutus.V1.Ledger.Api (CurrencySymbol(CurrencySymbol), TokenName(TokenName), PubKeyHash(PubKeyHash))
import Plutus.V1.Ledger.Scripts (Validator(Validator), MintingPolicy(MintingPolicy))
import Data.Aeson qualified as Aeson
import Data.Maybe (fromJust)
import Prelude
import Codec.Serialise (serialise)
import Data.ByteString.Lazy qualified as B

dummyAssetClass :: AssetClass
dummyAssetClass = AssetClass
  ( CurrencySymbol . fromJust . Aeson.decode $ "\"42E9FCF913093DFA0246DD743804E7AD437207119B0027ECD3401BB7496880E6\""
  , TokenName . fromJust . Aeson.decode $ "\"42E9FCF913093DFA0246DD743804E7AD437207119B0027ECD3401BB7496880E6\""
  )

dummyPubKeyHash :: PubKeyHash
dummyPubKeyHash = PubKeyHash . fromJust . Aeson.decode $ "\"DCA286992118F6BC911E8FED01E731A5EFF18C9914FC8E73AF55923DE8FD711D\""

main :: IO ()
main = do
  let (Validator vs) = vaultValidator dummyPubKeyHash
  let (MintingPolicy ns) = nodeValidPolicy (MapInstance dummyAssetClass)
  putStrLn $ "vaultValidator: " <> (show . B.length . serialise $ vs)
  putStrLn $ "nodeValidPolicy: " <> (show . B.length . serialise $ ns)
