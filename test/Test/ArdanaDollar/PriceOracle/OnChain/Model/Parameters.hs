module Test.ArdanaDollar.PriceOracle.OnChain.Model.Parameters (
  TestParameters (..),
  SpenderParams (..),
  StateUTXOParams (..),
  TestDatumParameters (..),
  mockCurrencySymbol,
  correctNFTCurrency,
  correctStateTokenValue,
) where

import Ledger qualified
import Plutus.V1.Ledger.Api (singleton)
import PlutusTx.Prelude
import Prelude (Show)

---------------------------------------------------------------------------------
-- mock currency symbol. can't construct with minter due to cycle
mockCurrencySymbol :: Ledger.CurrencySymbol
mockCurrencySymbol = "123456789012345678901234567890ef"

correctNFTCurrency :: (Ledger.CurrencySymbol, Ledger.TokenName)
correctNFTCurrency = (mockCurrencySymbol, "PriceTracking")

correctStateTokenValue :: Ledger.Value
correctStateTokenValue = uncurry singleton correctNFTCurrency 1

---------------------------------------------------------------------------------

data TestParameters = TestParameters
  { stateNFTCurrency :: (Ledger.CurrencySymbol, Ledger.TokenName)
  , timeRangeLowerBound :: Integer
  , timeRangeUpperBound :: Integer
  , ownerWallet :: Integer
  , transactorParams :: SpenderParams
  , inputParams :: StateUTXOParams
  , outputParams :: StateUTXOParams
  , peggedCurrency :: BuiltinByteString
  }
  deriving (Show)

data SpenderParams = NoSigner | JustSignedBy Integer | SignedByWithValue Integer Ledger.Value
  deriving (Show)

data StateUTXOParams = StateUTXOParams
  { stateTokenValue :: Ledger.Value
  , stateDatumValue :: TestDatumParameters
  }
  deriving (Show)

-- NOTE: we are not modelling missing/invalid datum
-- I haven't figured out a nice way to do that with Tasty.Plutus yet
data TestDatumParameters = TestDatumParameters
  { signedByWallet :: Integer
  , timeStamp :: Integer
  }
  deriving (Show)
