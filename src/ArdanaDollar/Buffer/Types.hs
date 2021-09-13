{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module ArdanaDollar.Buffer.Types (
  BufferDatum (..),
  BufferAction (..),
) where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Prelude qualified

--------------------------------------------------------------------------------

import PlutusTx qualified
import PlutusTx.Prelude

--------------------------------------------------------------------------------

data BufferDatum = BufferDatum
  { currentDebtAuctionPrice :: !Integer
  , currentSurplusAuctionPrice :: !Integer
  }
  deriving stock (Prelude.Eq, Generic, Prelude.Show)
  deriving anyclass (FromJSON, ToJSON)

data BufferAction
  = MkDebtBid Integer
  | MkSurplusBid Integer
  deriving (Prelude.Eq, Prelude.Show)

instance Eq BufferDatum where
  (BufferDatum debt1 surplus1) == (BufferDatum debt2 surplus2)
    | debt1 == debt2 && surplus1 == surplus2 = True
    | otherwise = False

PlutusTx.makeIsDataIndexed ''BufferDatum [('BufferDatum, 0)]
PlutusTx.makeIsDataIndexed
  ''BufferAction
  [ ('MkDebtBid, 0)
  , ('MkSurplusBid, 1)
  ]
PlutusTx.makeLift ''BufferAction
