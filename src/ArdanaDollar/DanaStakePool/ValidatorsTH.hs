{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module ArdanaDollar.DanaStakePool.ValidatorsTH (
  ValidatorTypes,
  spInst,
  spValidator,
  spAddress,
  userInitProofAssetClass,
  userInitProofPolicy,
) where

import ArdanaDollar.DanaStakePool.DanaCurrency qualified as DanaCurrency
import ArdanaDollar.DanaStakePool.Types
import ArdanaDollar.DanaStakePool.Validators

import Ledger qualified
import Ledger.Typed.Scripts qualified as Scripts
import Ledger.Value qualified as Value
import PlutusTx qualified
import PlutusTx.Prelude
import PlutusTx.TH qualified as TH

-------------------------------------------------------------------------------

{-# INLINEABLE userInitProofPolicy #-}
userInitProofPolicy :: NFTAssetClass -> Scripts.MintingPolicy
userInitProofPolicy nftAC =
  Ledger.mkMintingPolicyScript $
    $$(TH.compile [||Scripts.wrapMintingPolicy . mkUserInitProofPolicy||])
      `PlutusTx.applyCode` PlutusTx.liftCode nftAC

{-# INLINEABLE userInitProofSymbol #-}
userInitProofSymbol :: NFTAssetClass -> Value.CurrencySymbol
userInitProofSymbol = Ledger.scriptCurrencySymbol . userInitProofPolicy

{-# INLINEABLE userInitProofTokenName #-}
userInitProofTokenName :: Value.TokenName
userInitProofTokenName = Value.TokenName emptyByteString

{-# INLINEABLE userInitProofAssetClass #-}
userInitProofAssetClass :: NFTAssetClass -> Value.AssetClass
userInitProofAssetClass nftAC = Value.AssetClass (userInitProofSymbol nftAC, userInitProofTokenName)

-------------------------------------------------------------------------------

data ValidatorTypes
instance Scripts.ValidatorTypes ValidatorTypes where
  type DatumType ValidatorTypes = Datum
  type RedeemerType ValidatorTypes = Redeemer

{-# INLINEABLE inst #-}
inst :: DanaAssetClass -> NFTAssetClass -> UserInitProofAssetClass -> Scripts.TypedValidator ValidatorTypes
inst danaAsset nft symbol =
  Scripts.mkTypedValidator @ValidatorTypes
    ( $$(PlutusTx.compile [||mkValidator||])
        `PlutusTx.applyCode` PlutusTx.liftCode danaAsset
        `PlutusTx.applyCode` PlutusTx.liftCode nft
        `PlutusTx.applyCode` PlutusTx.liftCode symbol
    )
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.wrapValidator @Datum @Redeemer

--
{-# INLINEABLE spInst #-}
spInst :: NFTAssetClass -> Scripts.TypedValidator ValidatorTypes
spInst nftAC = inst (DanaAssetClass DanaCurrency.danaAsset) nftAC (UserInitProofAssetClass (userInitProofAssetClass nftAC))

{-# INLINEABLE spValidator #-}
spValidator :: NFTAssetClass -> Ledger.Validator
spValidator nftAC = Scripts.validatorScript $ spInst nftAC

{-# INLINEABLE spAddress #-}
spAddress :: NFTAssetClass -> Ledger.Address
spAddress nftAC = Ledger.scriptAddress $ spValidator nftAC
