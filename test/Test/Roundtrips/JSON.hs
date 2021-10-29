{-# LANGUAGE AllowAmbiguousTypes #-}

module Test.Roundtrips.JSON (
  jsonRoundtripTests,
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as Aeson
import Data.Kind (Type)
import Hedgehog (Gen, Property, evalMaybe, forAll, property, (===))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)
import Prelude

import Hedgehog.Gen.ArdanaDollar

jsonRoundtripTests :: TestTree
jsonRoundtripTests =
  testGroup
    "JSON roundtrips"
    [ testGroup "Vault" [testJsonLaws "VaultDatum" vaultDatum]
    , testGroup "Buffer" [testJsonLaws "BufferDatum" bufferDatum]
    , testGroup
        "DanaStakePool"
        [ testJsonLaws "NFTAssetClass" danaNftAssetClass
        , testJsonLaws "Balance" danaBalance
        , testJsonLaws "UserData" danaUserData
        , testJsonLaws "TraversalState" danaTraversalState
        , testJsonLaws "GlobalData" danaGlobalData
        , testJsonLaws "Datum" danaDatum
        , testJsonLaws "Redeemer" danaRedeemer
        ]
    , testGroup
        "PriceOracle"
        [ testJsonLaws "OracleMintingParams" oracleMintingParams
        , testJsonLaws "OracleValidatorParams" oracleValidatorParams
        , testJsonLaws "PriceTracking" priceTracking
        ]
    , testGroup
        "Treasury"
        [ testJsonLaws "Treasury" treasury
        , testJsonLaws "TreasuryStateTokenParams" treasuryStateTokenParams
        , testJsonLaws "TreasuryState" treasuryState
        , testJsonLaws "TreasuryDepositParams" treasuryDepositParams
        , testJsonLaws "TreasurySpendParams" treasurySpendParams
        , testJsonLaws "NewContract" newContract
        ]
    , testGroup
        "Onchain map"
        [testJsonLaws "MapInstance" onchainMapMapInstance]
    ]

-- LAWS
testJsonLaws ::
  forall (a :: Type). (Eq a, FromJSON a, ToJSON a, Show a) => String -> Gen a -> TestTree
testJsonLaws typeName gen =
  testGroup
    ("JSON laws for " <> typeName)
    [ testProperty ("Partial Isomorphism of " <> typeName) (jsonRoundtripPartialIsomorphism gen)
    , testProperty ("Encoding Equals Value of " <> typeName) (jsonRoundtripEncodingEqualsValue gen)
    ]

jsonRoundtripPartialIsomorphism ::
  forall (a :: Type). (Eq a, FromJSON a, Show a, ToJSON a) => Gen a -> Property
jsonRoundtripPartialIsomorphism gen = property $ do
  x <- forAll gen
  Aeson.decode (Aeson.encode x) === Just x

jsonRoundtripEncodingEqualsValue :: forall a. (Show a, ToJSON a) => Gen a -> Property
jsonRoundtripEncodingEqualsValue gen = property $ do
  x <- forAll gen
  (v :: Aeson.Value) <- evalMaybe . Aeson.decode . Aeson.encode $ x
  Aeson.toJSON x === v
