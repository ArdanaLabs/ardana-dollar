module Test.Roundtrips.BuiltinData (
  builtinDataRoundtripTests,
) where

import Data.Kind (Type)
import Hedgehog (Gen, forAll, property, (===))
import PlutusTx.IsData (FromData (fromBuiltinData), ToData (toBuiltinData))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)
import Prelude

import Hedgehog.Gen.ArdanaDollar

builtinDataRoundtripTests :: TestTree
builtinDataRoundtripTests =
  testGroup
    "BuiltinData roundtrips"
    [ builtinDataPartialIsomorphism "VaultDatum" vaultDatum
    , builtinDataPartialIsomorphism "VaultRedeemer" vaultRedeemer
    , builtinDataPartialIsomorphism "BufferDatum" bufferDatum
    , builtinDataPartialIsomorphism "BufferAction" bufferAction
    , testGroup
        "DanaStakePool"
        [ builtinDataPartialIsomorphism "Balance" danaBalance
        , builtinDataPartialIsomorphism "UserData" danaUserData
        , builtinDataPartialIsomorphism "TraversalState" danaTraversalState
        , builtinDataPartialIsomorphism "GlobalData" danaGlobalData
        , builtinDataPartialIsomorphism "Datum" danaDatum
        , builtinDataPartialIsomorphism "Redeemer" danaRedeemer
        ]
    , testGroup
        "Treasury"
        [ builtinDataPartialIsomorphism "TreasuryDatum" treasuryDatum
        , builtinDataPartialIsomorphism "TreasuryDepositParams" treasuryDepositParams
        , builtinDataPartialIsomorphism "TreasuryAction" treasuryAction
        ]
    , testGroup
        "Onchain map"
        [ builtinDataPartialIsomorphism "MapInstance" onchainMapMapInstance
        , builtinDataPartialIsomorphism "Pointer" onchainMapPointer
        , builtinDataPartialIsomorphism "Datum" onchainMapDatum
        , builtinDataPartialIsomorphism "TokenRedeemer" onchainTokenRedeemer
        ]
    ]

builtinDataPartialIsomorphism ::
  forall (a :: Type).
  (Eq a, FromData a, Show a, ToData a) =>
  String ->
  Gen a ->
  TestTree
builtinDataPartialIsomorphism typeName gen =
  testProperty ("Partial Isomorphism of " <> typeName) . property $ do
    x <- forAll gen
    fromBuiltinData (toBuiltinData x) === Just x
