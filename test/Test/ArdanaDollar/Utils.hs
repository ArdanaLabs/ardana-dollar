{-# LANGUAGE NumericUnderscores #-}

module Test.ArdanaDollar.Utils where

import qualified PlutusTx.Ratio as R
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import ArdanaDollar.Types
import ArdanaDollar.Utils


liqRatio :: R.Rational
liqRatio = 150 R.% 100

vaultUnitTests :: TestTree
vaultUnitTests = testGroup "vault arithmetic" $
  [ let exr = 16283 R.% 1_000_000
        coll = 1_000_000
        debt = 3000
    -- https://www.youtube.com/watch?v=GjMI_1zkBzo&t=455s
    in testGroup "1" $ [ testCase "collaterizationRatio" $
                           assertEqual "" (Finite (16283 R.% 3000))
                           (collaterizationRatio exr coll debt)
                       , testCase "maxDebtAllowed" $
                           assertEqual "" 10855
                           (maxDebtAllowed exr liqRatio coll)
                       , testCase "minCollateralRequired" $
                           assertEqual "" 276362
                           (minCollateralRequired exr liqRatio debt)
                       , testCase "liquidationPrice" $
                           assertEqual "" (Just (4500 R.% 1_000_000))
                           (liquidationPrice liqRatio coll debt)
                       , testCase "availableToGenerate" $
                           assertEqual "" 7855
                           (availableToGenerate exr liqRatio coll debt)
                       , testCase "availableToWithdraw" $
                           assertEqual "" 723638
                           (availableToWithdraw exr liqRatio coll debt)
                       ]

  , let exr = 16283 R.% 1_000_000
        coll = 1_000_000
        debt = 6000
    -- https://www.youtube.com/watch?v=GjMI_1zkBzo&t=540s
    in testGroup "2" $ [ testCase "collaterizationRatio" $
                           assertEqual "" (Finite (16283 R.% 6000))
                           (collaterizationRatio exr coll debt)
                       , testCase "maxDebtAllowed" $
                           assertEqual "" 10855
                           (maxDebtAllowed exr liqRatio coll)
                       , testCase "minCollateralRequired" $
                           assertEqual "" 552724
                           (minCollateralRequired exr liqRatio debt)
                       , testCase "liquidationPrice" $
                           assertEqual "" (Just (9000 R.% 1_000_000))
                           (liquidationPrice liqRatio coll debt)
                       , testCase "availableToGenerate" $
                           assertEqual "" 4855
                           (availableToGenerate exr liqRatio coll debt)
                       , testCase "availableToWithdraw" $
                           assertEqual "" 447276
                           (availableToWithdraw exr liqRatio coll debt)
                       ]

  , let exr = 16283 R.% 1_000_000
        coll = 1_300_000
        debt = 6000
    -- https://www.youtube.com/watch?v=GjMI_1zkBzo&t=935s
    in testGroup "3" $ [ testCase "collaterizationRatio" $
                           assertEqual "" (Finite (211679 R.% 60000))
                           (collaterizationRatio exr coll debt)
                       , testCase "maxDebtAllowed" $
                           assertEqual "" 14111
                           (maxDebtAllowed exr liqRatio coll)
                       , testCase "minCollateralRequired" $
                           assertEqual "" 552724
                           (minCollateralRequired exr liqRatio debt)
                       , testCase "liquidationPrice" $
                           assertEqual "" (Just (9000 R.% 1_300_000))
                           (liquidationPrice liqRatio coll debt)
                       , testCase "availableToGenerate" $
                           assertEqual "" 8111
                           (availableToGenerate exr liqRatio coll debt)
                       , testCase "availableToWithdraw" $
                           assertEqual "" 747276
                           (availableToWithdraw exr liqRatio coll debt)
                       ]

  , let exr = 16283 R.% 1_000_000
        coll = 1_300_000
        debt = 7800
    -- https://www.youtube.com/watch?v=GjMI_1zkBzo&t=960s
    in testGroup "4" $ [ testCase "collaterizationRatio" $
                           assertEqual "" (Finite (16283 R.% 6000))
                           (collaterizationRatio exr coll debt)
                       , testCase "maxDebtAllowed" $
                           assertEqual "" 14111
                           (maxDebtAllowed exr liqRatio coll)
                       , testCase "minCollateralRequired" $
                           assertEqual "" 718541
                           (minCollateralRequired exr liqRatio debt)
                       , testCase "liquidationPrice" $
                           assertEqual "" (Just (9000 R.% 1_000_000))
                           (liquidationPrice liqRatio coll debt)
                       , testCase "availableToGenerate" $
                           assertEqual "" 6311
                           (availableToGenerate exr liqRatio coll debt)
                       , testCase "availableToWithdraw" $
                           assertEqual "" 581459
                           (availableToWithdraw exr liqRatio coll debt)
                       ]

  , let exr = 2162 R.% 100_000_000
        coll = 200_000_000
        debt = 2000
    -- https://www.youtube.com/watch?v=GjMI_1zkBzo&t=1030s
    in testGroup "5" $ [ testCase "collaterizationRatio" $
                           assertEqual "" (Finite (1081 R.% 500))
                           (collaterizationRatio exr coll debt)
                       , testCase "maxDebtAllowed" $
                           assertEqual "" 2882
                           (maxDebtAllowed exr liqRatio coll)
                       , testCase "minCollateralRequired" $
                           assertEqual "" 138760408
                           (minCollateralRequired exr liqRatio debt)
                       , testCase "liquidationPrice" $
                           assertEqual "" (Just (15 R.% 1_000_000))
                           (liquidationPrice liqRatio coll debt)
                       , testCase "availableToGenerate" $
                           assertEqual "" 882
                           (availableToGenerate exr liqRatio coll debt)
                       , testCase "availableToWithdraw" $
                           assertEqual "" 61239592
                           (availableToWithdraw exr liqRatio coll debt)
                       ]

  , let exr = 2162 R.% 100_000_000
        coll = 200_000_000
        debt = 0
    -- https://www.youtube.com/watch?v=GjMI_1zkBzo&t=1060s
    in testGroup "6" $ [ testCase "collaterizationRatio" $
                           assertEqual "" Infinity
                           (collaterizationRatio exr coll debt)
                       , testCase "maxDebtAllowed" $
                           assertEqual "" 2882
                           (maxDebtAllowed exr liqRatio coll)
                       , testCase "minCollateralRequired" $
                           assertEqual "" 0
                           (minCollateralRequired exr liqRatio debt)
                       , testCase "liquidationPrice" $
                           assertEqual "" (Just (0 R.% 1))
                           (liquidationPrice liqRatio coll debt)
                       , testCase "availableToGenerate" $
                           assertEqual "" 2882
                           (availableToGenerate exr liqRatio coll debt)
                       , testCase "availableToWithdraw" $
                           assertEqual "" 200000000
                           (availableToWithdraw exr liqRatio coll debt)
                       ]

  , let exr = 2162 R.% 100_000_000
        coll = 0
        debt = 0
    -- https://www.youtube.com/watch?v=GjMI_1zkBzo&t=1075s
    in testGroup "7" $ [ testCase "collaterizationRatio" $
                           assertEqual "" Zero
                           (collaterizationRatio exr coll debt)
                       , testCase "maxDebtAllowed" $
                           assertEqual "" 0
                           (maxDebtAllowed exr liqRatio coll)
                       , testCase "minCollateralRequired" $
                           assertEqual "" 0
                           (minCollateralRequired exr liqRatio debt)
                       , testCase "liquidationPrice" $
                           assertEqual "" Nothing
                           (liquidationPrice liqRatio coll debt)
                       , testCase "availableToGenerate" $
                           assertEqual "" 0
                           (availableToGenerate exr liqRatio coll debt)
                       , testCase "availableToWithdraw" $
                           assertEqual "" 0
                           (availableToWithdraw exr liqRatio coll debt)
                       ]
  ]
