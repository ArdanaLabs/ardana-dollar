{-# LANGUAGE TypeFamilies #-}

module ArdanaDollar.DanaStakePool.Utils (
  positive,
  intersectionWith,
  sortBy,
) where

import PlutusTx.Prelude

import Ledger qualified
import Ledger.Value qualified

{-# INLINEABLE positive #-}
positive :: Ledger.Value -> Bool
positive v = all (>= 0) $ (\(_, _, i) -> i) <$> Ledger.Value.flattenValue v

{-# INLINEABLE sortBy #-}

{- | The 'sortBy' function is the non-overloaded version of 'sort'.
 >>> sortBy (\(a,_) (b,_) -> compare a b) [(2, "world"), (4, "!"), (1, "Hello")]
 [(1,"Hello"),(2,"world"),(4,"!")]
-}
sortBy :: (a -> a -> Ordering) -> [a] -> [a]
sortBy cmp = mergeAll . sequences
  where
    sequences (a : b : xs) = case a `cmp` b of
      GT -> descending b [a] xs
      _ -> ascending b (a :) xs
    sequences xs = [xs]

    descending a as (b : bs) = case a `cmp` b of
      GT -> descending b (a : as) bs
      _ -> (a : as) : sequences bs
    descending a as bs = (a : as) : sequences bs

    ascending a as (b : bs) = case a `cmp` b of
      GT ->
        let !x = as [a]
         in x : sequences bs
      _ -> ascending b (\ys -> as (a : ys)) bs
    ascending a as bs =
      let !x = as [a]
       in x : sequences bs

    mergeAll [x] = x
    mergeAll xs = mergeAll (mergePairs xs)

    mergePairs (a : b : xs) =
      let !x = merge a b
       in x : mergePairs xs
    mergePairs xs = xs

    merge as@(a : as') bs@(b : bs') = case a `cmp` b of
      GT -> b : merge as bs'
      _ -> a : merge as' bs
    merge [] bs = bs
    merge as [] = as

{-# INLINEABLE intersectionWith #-}

{- | The analogue of https://hackage.haskell.org/package/containers-0.6.5.1/docs/Data-Map-Internal.html#v:intersectionWith
 for PlutusTx.AssocMap.
 The assumption is that input lists how not contain duplicate keys.
 To be incorporated into plutus-extra.
-}
intersectionWith :: forall k a b c. Ord k => (a -> b -> c) -> [(k, a)] -> [(k, b)] -> [(k, c)]
intersectionWith f list1 list2 =
  let ord x y = compare (fst x) (fst y)
      sorted1 = sortBy ord list1
      sorted2 = sortBy ord list2
   in t sorted1 sorted2
  where
    t l1@(s1 : r1) l2@(s2 : r2) =
      case compare (fst s1) (fst s2) of
        LT -> t r1 l2
        EQ -> (fst s1, f (snd s1) (snd s2)) : t r1 r2
        GT -> t l1 r2
    t _ _ = []
