module Test.Data.StickBreakingSet.Mock (mockStickBreakingSetTests) where

import Control.Monad (join)
import Data.Kind (Type)
import Data.List (isPrefixOf)
import Data.Map qualified as M
import Data.Set qualified as S
import Hedgehog (
  Group (..),
  MonadGen,
  Property,
  footnoteShow,
  forAll,
  property,
  (===),
 )
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (fromGroup)
import Prelude
import Safe (maximumMay)

mockStickBreakingSetTests :: TestTree
mockStickBreakingSetTests = testGroup "StickBreakingSet" [fromGroup $ Group "tests" [("is a set", mockSetIsASet)]]

-- this is a mock of the datum - BuiltinByteString would replace String on chain
-- note there are no links between nodes so on-chain traversal is impossible by design
-- what this set provides is efficient proof of absence, existence, and insertion (I'm thinking about deletion)
data Stick = Stick
  { prefix :: String
  , leafs :: [String]
  , branches :: [String]
  }
  deriving (Show)

-- this is a mock of something that may exist off-chain for creating transactions and off-chain traversal
newtype MockSet = MockSet (M.Map String Stick)
  deriving (Show)

mockEmpty :: MockSet
mockEmpty = MockSet (M.singleton "" (Stick "" [] []))

-- this is a proof that can be done on chain that consumes a single Stick and reproduces it
mockContains :: MockSet -> String -> Bool
mockContains (MockSet m) s =
  let longestSharedPrefix = longest $ filter (`elem` M.keys m) ((s %) <$> M.keys m)
   in case M.lookup longestSharedPrefix m of
        Nothing -> error "map lookup error"
        Just so -> drop (length (prefix so)) s `elem` leafs so

-- the insertion can be done on chain - it involves consuming a single Stick and producing one or two sticks
-- to insert on chain we require a proof that the element is not contained
-- - in this mock that is handled by the guard in the first expression
-- - on chain we fail if the proof is not present
mockInsert :: MockSet -> String -> MockSet
mockInsert ms s | mockContains ms s = ms
mockInsert (MockSet m) s =
  let longestSharedPrefix = longest $ filter (`elem` M.keys m) ((s %) <$> M.keys m)
   in case M.lookup longestSharedPrefix m of
        Nothing -> error "malformed set"
        Just so ->
          let wop = drop (length (prefix so)) s
           in case longest ((wop %) <$> leafs so) of
                "" ->
                  case longest ((wop %) <$> branches so) of
                    -- The case where the insert goes into an existing node
                    "" -> MockSet (M.insert longestSharedPrefix (so {leafs = drop (length longestSharedPrefix) s : leafs so}) m)
                    e ->
                      -- The case where a branch is broken
                      --  - it is replaced with base of the branch containing the inserted node and the tip of the branch
                      let rmd = M.insert longestSharedPrefix (so {branches = e : filter (not . isPrefixOf e) (branches so)}) m
                          np = longestSharedPrefix <> e
                          w = head $ filter (isPrefixOf e) (branches so)
                       in MockSet (M.insert np (Stick np [drop (length np) s] [drop (length e) w]) rmd)
                e ->
                  -- The case where a leaf is broken
                  -- - it is deleted and the common prefix with the inserted element becomes a branch containing
                  --   the broken leaf and the inserted element
                  let rmd = M.insert longestSharedPrefix (so {leafs = filter (not . isPrefixOf e) (leafs so), branches = e : branches so}) m
                      np = longestSharedPrefix <> e
                      w = head $ filter (isPrefixOf e) (leafs so)
                   in MockSet (M.insert np (Stick np [drop (length e) w, drop (length np) s] []) rmd)

---- this requires traversal and is not possible to do on chain. it's here for testing
--mockSize :: MockSet -> Int
--mockSize (MockSet m) = sum (length . leafs . snd <$> M.toList m)

-- also for testing
mockAsList :: MockSet -> [String]
mockAsList (MockSet m) = join [(p <>) <$> ls | Stick p ls _ <- snd <$> M.toList m]

mockFromSet :: S.Set String -> MockSet
mockFromSet s = foldr (flip mockInsert) mockEmpty $ S.toList s

genSetOfStrings ::
  forall (m :: Type -> Type).
  MonadGen m =>
  m (S.Set String)
genSetOfStrings = do
  l <- Gen.list (Range.linear 0 100) (Gen.string (Range.singleton 32) Gen.alphaNum)
  pure $ S.fromList l

mockSetIsASet :: Property
mockSetIsASet =
  property $ do
    s <- forAll genSetOfStrings
    let mock = mockFromSet s
    footnoteShow mock
    s === S.fromList (mockAsList mock)

-- helpers

-- oh god, what sorcery is this?!? it's code golf for shared prefix of a String e.g.  "apple" % "apply" = "appl"
(%) :: String -> String -> String
(c : x) % (d : y) | c == d = c : x % y; _ % _ = ""

longest :: [String] -> String
longest xss = maybe "" snd (maximumMay [(length xs, xs) | xs <- xss])

