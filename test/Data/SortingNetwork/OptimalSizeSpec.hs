module Data.SortingNetwork.OptimalSizeSpec (
  spec,
) where

import Control.Monad
import Data.SortingNetwork.OptimalSize
import Test.Hspec

{-

-- easiest to just generate those:

import Text.Printf
let f i = printf "  %d -> sortList%dBy\n" i i
mapM_ f [2..16]

 -}

{-
  Routes to a specific partial function given length.

  TODO: Note that since we don't generate type signatures in TH for now,
  this is also to make sure that we have inferenced types that are general enough.
 -}
sortFnRouter :: Int -> Ord a => (a -> a -> Ordering) -> [a] -> [a]
sortFnRouter = \case
  2 -> sortList2By
  3 -> sortList3By
  4 -> sortList4By
  5 -> sortList5By
  6 -> sortList6By
  7 -> sortList7By
  8 -> sortList8By
  9 -> sortList9By
  10 -> sortList10By
  11 -> sortList11By
  12 -> sortList12By
  13 -> sortList13By
  14 -> sortList14By
  15 -> sortList15By
  16 -> sortList16By
  v -> error $ "Missing function for length " <> show v

isSorted :: Ord a => [a] -> Bool
isSorted xs = and (zipWith (<=) xs (tail xs))

spec :: Spec
spec = forM_ [2 .. 16] \n ->
  describe ("sortList" <> show n <> "By") do
    specify "0-1 principle" do
      let inputs :: [] [Bool]
          inputs = replicateM n [False, True]
          sortFn = sortFnRouter n (compare @Bool)
      forM_ inputs \inp ->
        sortFn inp `shouldSatisfy` isSorted
