module Data.SortingNetwork.OptimalSizeSpec (
  spec,
) where

import Data.SortingNetwork.Common (mkEndToEndSpec)
import Data.SortingNetwork.OptimalSize
import Test.Hspec

{-
  Routes to a specific partial function given length.
 -}
sortFnRouter :: Int -> forall a. Ord a => (a -> a -> Ordering) -> [a] -> [a]
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

spec :: Spec
spec = mkEndToEndSpec sortFnRouter [2 .. 16]
