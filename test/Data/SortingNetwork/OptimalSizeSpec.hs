module Data.SortingNetwork.OptimalSizeSpec (
  spec,
) where

import Data.SortingNetwork.Common (mkEndToEndSpec)
import Data.SortingNetwork.OptimalSize
import Test.Hspec

{-
  Routes to a specific partial function given length.
 -}
sortFnRouter :: Int -> forall a. (a -> a -> Ordering) -> [a] -> [a]
sortFnRouter = \case
  2 -> unsafeSortList2By
  3 -> unsafeSortList3By
  4 -> unsafeSortList4By
  5 -> unsafeSortList5By
  6 -> unsafeSortList6By
  7 -> unsafeSortList7By
  8 -> unsafeSortList8By
  9 -> unsafeSortList9By
  10 -> unsafeSortList10By
  11 -> unsafeSortList11By
  12 -> unsafeSortList12By
  13 -> unsafeSortList13By
  14 -> unsafeSortList14By
  15 -> unsafeSortList15By
  16 -> unsafeSortList16By
  v -> error $ "Missing function for length " <> show v

spec :: Spec
spec = mkEndToEndSpec sortFnRouter [2 .. 16]
