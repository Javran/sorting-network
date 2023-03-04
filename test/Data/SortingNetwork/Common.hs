module Data.SortingNetwork.Common (
  isSorted,
  mkEndToEndSpec,
) where

import Control.Monad
import Test.Hspec

isSorted :: Ord a => [a] -> Bool
isSorted xs = and (zipWith (<=) xs (tail xs))

mkEndToEndSpec ::
  (Int -> (forall a. Ord a => (a -> a -> Ordering) -> [a] -> [a])) ->
  [Int] ->
  Spec
mkEndToEndSpec sortFnRouter nRange = forM_ nRange \n ->
  describe ("sortList" <> show n <> "By") do
    specify "0-1 principle" do
      let inputs :: [] [Bool]
          inputs = replicateM n [False, True]
          sortFn = sortFnRouter n
      forM_ inputs \inp ->
        sortFn compare inp `shouldSatisfy` isSorted
