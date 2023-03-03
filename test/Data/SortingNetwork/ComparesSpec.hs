module Data.SortingNetwork.ComparesSpec (
  spec,
) where

import Control.Monad
import Control.Monad.ST
import Data.SortingNetwork.Compares
import Data.SortingNetwork.MutableVector (unsafeSortBy)
import Data.SortingNetwork.Types
import qualified Data.Vector as V
import Test.Hspec

sortViaVector :: Ord a => MkPairs -> [a] -> [a]
sortViaVector mkPairs xs = runST do
  v <- V.unsafeThaw (V.fromList xs)
  unsafeSortBy mkPairs compare v
  vFin <- V.unsafeFreeze v
  pure $ V.toList vFin

isSorted :: Ord a => [a] -> Bool
isSorted xs = and (zipWith (<=) xs (tail xs))

{-
  TODO: this is a bit too slow to my liking - maybe we should prefer end-to-end testing
  as there are way less vector allocation involved.
 -}
spec :: Spec
spec = forM_
  [ ("optimal", optimal)
  , ("batcher", batcher)
  ]
  \(tag, mkPairs) ->
    describe tag do
      describe "0-1 principle" do
        forM_ [2 .. 16] \n -> specify ("n = " <> show n) do
          let inputs :: [] [Bool]
              inputs = replicateM n [False, True]
          forM_ inputs \inp ->
            sortViaVector mkPairs inp `shouldSatisfy` isSorted
