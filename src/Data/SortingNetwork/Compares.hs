module Data.SortingNetwork.Compares (
  batcher,
) where

import Control.Monad
import Data.Bits

{-
  Ref: https://en.wikipedia.org/wiki/Batcher_odd%E2%80%93even_mergesort
 -}
batcher :: Int -> [] (Int, Int)
batcher n = do
  -- INVARIANT: p == shiftL 1 (pw - 1)
  (p, pw) <- zip (takeWhile (< n) $ iterate (* 2) 1) [1 ..]
  k <- takeWhile (>= 1) $ iterate (\v -> shiftR v 1) p
  j <- takeWhile (<= n - 1 - k) $ iterate (+ 2 * k) (rem k p)
  i <- [0 .. k - 1]
  guard $ shiftR (i + j) pw == shiftR (i + j + k) pw
  {-
    Index could get out of bound without this check
    when n is not a power of 2.

    I'm not sure about then correctness when n > 16,
    but our tests can verify its correctness for n = [2.. 16]
    based on 0-1 principle.
   -}
  guard $ i + j + k < n
  pure (i + j, i + j + k)
