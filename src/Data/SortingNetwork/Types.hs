module Data.SortingNetwork.Types (
  MkPairs,
) where

{-|
  A function that takes as argument number of elements
  and produces zero-based index pairs that we should compare
  to simulate a sorting network sequentially.
  Should return 'Nothing' if a network of the input size is not supported.
 -}
type MkPairs = Int -> Maybe [(Int, Int)]
