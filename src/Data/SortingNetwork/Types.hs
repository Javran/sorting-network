module Data.SortingNetwork.Types (
  MkPairs,
) where

{-|
  A partial function that takes as argument number of elements
  and produces index pairs that we should compare to simulate a sorting network sequentially.
 -}
type MkPairs = Int -> Maybe [(Int, Int)]
