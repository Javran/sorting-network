{-# LANGUAGE TemplateHaskell #-}

module Data.SortingNetwork.OptimalSize where

import Data.SortingNetwork.Compares (optimal)
import Data.SortingNetwork.TH (
  mkSortTupByFns,
  mkUnsafeSortListByFns,
 )

{-
  Sorting network optimal in terms of size
  (minimum number of compares)
 -}

mkUnsafeSortListByFns optimal [2 .. 16]

mkSortTupByFns optimal [2 .. 16]
