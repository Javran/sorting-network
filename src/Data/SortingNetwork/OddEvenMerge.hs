{-# LANGUAGE TemplateHaskell #-}

module Data.SortingNetwork.OddEvenMerge where

import Data.SortingNetwork.Compares (oddEvenMerge)
import Data.SortingNetwork.TH (
  mkSortTupByFns,
  mkUnsafeSortListByFns,
 )

{-
  Batcher's odd–even mergesort
 -}

mkUnsafeSortListByFns oddEvenMerge [2 .. 16]

mkSortTupByFns oddEvenMerge [2 .. 16]
