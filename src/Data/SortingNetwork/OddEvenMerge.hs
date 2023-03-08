{-# LANGUAGE TemplateHaskell #-}

module Data.SortingNetwork.OddEvenMerge where

import Data.SortingNetwork.Compares
import Data.SortingNetwork.TH

{-
  Batcher's odd–even mergesort
 -}

mkUnsafeSortListByFns batcher [2 .. 16]

mkSortTupByFns batcher [2 .. 16]

