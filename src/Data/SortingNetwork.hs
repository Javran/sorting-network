{-# LANGUAGE TemplateHaskell #-}

module Data.SortingNetwork where

import Data.SortingNetwork.Compares
import Data.SortingNetwork.TH

mkSortListByFns batcher [2 .. 16]

mkSortTupByFns batcher [2 .. 16]
