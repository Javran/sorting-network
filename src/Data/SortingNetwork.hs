{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Data.SortingNetwork where

import Data.SortingNetwork.Compares
import Data.SortingNetwork.TH

{-
  TODO: top level type signatures are suppressed for this module for now.
  done some quick searches, similar problems popped up like https://stackoverflow.com/q/37478037/315302
  but there doesn't seem to be a way to do this with qq.
 -}

mkSortListByFns batcher [2 .. 16]

mkSortTupByFns batcher [2 .. 16]
