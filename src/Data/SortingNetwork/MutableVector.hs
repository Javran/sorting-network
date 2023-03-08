{-|
  Functions that sorts mutable vectors using sorting network.
 -}

module Data.SortingNetwork.MutableVector (
  unsafeSortBy,
  maySortBy,
) where

import Control.Monad
import Control.Monad.Primitive
import Data.SortingNetwork.Types
import qualified Data.Vector.Generic.Mutable as VM

{- TODO: test coverage -}
{-|
  Sorts a mutable vector by applying compare-and-swap operations generated through 'MkPairs'.

  Raises error if vector size cannot be handled by the sorting network.
 -}
unsafeSortBy :: (PrimMonad m, VM.MVector v e) => MkPairs -> (e -> e -> Ordering) -> v (PrimState m) e -> m ()
unsafeSortBy mkPairs cmp v = case mkPairs n of
  Just pairs ->
    forM_ pairs \(i, j) -> do
      vi <- VM.unsafeRead v i
      vj <- VM.unsafeRead v j
      when (cmp vi vj == GT) do
        VM.unsafeSwap v i j
  Nothing -> error $ "MkPairs returned Nothing on length " <> show n
  where
    n = VM.length v

{-|
  Safe version of 'unsafeSortBy'.

  This function either returns input vector reference upon successful sorting
  or 'Nothing' if the vector size cannot be handled.
 -}
maySortBy :: (PrimMonad m, VM.MVector v e) => MkPairs -> (e -> e -> Ordering) -> v (PrimState m) e -> m (Maybe (v (PrimState m) e))
maySortBy mkPairs cmp v = case mkPairs (VM.length v) of
  Just _ -> Just v <$ unsafeSortBy mkPairs cmp v
  Nothing -> pure Nothing
