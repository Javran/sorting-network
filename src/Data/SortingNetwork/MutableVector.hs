module Data.SortingNetwork.MutableVector (
  unsafeSortBy,
) where

import Control.Monad
import Control.Monad.Primitive
import Data.SortingNetwork.Types
import qualified Data.Vector.Generic.Mutable as VM

{- TODO: test coverage -}
unsafeSortBy :: (PrimMonad m, VM.MVector v e) => MkPairs -> (e -> e -> Ordering) -> v (PrimState m) e -> m ()
unsafeSortBy mkPairs cmp v =
  forM_ (mkPairs $ VM.length v) \(i, j) -> do
    vi <- VM.unsafeRead v i
    vj <- VM.unsafeRead v j
    when (cmp vi vj == GT) do
      VM.unsafeSwap v i j
