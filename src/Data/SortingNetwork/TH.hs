{-# LANGUAGE TemplateHaskell #-}

module Data.SortingNetwork.TH (
  gMkSortBy,
  mkUnsafeSortListBy,
  mkSortTupBy,
  mkUnsafeSortListByFns,
  mkSortTupByFns,
) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Semigroup
import Data.SortingNetwork.Types
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import Language.Haskell.TH

-- | A monadic partial expression pending inner body.
type PartQ = Exp -> Q Exp

{- TODO: we should probably have functions for unboxed tuples -}

{- |
  @gMkSortBy mkPairs n mkP mkE@ generates a function that sorts elements using sorting network.

  @mkP :: [Pat] -> Pat@ and @mkE :: [Exp] -> Exp@ deals with unpacking input value and packing final results
  respectively. This generalization allows us to deal with lists, tuples, and unboxed-tuples all at once.
-}
gMkSortBy :: MkPairs -> Int -> ([Pat] -> Pat) -> ([Exp] -> Exp) -> Q Exp
gMkSortBy mkPairs n mkP mkE = do
  -- cmp :: a -> a -> Ordering
  cmp <- newName "cmp"

  swapper <- newName "sw"
  swapperVal <- [|\u v f -> if $(varE cmp) u v == GT then f v u else f u v|]

  ns0 <- replicateM n $ newName "v"
  let -- let sw = ... in <???>
      step0 :: PartQ
      step0 bd = [|let $(varP swapper) = $(pure swapperVal) in $(pure bd)|]

  pairs <- case mkPairs n of
    Just ps -> pure ps
    Nothing -> fail $ "MkPairs returned Nothing on length " <> show n

  (mkBody :: PartQ, ns :: [Name]) <- do
    nv <- liftIO $ V.unsafeThaw (V.fromList ns0)
    e <-
      foldM
        ( \(mk :: PartQ) (i, j) -> do
            iOld <- liftIO $ VM.unsafeRead nv i
            jOld <- liftIO $ VM.unsafeRead nv j
            iNew <- newName "v"
            jNew <- newName "v"
            liftIO do
              VM.unsafeWrite nv i iNew
              VM.unsafeWrite nv j jNew
            pure \(hole :: Exp) ->
              mk
                =<< [|
                  $(varE swapper)
                    $(varE iOld)
                    $(varE jOld)
                    (\ $(varP iNew) $(varP jNew) -> $(pure hole))
                  |]
        )
        step0
        pairs
    nvFin <- liftIO $ V.unsafeFreeze nv
    pure (e, V.toList nvFin)

  [|
    \ $(varP cmp)
      $(pure $ mkP $ VarP <$> ns0) ->
        $(mkBody $ mkE $ VarE <$> ns)
    |]

{- |
  @mkUnsafeSortListBy mkPairs n@ generates an expression of type @(a -> a -> Ordering) -> [a] -> [a]@.

  Note that resulting function is partial and requires input list to contain exactly @n@ elements.
-}
mkUnsafeSortListBy :: MkPairs -> Int -> ExpQ
mkUnsafeSortListBy mkPairs n = gMkSortBy mkPairs n ListP ListE

{- |
  @mkSortTupBy mkPairs n@ generates an expression of type @(a -> a -> Ordering) -> (a, a, ...) -> (a, a, ...)@.

  Where the input and output tuple @(a, a, ...)@ contains @n@ elements.
-}
mkSortTupBy :: MkPairs -> Int -> ExpQ
mkSortTupBy mkPairs n = gMkSortBy mkPairs n TupP (TupE . fmap Just)

{-
  Note: I'm not sure if there are more convenient ways to have type signatures with qq,
  so current approach is just to build it from plain constructors.

  Might be related: https://stackoverflow.com/q/37478037/315302
 -}

mkUnsafeSortListByFns, mkSortTupByFns :: MkPairs -> [Int] -> Q [Dec]
mkUnsafeSortListByFns mkPairs ns =
  concat <$> forM ns \n -> do
    let defN :: Name
        defN = mkName $ "unsafeSortList" <> show n <> "By"
    bd <- mkUnsafeSortListBy mkPairs n
    sequence
      [ sigD defN [t|forall a. (a -> a -> Ordering) -> [a] -> [a]|]
      , funD defN [clause [] (normalB $ pure bd) []]
      ]
mkSortTupByFns mkPairs ns =
  concat <$> forM ns \n -> do
    let defN = mkName $ "sortTup" <> show n <> "By"
    bd <- mkSortTupBy mkPairs n
    a <- newName "a"
    tupTy <- do
      constr <- tupleT n
      pure $ appEndo (stimes n (Endo (\t -> AppT t (VarT a)))) constr
    sequence
      [ sigD defN [t|($(varT a) -> $(varT a) -> Ordering) -> $(pure tupTy) -> $(pure tupTy)|]
      , funD defN [clause [] (normalB $ pure bd) []]
      ]
