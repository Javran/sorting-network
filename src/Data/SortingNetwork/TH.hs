{-# LANGUAGE TemplateHaskell #-}

module Data.SortingNetwork.TH (
  gMkSortBy,
  mkSortListBy,
  mkSortTupBy,
  mkSortListByFns,
  mkSortTupByFns,
) where

import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import Language.Haskell.TH

type MkPairs = Int -> [(Int, Int)]
type PartQ = Exp -> Q Exp

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
        (mkPairs n)
    nvFin <- liftIO $ V.unsafeFreeze nv
    pure (e, V.toList nvFin)

  [|
    \ $(varP cmp)
      $(pure $ mkP $ VarP <$> ns0) ->
        $(mkBody $ mkE $ VarE <$> ns)
    |]

mkSortListBy, mkSortTupBy :: MkPairs -> Int -> ExpQ
mkSortListBy mkPairs n = gMkSortBy mkPairs n ListP ListE
mkSortTupBy mkPairs n = gMkSortBy mkPairs n TupP (TupE . fmap Just)

mkSortListByFns, mkSortTupByFns :: MkPairs -> [Int] -> Q [Dec]
mkSortListByFns mkPairs ns =
  concat <$> forM ns \n -> do
    let defN :: Name
        defN = mkName $ "sortList" <> show n <> "By"
    bd <- mkSortListBy mkPairs n
    [d|$(varP defN) = $(pure bd)|]
mkSortTupByFns mkPairs ns =
  concat <$> forM ns \n -> do
    let defN = mkName $ "sortTup" <> show n <> "By"
    bd <- mkSortTupBy mkPairs n
    [d|$(varP defN) = $(pure bd)|]
