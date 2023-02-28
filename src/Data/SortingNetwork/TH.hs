{-# LANGUAGE TemplateHaskell #-}

module Data.SortingNetwork.TH where

import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import Language.Haskell.TH

type PartQ = Exp -> Q Exp

gMkSortByFn :: (Int -> [(Int, Int)]) -> Int -> ([Pat] -> Pat) -> ([Exp] -> Exp) -> Q Exp
gMkSortByFn mkPairs n mkP mkE = do
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

mkSortByFnList, mkSortByFnTup :: (Int -> [(Int, Int)]) -> Int -> ExpQ
mkSortByFnList mkPairs n = gMkSortByFn mkPairs n ListP ListE
mkSortByFnTup mkPairs n = gMkSortByFn mkPairs n TupP (TupE . fmap Just)
