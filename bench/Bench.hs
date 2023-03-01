module Bench (
  main,
) where

import Control.Monad
import Criterion.Main
import Data.List
import Data.SortingNetwork
import Data.Word
import System.Random.MWC

type InputSet = [] [Word8]

mkInputs :: GenIO -> Int -> Int -> IO (InputSet, InputSet)
mkInputs g n count = do
  randomInps <- replicateM count do
    replicateM n do
      uniformRM (0, 0xFF) g
  dutchInps <- replicateM count do
    replicateM n do
      uniformRM (0, 2) g
  pure (randomInps, dutchInps)

main :: IO ()
main = do
  g <- createSystemRandom
  defaultMain do
    (n, sortByFn) <-
      [ (4, sortList4By)
        , (7, sortList7By)
        , (8, sortList8By)
        , (16, sortList16By)
        ]
    let mkBench inps =
            [ bench "Data.List.sort" $ nf (fmap sort) inps
            , bench ("sortList" <> show n <> "By") $ nf (fmap (sortByFn compare)) inps
            ]

    pure $
      env (mkInputs g n 256) $ \ ~(rInps, dInps) ->
        bgroup
          ("n = " <> show n)
          [ bgroup "random" $ mkBench rInps
          , bgroup "dutch" $ mkBench dInps
          , bgroup "reversed" $ mkBench [take n [i, i - 1 ..] | i <- [100 :: Word8 .. 81]]
          , bgroup "sorted" $ mkBench [take n [i ..] | i <- [1 :: Word8 .. 20]]
          ]
