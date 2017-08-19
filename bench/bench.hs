module Main (main) where

import           Criterion.Main

import           Data.TreeFold
import qualified Data.TreeFold.Parallel as Parallel
import qualified Data.TreeFold.Strict   as Strict

import           System.Random

import           Control.Monad
import           Data.Foldable

double :: IO Double
double = randomIO

atSize :: Int -> Benchmark
atSize n =
    env (replicateM n double) $
    \xs ->
         bgroup
             (show n)
             [ bench "TreeFold.Parallel" $ whnf (Parallel.treeFold 6 (+) 0) xs
             , bench "TreeFold" $ whnf (treeFold (+) 0) xs
             , bench "TreeFold.Strict" $ whnf (Strict.treeFold (+) 0) xs
             , bench "sum" $ whnf sum xs
             , bench "foldl'" $ whnf (foldl' (+) 0) xs]


main :: IO ()
main = defaultMain [bgroup "sums" (map atSize [1000000])]
