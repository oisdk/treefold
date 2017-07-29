module Main (main) where

import           Criterion.Main

import           Data.TreeFold
import qualified Data.TreeFold.Strict as Strict

import           System.Random

import           Control.Monad

double :: IO Double
double = randomIO

atSize :: Int -> Benchmark
atSize n =
    env (replicateM n double) $
    \xs ->
         bgroup
             (show n)
             [ bench "TreeFold" $ whnf (treeFold (+) 0) xs
             , bench "Strict" $ whnf (Strict.treeFold (+) 0) xs
             , bench "sum" $ whnf sum xs ]


main :: IO ()
main = defaultMain [bgroup "sums" (map atSize [50000, 100000])]
