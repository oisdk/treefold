module Main (main) where

import           Criterion.Main

import           Data.TreeFold
import qualified Data.TreeFold.Parallel as Parallel
import qualified Data.TreeFold.Foldable as Foldable
import qualified Data.TreeFold.Strict   as Strict

import           System.Random

import           Control.Monad
import           Data.Foldable

import qualified Data.IntSet as Set

double :: IO Double
double = randomIO

integer :: IO Int
integer = randomIO

sumAtSize :: Int -> Benchmark
sumAtSize n =
    env (replicateM n double) $
    \xs ->
         bgroup
             (show n)
             [ bench "Parallel" $ whnf (Parallel.treeFold Parallel.rpar 6 (+) 0) xs
             , bench "Lazy" $ whnf (treeFold (+) 0) xs
             , bench "Foldable" $ whnf (Foldable.treeFold (+) 0) xs
             , bench "Strict" $ whnf (Strict.treeFold (+) 0) xs
             , bench "sum" $ whnf sum xs
             , bench "foldl'" $ whnf (foldl' (+) 0) xs]


unionAtSize :: Int -> Benchmark
unionAtSize n =
    env (replicateM n integer) $
    \xs ->
         bgroup
             (show n)
             [ bench "Parallel" $ whnf (Parallel.treeFoldMap Parallel.rpar 10 Set.singleton Set.union Set.empty) xs
             , bench "Strict" $ whnf (Strict.treeFoldMap Set.singleton Set.union Set.empty) xs
             , bench "Lazy" $ whnf (treeFoldMap Set.singleton Set.union Set.empty) xs
             , bench "Foldable" $ whnf (Foldable.treeFoldMap Set.singleton Set.union Set.empty) xs
             , bench "foldl'" $ whnf (foldl' (flip Set.insert) Set.empty) xs
             , bench "foldMap" $ whnf (foldMap Set.singleton) xs]



main :: IO ()
main =
    defaultMain
        [ bgroup "union" (map unionAtSize [100000])
        , bgroup "sums" (map sumAtSize [100000])
        ]
