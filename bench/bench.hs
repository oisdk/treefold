module Main (main) where

import           Criterion.Main

import           Data.TreeFold
import qualified Data.TreeFold.Parallel as Parallel
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
             [ bench "TreeFold.Parallel" $ whnf (Parallel.treeFold (Parallel.rparWith Parallel.rseq) 6 (+) 0) xs
             , bench "TreeFold" $ whnf (treeFold (+) 0) xs
             , bench "TreeFold.Strict" $ whnf (Strict.treeFold (+) 0) xs
             , bench "sum" $ whnf sum xs
             , bench "foldl'" $ whnf (foldl' (+) 0) xs]


unionAtSize :: Int -> Benchmark
unionAtSize n =
    env (replicateM n integer) $
    \xs ->
         bgroup
             (show n)
             [ bench "TreeFold.Parallel" $
               whnf
                   (Parallel.treeFoldMap
                        (Parallel.rparWith Parallel.rseq)
                        8
                        Set.singleton
                        Set.union
                        Set.empty)
                   xs
             , bench "TreeFold.Strict" $
               whnf (Strict.treeFoldMap Set.singleton Set.union Set.empty) xs
             , bench "TreeFold" $
               whnf (treeFoldMap Set.singleton Set.union Set.empty) xs]



main :: IO ()
main =
    defaultMain
        [ bgroup "union" (map unionAtSize [1000000])
        , bgroup "sums" (map sumAtSize [1000000])]
