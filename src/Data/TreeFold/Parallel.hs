module Data.TreeFold.Parallel
  (treeFold
  ,treeFoldNonEmpty
  ,treeFoldMap
  ,treeFoldMapNonEmpty
  ,rseq
  ,rdeepseq
  ,rparWith
  ,parSeq)
  where

import           Control.Parallel.Strategies
import           Data.List.NonEmpty          (NonEmpty (..))

import qualified Data.TreeFold.Strict as Strict

import           Control.Arrow               (first)
import           Data.List                   (splitAt, unfoldr)

treeFold :: Strategy a -> Int -> (a -> a -> a) -> a -> [a] -> a
treeFold _ _ _ z []     = z
treeFold s n f _ (x:xs) = treeFoldNonEmpty s n f (x :| xs)

treeFoldNonEmpty :: Strategy a -> Int -> (a -> a -> a) -> NonEmpty a -> a
treeFoldNonEmpty s n f =
    Strict.treeFoldNonEmpty f .
    runEval . traverse (s . Strict.treeFoldNonEmpty f) . chunk (2 ^ n)

treeFoldMap :: Strategy a -> Int -> (b -> a) -> (a -> a -> a) -> a -> [b] -> a
treeFoldMap _ _ _ _ z []     = z
treeFoldMap s n c f _ (x:xs) = treeFoldMapNonEmpty s n c f (x :| xs)

treeFoldMapNonEmpty :: Strategy a -> Int -> (b -> a) -> (a -> a -> a) -> NonEmpty b -> a
treeFoldMapNonEmpty s n c f =
    Strict.treeFoldNonEmpty f .
    runEval . traverse (s . Strict.treeFoldMapNonEmpty c f) . chunk (2 ^ n)

chunk :: Int -> NonEmpty a -> NonEmpty (NonEmpty a)
chunk n = toNonEmpty . unfoldr f . toList where
  f []     = Nothing
  f (x:xs) = (Just . first (x:|) . splitAt (n-1)) xs
  toNonEmpty (x:xs) = x :| xs
  toNonEmpty [] = error "Data.TreeFold.Parallel.chunk: empty list"
  toList (x :| xs) = x:xs

parSeq :: Strategy a
parSeq = rparWith rseq
{-# INLINE parSeq #-}
