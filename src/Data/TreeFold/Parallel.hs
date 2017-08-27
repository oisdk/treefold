module Data.TreeFold.Parallel
  (treeFold
  ,treeFoldNonEmpty
  ,treeFoldMap
  ,treeFoldMapNonEmpty
  ,rseq
  ,rdeepseq
  ,rparWith)
  where

import           Control.Parallel.Strategies
import           Data.List.NonEmpty          (NonEmpty (..))

import           Data.TreeFold               (pairFold, pairFoldMap)

treeFold :: Strategy a -> Int -> (a -> a -> a) -> a -> [a] -> a
treeFold _ _ _ z []     = z
treeFold s n f _ (x:xs) = treeFoldNonEmpty s n f (x :| xs)

treeFoldNonEmpty :: Strategy a -> Int -> (a -> a -> a) -> NonEmpty a -> a
treeFoldNonEmpty s n f = go n
  where
    go _ (x :| [])  = x
    go 0 xs         = go n (xs `using` traverse s)
    go m (a :| b:l) = go (m-1) (f a b :| pairFold f l)

treeFoldMap :: Strategy a -> Int -> (b -> a) -> (a -> a -> a) -> a -> [b] -> a
treeFoldMap _ _ _ _ z [] = z
treeFoldMap s n c f _ (x:xs) = treeFoldMapNonEmpty s n c f (x :| xs)

treeFoldMapNonEmpty :: Strategy a -> Int -> (b -> a) -> (a -> a -> a) -> NonEmpty b -> a
treeFoldMapNonEmpty s n c f = once
  where
    once (x :| []) = c x
    once (a :| b:l) = go (n-1) (f (c a) (c b) :| pairFoldMap c f l)
    go _ (x :| [])  = x
    go 0 xs         = go n (xs `using` traverse s)
    go m (a :| b:l) = go (m-1) (f a b :| pairFold f l)
