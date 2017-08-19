module Data.TreeFold.Parallel
  (treeFold
  ,treeFoldNonEmpty
  ,treeFoldMap
  ,treeFoldMapNonEmpty)
  where

import           Control.Parallel.Strategies
import           Data.List.NonEmpty          (NonEmpty (..))

import           Data.TreeFold               (pairFold, pairFoldMap)

treeFold :: Int -> (a -> a -> a) -> a -> [a] -> a
treeFold _ _ z []     = z
treeFold n f _ (x:xs) = treeFoldNonEmpty n f (x :| xs)

treeFoldNonEmpty :: Int -> (a -> a -> a) -> NonEmpty a -> a
treeFoldNonEmpty n f = go n
  where
    go _ (x :| [])  = x
    go 0 xs         = go n (xs `using` parTraversable rseq)
    go m (a :| b:l) = go (m-1) (f a b :| pairFold f l)

treeFoldMap :: Int -> (b -> a) -> (a -> a -> a) -> a -> [b] -> a
treeFoldMap _ _ _ z [] = z
treeFoldMap n c f _ (x:xs) = treeFoldMapNonEmpty n c f (x :| xs)

treeFoldMapNonEmpty :: Int -> (b -> a) -> (a -> a -> a) -> NonEmpty b -> a
treeFoldMapNonEmpty n c f = go
  where
    go (x :| []) = c x
    go (a :| b:l) = treeFoldNonEmpty n f (f (c a) (c b) :| pairFoldMap c f l)
