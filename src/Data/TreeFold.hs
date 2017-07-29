module Data.TreeFold where

import Data.List.NonEmpty (NonEmpty(..))

treeFold :: (a -> a -> a) -> a -> [a] -> a
treeFold _ z [] = z
treeFold f _ (x:xs) = treeFoldNonEmpty f (x :| xs)

pairFold :: (a -> a -> a) -> [a] -> [a]
pairFold f = go where
  go (x : y : rest) = f x y : go rest
  go xs = xs

treeFoldNonEmpty :: (a -> a -> a) -> NonEmpty a -> a
treeFoldNonEmpty f = go where
  go (x :| []) = x
  go (a :| b : l) = go (f a b :| pairFold f l)
