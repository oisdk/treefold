module Data.TreeFold.Strict where

import Data.List.NonEmpty (NonEmpty(..))

treeFold :: (a -> a -> a) -> a -> [a] -> a
treeFold _ z [] = z
treeFold f _ (x:xs) = treeFoldNonEmpty f (x :| xs)

pairFold :: (a -> a -> a) -> [a] -> [a]
pairFold f = go
  where
    go (x:y:rest) =
        let z = f x y
        in z `seq` (z : go rest)
    go xs = xs

treeFoldNonEmpty :: (a -> a -> a) -> NonEmpty a -> a
treeFoldNonEmpty f = go
  where
    go (x :| []) = x
    go (x :| y:rest) =
        let z = f x y
        in z `seq` go (z :| pairFold f rest)
