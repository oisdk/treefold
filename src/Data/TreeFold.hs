-- | This module provides folds which try to combine elements in a balanced
-- way. These can be useful for constructing balanced binary trees, or more
-- stable summation.
--
-- Adapted from
-- <http://www.mail-archive.com/haskell@haskell.org/msg01788.html here>.
--
-- For a strict version see "Data.TreeFold.Strict".
module Data.TreeFold where

import Data.List.NonEmpty (NonEmpty(..))

-- $setup
-- >>> :{
-- data Tree a = Empty
--             | Leaf a
--             | Tree a :*: Tree a
--             deriving Show
-- :}

-- | Reduces a list of values using a binary operation, trying as much as
-- possible to balance. For instance, given the above binary tree type, the
-- operation:
--
-- >>> (treeFold (:*:) Empty . map Leaf) [1,2,3,4]
-- (Leaf 1 :*: Leaf 2) :*: (Leaf 3 :*: Leaf 4)
--
-- Will construct a balanced binary tree, whereas the equivalent using a normal
-- fold:
--
-- >>> foldr ((:*:) . Leaf) Empty [1,2,3,4]
-- Leaf 1 :*: (Leaf 2 :*: (Leaf 3 :*: (Leaf 4 :*: Empty)))
--
-- Will construct a right-heavy tree.
--
-- >>> (treeFold (:*:) Empty . map Leaf) [1,2,3,4,5]
-- ((Leaf 1 :*: Leaf 2) :*: (Leaf 3 :*: Leaf 4)) :*: Leaf 5
--
-- Other uses for this function include more stable floating-point
-- summation. The following sum algorithm:
--
-- >>> treeFold (+) 0 (replicate 10 9.9)
-- 99.0
--
-- Will have O(log n) error growth for summing n numbers, in comparison to
-- O(n) for:
--
-- >>> sum (replicate 10 9.9)
-- 99.00000000000001
--
-- For a strict version of this function see 'Data.TreeFold.Strict.treeFold'.
treeFold :: (a -> a -> a) -> a -> [a] -> a
treeFold _ z [] = z
treeFold f _ (x:xs) = treeFoldNonEmpty f (x :| xs)

-- | Perform a tree fold after a map.
--
-- >>> treeFoldMap Leaf (:*:) Empty [1,2,3,4]
-- (Leaf 1 :*: Leaf 2) :*: (Leaf 3 :*: Leaf 4)
--
-- For a strict version of this function see 'Data.TreeFold.Strict.treeFoldMap'.
treeFoldMap :: (b -> a) -> (a -> a -> a) -> a -> [b] -> a
treeFoldMap _ _ z [] = z
treeFoldMap c f _ (x:xs) = treeFoldMapNonEmpty c f (x :| xs)

-- | Perform a tree fold on a non empty input. For a strict version of this
-- function see 'Data.TreeFold.Strict.treeFoldNonEmpty'.
treeFoldNonEmpty :: (a -> a -> a) -> NonEmpty a -> a
treeFoldNonEmpty f = go
  where
    go (x :| []) = x
    go (a :| b:l) = go (f a b :| pairFold l)
    pairFold (x:y:rest) = f x y : pairFold rest
    pairFold xs = xs

-- | Perform a tree fold after a map on non-empty input. For a strict version of
-- this function see 'Data.TreeFold.Strict.treeFoldMapNonEmpty'.
treeFoldMapNonEmpty :: (b -> a) -> (a -> a -> a) -> NonEmpty b -> a
treeFoldMapNonEmpty c f = go
  where
    go (x :| []) = c x
    go (a :| b:l) = treeFoldNonEmpty f (f (c a) (c b) :| pairFoldMap l)
    pairFoldMap (x:y:rest) = f (c x) (c y) : pairFoldMap rest
    pairFoldMap [] = []
    pairFoldMap [x] = [c x]
