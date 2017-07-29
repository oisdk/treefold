-- | This module provides folds which try to combine elements in a balanced
-- way. These can be useful for constructing balanced binary trees, or more
-- stable summation.
--
-- Adapted from
-- <http://www.mail-archive.com/haskell@haskell.org/msg01788.html here>.
--
-- This is a strict version of "Data.TreeFold".
module Data.TreeFold.Strict where

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
-- This function is the strict version of 'Data.TreeFold.treeFold'.
treeFold :: (a -> a -> a) -> a -> [a] -> a
treeFold _ z [] = z
treeFold f _ (x:xs) = treeFoldNonEmpty f (x :| xs)

-- | Perform a tree fold after a map.
--
-- >>> treeFoldMap Leaf (:*:) Empty [1,2,3,4]
-- (Leaf 1 :*: Leaf 2) :*: (Leaf 3 :*: Leaf 4)
--
-- This function is a strict version of 'Data.TreeFold.treeFoldMap'.
treeFoldMap :: (b -> a) -> (a -> a -> a) -> a -> [b] -> a
treeFoldMap _ _ z [] = z
treeFoldMap c f _ (x:xs) = treeFoldMapNonEmpty c f (x :| xs)

-- | Apply a combining function to pairs in a list.
--
-- >>> pairFold (++) ["a","b","c","d","e"]
-- ["ab","cd","e"]
--
-- This function is a strict version of 'Data.TreeFold.pairFold'.
pairFold :: (a -> a -> a) -> [a] -> [a]
pairFold f = go
  where
    go (x:y:rest) =
        let z = f x y
        in z `seq` (z : go rest)
    go xs = xs

-- | Apply a combining function to pairs in a list, after a map.
--
-- >>> pairFoldMap (:[]) (++) "abcde"
-- ["ab","cd","e"]
--
-- This function is a strict version of 'Data.TreeFold.pairFoldMap'.
pairFoldMap :: (b -> a) -> (a -> a -> a) -> [b] -> [a]
pairFoldMap c f = go
  where
    go (x:y:rest) =
        let z = f (c x) (c y)
        in z `seq` (z : go rest)
    go [] = []
    go [x] =
        let z = c x
        in z `seq` [z]

-- | Perform a tree fold on a non empty input. This function is a strict version
-- of 'Data.TreeFold.treeFoldNonEmpty'.
treeFoldNonEmpty :: (a -> a -> a) -> NonEmpty a -> a
treeFoldNonEmpty f = go
  where
    go (x :| []) = x
    go (x :| y:rest) =
        let z = f x y
        in z `seq` go (z :| pairFold f rest)

-- | Perform a tree fold after a map on non-empty input. This function is a
-- strict version of 'Data.TreeFold.treeFoldMapNonEmpty'.
treeFoldMapNonEmpty :: (b -> a) -> (a -> a -> a) -> NonEmpty b -> a
treeFoldMapNonEmpty c f = go
  where
    go (x :| []) = c x
    go (a :| b:l) =
        let z = f (c a) (c b)
        in z `seq` treeFoldNonEmpty f (z :| pairFoldMap c f l)
