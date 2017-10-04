-- | This module provides strict versions of the functions from "Data.TreeFold".
module Data.TreeFold.Strict where

import Data.List.NonEmpty (NonEmpty(..))

-- $setup
-- >>> :{
-- data Tree a = Empty
--             | Leaf a
--             | Tree a :*: Tree a
--             deriving Show
-- :}

-- | A strict version of 'Data.TreeFold.treeFold'.
--
-- >>> (treeFold (:*:) Empty . map Leaf) [1,2,3,4]
-- (Leaf 1 :*: Leaf 2) :*: (Leaf 3 :*: Leaf 4)
-- >>> (treeFold (:*:) Empty . map Leaf) [1,2,3,4,5]
-- ((Leaf 1 :*: Leaf 2) :*: (Leaf 3 :*: Leaf 4)) :*: Leaf 5
-- >>> treeFold (+) 0 (replicate 10 9.9)
-- 99.0
treeFold :: (a -> a -> a) -> a -> [a] -> a
treeFold _ z [] = z
treeFold f _ (x:xs) = treeFoldNonEmpty f (x :| xs)

-- | A strict version of 'Data.TreeFold.treeFoldMap'.
--
-- >>> treeFoldMap Leaf (:*:) Empty [1,2,3,4]
-- (Leaf 1 :*: Leaf 2) :*: (Leaf 3 :*: Leaf 4)
treeFoldMap :: (b -> a) -> (a -> a -> a) -> a -> [b] -> a
treeFoldMap _ _ z [] = z
treeFoldMap c f _ (x:xs) = treeFoldMapNonEmpty c f (x :| xs)

-- | A strict version of 'Data.TreeFold.treeFoldNonEmpty'.
treeFoldNonEmpty :: (a -> a -> a) -> NonEmpty a -> a
treeFoldNonEmpty f = go
  where
    go (x :| []) = x
    go (x :| y:rest) =
        let z = f x y
        in z `seq` go (z :| pairFold rest)
    pairFold (x:y:rest) =
        let z = f x y
        in z `seq` (z : pairFold rest)
    pairFold xs = xs

-- | A strict version of 'Data.TreeFold.treeFoldMapNonEmpty'.
treeFoldMapNonEmpty :: (b -> a) -> (a -> a -> a) -> NonEmpty b -> a
treeFoldMapNonEmpty c f = go
  where
    go (x :| []) = c x
    go (a :| b:l) =
        let z = f (c a) (c b)
        in z `seq` treeFoldNonEmpty f (z :| pairFoldMap l)
    pairFoldMap (x:y:rest) =
        let z = f (c x) (c y)
        in z `seq` (z : pairFoldMap rest)
    pairFoldMap [] = []
    pairFoldMap [x] =
        let z = c x
        in z `seq` [z]
