-- | This module provides parallel versions of the functions from "Data.TreeFold".
module Data.TreeFold.Parallel
  (treeFold
  ,treeFoldNonEmpty
  ,treeFoldMap
  ,treeFoldMapNonEmpty)
  where

import           Data.List.NonEmpty (NonEmpty (..))

import           Control.Parallel   (par, pseq)
import           GHC.Conc           (numCapabilities)

import qualified Data.TreeFold      as Lazy

-- $setup
-- >>> :{
-- data Tree a = Empty
--             | Leaf a
--             | Tree a :*: Tree a
--             deriving Show
-- :}

-- | A parallel version of 'Data.TreeFold.treeFold'.
--
-- >>> (treeFold (:*:) Empty . map Leaf) [1,2,3,4]
-- (Leaf 1 :*: Leaf 2) :*: (Leaf 3 :*: Leaf 4)
-- >>> (treeFold (:*:) Empty . map Leaf) [1,2,3,4,5]
-- ((Leaf 1 :*: Leaf 2) :*: (Leaf 3 :*: Leaf 4)) :*: Leaf 5
-- >>> treeFold (+) 0 (replicate 10 9.9)
-- 99.0
treeFold :: (a -> a -> a) -> a -> [a] -> a
treeFold f z xs =
    Lazy.treeFoldMap const (splitPar f) (const z) xs numCapabilities

splitPar :: (a -> a -> a) -> (Int -> a) -> (Int -> a) -> Int -> a
splitPar f = go
  where
    go l r 0 = f (l 0) (r 0)
    go l r n = lt `par` (rt `pseq` f lt rt)
      where
        lt = l m
        rt = r m
        m = n `div` 2

-- | A parallel version of 'Data.TreeFold.treeFoldNonEmpty'.
treeFoldNonEmpty :: (a -> a -> a) -> NonEmpty a -> a
treeFoldNonEmpty f xs =
    Lazy.treeFoldMapNonEmpty const (splitPar f) xs numCapabilities

-- | A parallel version of 'Data.TreeFold.treeFoldMap'.
--
-- >>> treeFoldMap Leaf (:*:) Empty [1,2,3,4]
-- (Leaf 1 :*: Leaf 2) :*: (Leaf 3 :*: Leaf 4)
treeFoldMap :: (b -> a) -> (a -> a -> a) -> a -> [b] -> a
treeFoldMap c f z xs =
    Lazy.treeFoldMap (const . c) (splitPar f) (const z) xs numCapabilities

-- | A parallel version of 'Data.TreeFold.treeFoldMapNonEmpty'.
treeFoldMapNonEmpty :: (b -> a) -> (a -> a -> a) -> NonEmpty b -> a
treeFoldMapNonEmpty c f xs =
    Lazy.treeFoldMapNonEmpty (const . c) (splitPar f) xs numCapabilities
