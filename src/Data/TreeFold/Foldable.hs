-- | This module provides folds which try to combine elements in a balanced
-- way. These can be useful for constructing balanced binary trees, or more
-- stable summation.
--
-- Adapted from
-- <http://www.mail-archive.com/haskell@haskell.org/msg01788.html here>.
--
-- This is a version of "Data.TreeFold" which works on any 'Foldable' container,
-- rather than just lists.
module Data.TreeFold.Foldable
  (treeFold
  ,treeFoldMap)
  where

import           Data.Foldable
import qualified Data.TreeFold      as List
import           Data.List.NonEmpty (NonEmpty (..))

-- | A version of 'Data.TreeFold.treeFold' which works on any 'Foldable'.
treeFold :: Foldable f => (a -> a -> a) -> a -> f a -> a
treeFold = treeFoldMap id

-- | A version of 'Data.TreeFold.treeFoldMap' which works on any 'Foldable'.
treeFoldMap :: Foldable f => (b -> a) -> (a -> a -> a) -> a -> f b -> a
treeFoldMap g c e xs =
    case ys of
        []     -> e
        (z:zs) -> List.treeFoldNonEmpty c (z :| zs)
  where
    ys = foldr (f . g) toList xs Nothing
    f y a Nothing  = a (Just y)
    f y a (Just x) = c x y : a Nothing
