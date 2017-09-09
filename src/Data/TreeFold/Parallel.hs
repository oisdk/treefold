-- | This module provides folds which try to combine elements in a balanced
-- way. These can be useful for constructing balanced binary trees, or more
-- stable summation.
--
-- Adapted from
-- <http://www.mail-archive.com/haskell@haskell.org/msg01788.html here>.
--
-- This is a parallel version of "Data.TreeFold": all the functions here take
-- a 'Control.Parallel.Strategies.Strategy' which specifies how to evaluate the
-- fold in parallel, and a number for the size of the chunks evaluated in
-- parallel. The number works like this. If you imagine the treefold as being
-- executed on a list of 32 elements like so:
--
-- > 5_______________________________________________________________
-- >                                 /\
-- >                                /  \
-- >                               /    \
-- >                              /      \
-- >                             /        \
-- >                            /          \
-- >                           /            \
-- >                          /              \
-- >                         /                \
-- >                        /                  \
-- >                       /                    \
-- >                      /                      \
-- >                     /                        \
-- >                    /                          \
-- >                   /                            \
-- > 4________________/______________________________\_______________
-- >                 /\                              /\
-- >                /  \                            /  \
-- >               /    \                          /    \
-- >              /      \                        /      \
-- >             /        \                      /        \
-- >            /          \                    /          \
-- >           /            \                  /            \
-- > 3________/______________\________________/______________\_______
-- >         /\              /\              /\              /\
-- >        /  \            /  \            /  \            /  \
-- >       /    \          /    \          /    \          /    \
-- > 2____/______\________/______\________/______\________/______\___
-- >     /\      /\      /\      /\      /\      /\      /\      /\
-- > 1__/__\____/__\____/__\____/__\____/__\____/__\____/__\____/__\_
-- > 0_/\__/\__/\__/\__/\__/\__/\__/\__/\__/\__/\__/\__/\__/\__/\__/\
--
-- Depending on what number you supply, the tree fold will run lazily until it
-- gets to the corresponding point in the diagram above. Then, it will evaluate
-- the subtrees in parallel according to the strategy supplied. It will then
-- start again on the list of evaluated values, continuing until it gets to only
-- one. Supplying 0 is the same as running a strict fold.
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

import           Data.TreeFold               (pairFold, pairFoldMap)
import qualified Data.TreeFold.Strict        as Strict

-- | Reduce a list of values using a binary operation, trying as much as
-- possible to balance. The reduction is carried out in parallel according to
-- the strategy supplied, broken up into subtrees with a depth according to the
-- value supplied. This is a parallel version of 'Data.TreeFold.treeFold'.
treeFold :: Strategy a -> Int -> (a -> a -> a) -> a -> [a] -> a
treeFold _ _ _ z []     = z
treeFold s n f _ (x:xs) = treeFoldNonEmpty s n f (x :| xs)

-- | Reduce a non-empty list of values using a binary operation, trying as much
-- as possible to balance. The reduction is carried out in parallel according to
-- the strategy supplied, broken up into subtrees with a depth according to the
-- value supplied. This is a parallel version of
-- 'Data.TreeFold.treeFoldNonEmpty'.
treeFoldNonEmpty :: Strategy a -> Int -> (a -> a -> a) -> NonEmpty a -> a
treeFoldNonEmpty _ n f | n <= 0 = Strict.treeFoldNonEmpty f
treeFoldNonEmpty s n f = go n
  where
    go _ (x :| [])  = x
    go 0 xs         = go n (xs `using` traverse s)
    go m (a :| b:l) = go (m-1) (f a b :| pairFold f l)

-- | Reduce a list of values using a binary operation, after applying a map.
-- This function is a parallel version of 'Data.TreeFold.treeFoldMap'.
treeFoldMap :: Strategy a -> Int -> (b -> a) -> (a -> a -> a) -> a -> [b] -> a
treeFoldMap _ _ _ _ z []     = z
treeFoldMap s n c f _ (x:xs) = treeFoldMapNonEmpty s n c f (x :| xs)

-- | Reduce a non-empty list of values using a binary operation, after applying
-- a map. This function is a parallel version of
-- 'Data.TreeFold.treeFoldMapNonEmpty'.
treeFoldMapNonEmpty :: Strategy a -> Int -> (b -> a) -> (a -> a -> a) -> NonEmpty b -> a
treeFoldMapNonEmpty _ n c f | n <= 0 = Strict.treeFoldMapNonEmpty c f
treeFoldMapNonEmpty s n c f = once
  where
    once (x :| [])  = c x
    once (a :| b:l) = go (n-1) (f (c a) (c b) :| pairFoldMap c f l)
    go _ (x :| [])  = x
    go 0 xs         = go n (xs `using` traverse s)
    go m (a :| b:l) = go (m-1) (f a b :| pairFold f l)

-- | Evaluate to weak head normal form, in parallel.
parSeq :: Strategy a
parSeq = rparWith rseq
{-# INLINE parSeq #-}
