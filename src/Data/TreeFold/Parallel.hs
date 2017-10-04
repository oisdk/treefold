-- | This is a parallel version of "Data.TreeFold": all the functions here take
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
-- one.
module Data.TreeFold.Parallel
  (treeFold
  ,treeFoldNonEmpty
  ,treeFoldMap
  ,treeFoldMapNonEmpty
  ,rpar)
  where

import           Control.Parallel.Strategies (Strategy, rpar, using,
                                              withStrategy)
import           Data.List.NonEmpty          (NonEmpty (..))

import qualified Data.TreeFold               as Lazy

-- | A parallel version of 'Data.TreeFold.treeFold'.
treeFold :: Strategy a -> Int -> (a -> a -> a) -> a -> [a] -> a
treeFold _ _ _ z []     = z
treeFold s n f _ (x:xs) = treeFoldNonEmpty s n f (x :| xs)

-- | A parallel version of 'Data.TreeFold.treeFoldNonEmpty'.
treeFoldNonEmpty :: Strategy a -> Int -> (a -> a -> a) -> NonEmpty a -> a
treeFoldNonEmpty s n f
  | n <= 0 = withStrategy s . Lazy.treeFoldNonEmpty f
  | otherwise = go n
  where
    go _ (x :| [])  = x
    go 0 xs         = go n (xs `using` traverse s)
    go m (a :| b:l) = go (m - 1) (f a b :| pairFold l)
    pairFold (a:b:rest) = f a b : pairFold rest
    pairFold xs = xs

-- | A parallel version of 'Data.TreeFold.treeFoldMap'.
treeFoldMap :: Strategy a -> Int -> (b -> a) -> (a -> a -> a) -> a -> [b] -> a
treeFoldMap _ _ _ _ z []     = z
treeFoldMap s n c f _ (x:xs) = treeFoldMapNonEmpty s n c f (x :| xs)

-- | A parallel version of 'Data.TreeFold.treeFoldMapNonEmpty'.
treeFoldMapNonEmpty :: Strategy a -> Int -> (b -> a) -> (a -> a -> a) -> NonEmpty b -> a
treeFoldMapNonEmpty s n c f
  | n <= 0 = withStrategy s . Lazy.treeFoldMapNonEmpty c f
  | otherwise = once
  where
    once (x :| [])  = c x
    once (a :| b:l) = go (n - 1) (f (c a) (c b) :| pairFoldMap l)
    go _ (x :| [])  = x
    go 0 xs         = go n (xs `using` traverse s)
    go m (a :| b:l) = go (m - 1) (f a b :| pairFold l)
    pairFoldMap (x:y:rest) = f (c x) (c y) : pairFoldMap rest
    pairFoldMap [x] = [c x]
    pairFoldMap [] = []
    pairFold (a:b:rest) = f a b : pairFold rest
    pairFold xs = xs


