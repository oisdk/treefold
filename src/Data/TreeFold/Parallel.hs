module Data.TreeFold.Parallel where

import Data.List.NonEmpty (NonEmpty(..))
import Control.Parallel
import qualified Data.TreeFold as Seq

data BinaryTree a = Leaf a | Branch (BinaryTree a) (BinaryTree a)

parEval :: Int -> (a -> a -> a) -> BinaryTree a -> a
parEval n f = go n where
  go 0 t = goSeq t
  go m (Branch xs ys) = x `par` y `par` f x y where
    x = go (m-1) xs
    y = go (m-1) ys
  go _ (Leaf x) = x
  goSeq (Branch xs ys) = x `seq` y `seq` f x y where
    x = goSeq xs
    y = goSeq ys
  goSeq (Leaf x) = x

treeFold :: (a -> a -> a) -> a -> [a] -> a
treeFold _ z [] = z
treeFold f _ (x:xs) = parEval 8 f (Seq.treeFoldNonEmpty Branch (Leaf x :| map Leaf xs))
