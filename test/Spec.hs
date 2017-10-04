{-# LANGUAGE TemplateHaskell      #-}

import           Test.DocTest

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Data.TreeFold
import qualified Data.TreeFold.Strict as Strict
import qualified Data.TreeFold.Parallel as Parallel

import           Data.List.NonEmpty (NonEmpty(..))

data Tree a = Leaf a
            | Tree a :^: Tree a
            deriving (Show, Eq, Ord)

prop_equivToLazy :: Property
prop_equivToLazy =
    property $
    do xs <- forAll $ Gen.nonEmpty (Range.linear 1 100) Gen.alpha
       let lazy = treeFoldMapNonEmpty Leaf (:^:) xs
       lazy === Strict.treeFoldMapNonEmpty Leaf (:^:) xs
       lazy ===
           Parallel.treeFoldMapNonEmpty
               Leaf
               (:^:)
               xs

prop_lazyEmpty :: Property
prop_lazyEmpty =
    property $
    do treeFold (error "treeFold: not lazy enough") () [] === ()
       Strict.treeFold (error "Strict.treeFold: not lazy enough") () [] === ()
       Parallel.treeFold
           (error "Parallel.treeFold: not lazy enough")
           ()
           [] ===
           ()

prop_equivNonEmpty :: Property
prop_equivNonEmpty =
    property $
    do (x :| xs) <-
           forAll $
           Gen.nonEmpty (Range.linear 1 100) (Gen.int (Range.linear 1 100))
       treeFold (+) (error "treeFold: not lazy enough") (x : xs) ===
           treeFoldNonEmpty (+) (x :| xs)
       Strict.treeFold (+) (error "treeFold: not lazy enough") (x : xs) ===
           Strict.treeFoldNonEmpty (+) (x :| xs)
       Parallel.treeFold
           (+)
           (error "Parallel.treeFold: not lazy enough")
           (x : xs) ===
           Parallel.treeFoldNonEmpty (+) (x :| xs)



main :: IO ()
main = do
    _ <- checkParallel $$(discover)
    doctest ["-isrc", "src/"]
