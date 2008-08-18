module Test.QuickCheck.Instances.List
       (anyList,nonEmpty
       ,infiniteList
       ,increasing,nondecreasing
       ,increasingInf
       ) where

import Test.QuickCheck
import Test.QuickCheck.Applicative ()
import Test.QuickCheck.Instances.Num
import Control.Applicative

{- | Generates a non-empty list with the contents generated using its
     argument.
-}
nonEmpty :: Arbitrary a => Gen a -> Gen [a]
nonEmpty x = liftA2 (:) x (anyList x)

{- | Generates any list (possibly empty) with the contents generated using
     its argument.
-}
anyList :: Arbitrary a => Gen a -> Gen [a]
anyList x = frequency [(1, pure []), (4, nonEmpty x)]

{- | Generates an infinite list with contents generated using its argument
-}
infiniteList :: Arbitrary a => Gen a -> Gen [a]
infiniteList x = liftA2 (:) x (infiniteList x)


sumA :: (Applicative f, Num a) => f a -> f [a] -> f [a]
sumA = liftA2 (scanl (+))

monotonic_ :: (Arbitrary a, Num a) => (Gen a -> Gen [a]) -> Gen a -> Gen [a]
monotonic_ listGen gen = sumA arbitrary (listGen gen)

-- TODO: Generalise this to Ord a.
monotonic :: (Arbitrary a, Num a) => Gen a -> Gen [a]
monotonic gen = monotonic_ anyList gen

-- | Generate an infinite list of increasing values
increasingInf :: (Arbitrary a, Num a) => Gen [a]
increasingInf = monotonic_ infiniteList positive

-- | Generate increasing towards infinity
increasing :: (Arbitrary a, Num a) => Gen [a]
increasing = monotonic positive

-- | Generate nondecreasing values
nondecreasing :: (Arbitrary a, Num a) => Gen [a]
nondecreasing = monotonic nonNegative
