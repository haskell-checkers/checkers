module Test.QuickCheck.Instances.List
       (anyList,nonEmpty
       ,infiniteList
       ,setLength
       ,increasing,nondecreasing
       ,increasingInf,nondecreasingInf
       ,decreasing,nonincreasing
       ,decreasingInf,nonincreasingInf
       ) where

import Test.QuickCheck
import Test.QuickCheck.Instances.Num
import Control.Applicative

{- | Generates a non-empty list with the contents generated using its
     argument.
-}
nonEmpty :: Gen a -> Gen [a]
nonEmpty x = liftA2 (:) x (anyList x)

{- | Generates any list (possibly empty) with the contents generated using
     its argument.
-}
anyList :: Gen a -> Gen [a]
anyList x = frequency [(1, pure []), (4, nonEmpty x)]

{- | Generates an infinite list with contents generated using its argument
-}
infiniteList :: Gen a -> Gen [a]
infiniteList x = liftA2 (:) x (infiniteList x)

{- | Generates a list with a set length
-}
setLength :: Int -> Gen a -> Gen [a]
setLength 0 _ = pure []
setLength n g = (:) <$> g <*> setLength (n-1) g

sumA :: (Applicative f, Num a) => f a -> f [a] -> f [a]
sumA = liftA2 (scanl (+))

monotonic_ :: (Arbitrary a, Num a) => (Gen a -> Gen [a]) -> Gen a -> Gen [a]
monotonic_ listGen gen = sumA arbitrary (listGen gen)

-- TODO: Generalise this to Ord a.
monotonic :: (Arbitrary a, Num a) => Gen a -> Gen [a]
monotonic gen = monotonic_ anyList gen

-- | Generate increasing towards infinity
increasing :: (Arbitrary a, Eq a, Num a) => Gen [a]
increasing = monotonic positive

-- | Generate an infinite list of increasing values
increasingInf :: (Arbitrary a, Eq a, Num a) => Gen [a]
increasingInf = monotonic_ infiniteList positive

-- | Generate nondecreasing values
nondecreasing :: (Arbitrary a, Num a) => Gen [a]
nondecreasing = monotonic nonNegative

-- | Generate an infinite list of nondecreasing values
nondecreasingInf :: (Arbitrary a, Num a) => Gen [a]
nondecreasingInf = monotonic_ infiniteList nonNegative

-- | Generate increasing towards infinity
decreasing :: (Arbitrary a, Eq a, Num a) => Gen [a]
decreasing = monotonic negative

-- | Generate an infinite list of increasing values
decreasingInf :: (Arbitrary a, Eq a, Num a) => Gen [a]
decreasingInf = monotonic_ infiniteList negative

-- | Generate nondecreasing values
nonincreasing :: (Arbitrary a, Num a) => Gen [a]
nonincreasing = monotonic nonPositive

-- | Generate an infinite list of nondecreasing values
nonincreasingInf :: (Arbitrary a, Num a) => Gen [a]
nonincreasingInf = monotonic_ infiniteList nonPositive
