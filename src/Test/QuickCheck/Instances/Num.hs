module Test.QuickCheck.Instances.Num 
       (nonNegative,nonPositive
       ,negative,positive
       ,nonZero,nonZero_
       ) where

import Test.QuickCheck
import Control.Monad.Extensions
import Control.Applicative

nonNegative :: (Num a, Arbitrary a) => Gen a
nonNegative = abs <$> arbitrary

positive :: (Eq a, Num a, Arbitrary a) => Gen a
positive = nonZero nonNegative

nonPositive :: (Num a, Arbitrary a) => Gen a
nonPositive = negate <$> nonNegative

negative :: (Eq a, Num a, Arbitrary a) => Gen a
negative = negate <$> positive

nonZero :: (Eq a, Num a, Arbitrary a) => Gen a -> Gen a
nonZero g =
  sized (\s -> satisfiesM (/= 0) (if (s == 0) then (resize 1 g) else g))

nonZero_ :: (Eq a, Num a, Arbitrary a) => Gen a
nonZero_ = nonZero arbitrary
