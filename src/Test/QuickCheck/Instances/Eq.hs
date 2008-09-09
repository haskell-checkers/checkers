module Test.QuickCheck.Instances.Eq (notEqualTo, notOneof) where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Control.Monad.Extensions

notEqualTo :: (Eq a,Arbitrary a) => a -> Gen a -> Gen a
notEqualTo v = satisfiesM (/= v)

notOneof :: (Eq a,Arbitrary a) => [a] -> Gen a
notOneof es = arbitrarySatisfying (not . (`elem` es))

