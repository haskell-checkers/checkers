module Test.QuickCheck.Instances.Ord where

import Test.QuickCheck
import Control.Monad.Extensions

greaterThan :: (Ord a) => a -> Gen a -> Gen a
greaterThan v = satisfiesM (> v)

lessThan :: (Ord a) => a -> Gen a -> Gen a
lessThan v = satisfiesM (< v)
