module Test.QuickCheck.Instances.List where

import Test.QuickCheck
import Control.Monad

{- | Generates a non-empty list with the contents generated using its
     argument.
-}
nonEmpty :: Arbitrary a => Gen a -> Gen [a]
nonEmpty x = liftM2 (:) x (anyList x)

{- | Generates any list (possibly empty) with the contents generated using
     its argument.
-}
anyList :: Arbitrary a => Gen a -> Gen [a]
anyList x = frequency [(1, return []), (4, nonEmpty x)]

{- | Generates an infinite list with contents generated using its argument
-}
infiniteList :: Arbitrary a => Gen a -> Gen [a]
infiniteList x = liftM2 (:) x (infiniteList x)
