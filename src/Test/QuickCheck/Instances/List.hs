module Test.QuickCheck.Instances.List where

import Test.QuickCheck
import Control.Monad

{- | Generates a non-empty list with the contents generated using its
     argument.
-}
nonEmpty :: Arbitrary a => Gen a -> Gen [a]
nonEmpty x = liftM2 (:) x (oneof [return [],nonEmpty x])

{- | Generates any list (possibly empty) with the contents generated using
     its argument.
-}
anyList :: Arbitrary a => Gen a -> Gen [a]
anyList x = oneof [return [],anyList x]
