module Test.QuickCheck.Instances.Int where

import Control.Monad
import Test.QuickCheck
import Data.Int

instance Arbitrary Int64 where
  arbitrary = liftM fromInteger arbitrary
  coarbitrary n = variant (fromIntegral n)

instance Arbitrary Int32 where
  arbitrary = liftM fromInteger arbitrary
  coarbitrary n = variant (fromIntegral n)

instance Arbitrary Int16 where
  arbitrary = liftM fromInteger arbitrary
  coarbitrary n = variant (fromIntegral n)

instance Arbitrary Int8 where
  arbitrary = liftM fromInteger arbitrary
  coarbitrary n = variant (fromIntegral n)

{- | Generates a positive integer.
-}
positiveInt :: (Arbitrary a,Integral a) => Gen a
positiveInt = liftM ((+1) . abs) arbitrary

negativeInt :: (Arbitrary a, Integral a) => Gen a
negativeInt = liftM (((-1) -) . abs) arbitrary

nonZeroInt :: (Arbitrary a,Integral a) => Gen a
nonZeroInt = oneof [positiveInt, negativeInt]