module Test.QuickCheck.Instances.Int where

import Control.Applicative
import Test.QuickCheck
import Data.Int

instance Arbitrary Int64 where
  arbitrary   = fromInteger <$> arbitrary
  coarbitrary = variant . fromIntegral

instance Arbitrary Int32 where
  arbitrary   = fromInteger <$> arbitrary
  coarbitrary = variant . fromIntegral

instance Arbitrary Int16 where
  arbitrary   = fromInteger <$> arbitrary
  coarbitrary = variant . fromIntegral

instance Arbitrary Int8 where
  arbitrary   = fromInteger <$> arbitrary
  coarbitrary = variant . fromIntegral

{- | Generates a positive integer.
-}
positiveInt :: (Arbitrary a,Integral a) => Gen a
positiveInt = (+1) . abs <$> arbitrary

{- | Generates a negative integer.
-}
negativeInt :: (Arbitrary a, Integral a) => Gen a
negativeInt = negate <$> positiveInt

{- | Generates a non-zero integer.
-}
nonZeroInt :: (Arbitrary a,Integral a) => Gen a
nonZeroInt = oneof [positiveInt
                   ,negativeInt]