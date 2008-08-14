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
