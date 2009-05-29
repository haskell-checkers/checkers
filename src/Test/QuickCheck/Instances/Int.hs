module Test.QuickCheck.Instances.Int where

import Control.Applicative
import Test.QuickCheck
import Data.Int

instance Arbitrary Int64 where
  arbitrary   = fromInteger <$> arbitrary

instance CoArbitrary Int64 where
  coarbitrary = variant . (fromIntegral :: Int64 -> Int)

instance Arbitrary Int32 where
  arbitrary   = fromInteger <$> arbitrary

instance CoArbitrary Int32 where
  coarbitrary = variant . (fromIntegral :: Int32 -> Int)

instance Arbitrary Int16 where
  arbitrary   = fromInteger <$> arbitrary

instance CoArbitrary Int16 where
  coarbitrary = variant . (fromIntegral :: Int16 -> Int)

instance Arbitrary Int8 where
  arbitrary   = fromInteger <$> arbitrary

instance CoArbitrary Int8 where
  coarbitrary = variant . (fromIntegral :: Int8 -> Int)
