module Test.QuickCheck.Instances.Word where

import Control.Applicative
import Test.QuickCheck
import Data.Word

instance Arbitrary Word64 where
  arbitrary   = fromInteger <$> arbitrary

instance CoArbitrary Word64 where
  coarbitrary = variant . (fromIntegral :: Word64 -> Int)

instance Arbitrary Word32 where
  arbitrary   = fromInteger <$> arbitrary

instance CoArbitrary Word32 where
  coarbitrary = variant . (fromIntegral :: Word32 -> Int)

instance Arbitrary Word16 where
  arbitrary   = fromInteger <$> arbitrary

instance CoArbitrary Word16 where
  coarbitrary = variant . (fromIntegral :: Word16 -> Int)

instance Arbitrary Word8 where
  arbitrary   = fromInteger <$> arbitrary

instance CoArbitrary Word8 where
  coarbitrary = variant . (fromIntegral :: Word8 -> Int)


instance Arbitrary Word where
  arbitrary   = fromInteger <$> arbitrary

instance CoArbitrary Word where
  coarbitrary = variant . (fromIntegral :: Word -> Int)
