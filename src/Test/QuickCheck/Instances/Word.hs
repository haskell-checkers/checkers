module Test.QuickCheck.Instances.Word where

import Control.Applicative
import Test.QuickCheck
import Data.Word

instance Arbitrary Word64 where
  arbitrary   = fromInteger <$> arbitrary
  coarbitrary = variant . fromIntegral

instance Arbitrary Word32 where
  arbitrary   = fromInteger <$> arbitrary
  coarbitrary = variant . fromIntegral

instance Arbitrary Word16 where
  arbitrary   = fromInteger <$> arbitrary
  coarbitrary = variant . fromIntegral

instance Arbitrary Word8 where
  arbitrary   = fromInteger <$> arbitrary
  coarbitrary = variant . fromIntegral
