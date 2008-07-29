module Test.QuickCheck.Instances.Word where

import Control.Monad
import Test.QuickCheck
import Data.Word

instance Arbitrary Word64 where
  arbitrary = liftM fromInteger arbitrary
  coarbitrary n = variant (fromIntegral n)

instance Arbitrary Word32 where
  arbitrary = liftM fromInteger arbitrary
  coarbitrary n = variant (fromIntegral n)

instance Arbitrary Word16 where
  arbitrary = liftM fromInteger arbitrary
  coarbitrary n = variant (fromIntegral n)

instance Arbitrary Word8 where
  arbitrary = liftM fromInteger arbitrary
  coarbitrary n = variant (fromIntegral n)
