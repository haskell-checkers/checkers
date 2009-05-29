module Test.QuickCheck.Instances.Array where

import Test.QuickCheck
import Control.Applicative
import Data.Array

instance (Ix a, Integral a, Arbitrary b) => Arbitrary (Array a b) where
  arbitrary   =
    (\x -> listArray (0,fromIntegral (length x - 1)) x) <$> arbitrary 

instance (Ix a, Integral a, CoArbitrary b) => CoArbitrary (Array a b) where
  coarbitrary = coarbitrary . elems
