{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
#if MIN_VERSION_base(4,9,0)
-- https://github.com/conal/checkers/pull/38
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
#endif

module Test.QuickCheck.Instances.Array where

import Test.QuickCheck
import Data.Array

instance (Ix a, Integral a, Arbitrary b) => Arbitrary (Array a b) where
  arbitrary   =
    (\x -> listArray (0,fromIntegral (length x - 1)) x) <$> arbitrary

instance (Ix a, CoArbitrary b) => CoArbitrary (Array a b) where
  coarbitrary = coarbitrary . elems
