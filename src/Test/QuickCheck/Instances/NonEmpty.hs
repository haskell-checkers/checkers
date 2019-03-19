{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module Test.QuickCheck.Instances.NonEmpty where

import Control.Applicative
import Data.List.NonEmpty
import Test.QuickCheck

instance Arbitrary a => Arbitrary (NonEmpty a) where
  arbitrary = liftA2 (:|) arbitrary arbitrary

instance CoArbitrary a => CoArbitrary (NonEmpty a) where
  coarbitrary = coarbitrary . toList
