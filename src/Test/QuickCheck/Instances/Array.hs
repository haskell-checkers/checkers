module Test.QuickCheck.Instances.Array where

import Test.QuickCheck
import Control.Monad
import Data.Array

instance (Ix a, Integral a, Arbitrary b) => Arbitrary (Array a b) where
  arbitrary =
    liftM (\x -> listArray (0,fromIntegral (length x - 1)) x) arbitrary 
  coarbitrary x = coarbitrary (elems x)
