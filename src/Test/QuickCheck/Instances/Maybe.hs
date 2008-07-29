module Test.QuickCheck.Instances.Maybe where

import Test.QuickCheck
import Control.Monad

maybeGen :: Gen a -> Gen (Maybe a)
maybeGen x = oneof [return Nothing, liftM Just x]
