module Test.QuickCheck.Instances.Maybe (maybeGen) where

import Test.QuickCheck
import Test.QuickCheck.Applicative ()
import Control.Applicative

maybeGen :: Gen a -> Gen (Maybe a)
maybeGen x = oneof [pure Nothing
                   ,Just <$> x]
