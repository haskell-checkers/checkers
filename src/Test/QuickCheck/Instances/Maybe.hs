module Test.QuickCheck.Instances.Maybe (maybeGen) where

import Control.Applicative (pure, (<$>))
import Test.QuickCheck

maybeGen :: Gen a -> Gen (Maybe a)
maybeGen x = oneof [pure Nothing
                   ,Just <$> x]
