module Test.QuickCheck.Instances.Maybe where

import Test.QuickCheck
import Control.Monad
import Control.Applicative

maybeGen :: Gen a -> Gen (Maybe a)
maybeGen x = oneof [return Nothing
                   ,Just <$> x]
