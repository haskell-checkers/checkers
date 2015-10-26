{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module Test.QuickCheck.Instances
       (module Test.QuickCheck.Instances.Char
       ,module Test.QuickCheck.Instances.Eq
       ,module Test.QuickCheck.Instances.List
       ,module Test.QuickCheck.Instances.Maybe
       ,module Test.QuickCheck.Instances.Num
       ,module Test.QuickCheck.Instances.Ord
       ,module Test.QuickCheck.Instances.Tuple
       ) where

import Test.QuickCheck.Instances.Array ()
import Test.QuickCheck.Instances.Char
import Test.QuickCheck.Instances.Eq
import Test.QuickCheck.Instances.List
import Test.QuickCheck.Instances.Maybe
import Test.QuickCheck.Instances.Num
import Test.QuickCheck.Instances.Ord
import Test.QuickCheck.Instances.Tuple
