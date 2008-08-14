{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module Test.QuickCheck.Applicative where

import Test.QuickCheck
import Control.Monad
import Control.Applicative

instance Applicative Gen where { pure = return ; (<*>) = ap }
