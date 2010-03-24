{-# OPTIONS_GHC -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      :  Data.Later
-- Copyright   :  (c) David Sankel 2008
-- License     :  BSD3
-- 
-- Maintainer  :  david@sankelsoftware.com
-- Stability   :  experimental
-- 
-- Later. Allows for testing of functions that depend on the order of
-- evaluation.
--
-- TODO: move this functionality to the testing package for Unamb.
----------------------------------------------------------------------

module Test.QuickCheck.Later
  ( isAssocTimes
  , isCommutTimes
  , delay
  , delayForever
  ) where

import Test.QuickCheck.Checkers
import Test.QuickCheck

import System.Random (Random)

import System.IO.Unsafe
import Control.Concurrent
import Control.Monad (forever)

-- Generate a random delay up to given max seconds for a property.
delayP :: (Num t, System.Random.Random t, Testable b) => t -> (t -> b) -> Property
delayP d = forAll (genR (0,d))

-- | Is the given function commutative when restricted to the same value
-- but possibly different times?
isCommutTimes :: (EqProp b, Arbitrary a, Show a) => Double -> (a -> a -> b) -> Property

isCommutTimes d (#) =
  delayP d $ \ t1 ->
  delayP d $ \ t2 ->
  \ v -> let del = flip delay v in
           del t1 # del t2 =-= del t2 # del t1

-- Note that we delay v by t1 and by t2 twice.
-- 
-- TODO: make sure CSE isn't kicking in.  Examine the core code.

-- | Is the given function associative when restricted to the same value
-- but possibly different times?
isAssocTimes :: (EqProp a, Arbitrary a, Show a) => Double -> (a -> a -> a) -> Property

isAssocTimes d (#) =
  delayP d $ \ t1 ->
  delayP d $ \ t2 ->
  delayP d $ \ t3 ->
  \ v -> let del = flip delay v in
           (del t1 # del t2) # del t3 =-= del t1 # (del t2 # del t3)


-- The value eventually returned by an action.  Probably handy elsewhere.
-- TODO: what are the necessary preconditions in order to make this
-- function referentially transparent?
eventually :: IO a -> a
eventually = unsafePerformIO . unsafeInterleaveIO

-- Why unsafeInterleaveIO?  Because ...

-- | Delay a value's availability by the given duration in seconds.
-- Note that the delay happens only on the first evaluation.
delay :: RealFrac t => t -> a -> a
delay d a = eventually $ threadDelay (round (1.0e6 * d)) >> return a

-- | A value that is never available.  Rerun of @hang@ from unamb, but
-- replicated to avoid mutual dependency.
-- 
-- TODO: Remove when this module is moved into the unamb-test package.
delayForever :: a
delayForever = unsafePerformIO $ do _ <- forever (threadDelay maxBound)
                                    return undefined
