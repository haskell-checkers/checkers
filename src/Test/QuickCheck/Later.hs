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
----------------------------------------------------------------------

module Test.QuickCheck.Later
  ( isAssocDistinctTimes
  , isCommutDistinctTimes
  ) where

import Test.QuickCheck.Help
import Test.QuickCheck

import Control.Applicative
import System.Random

import System.IO.Unsafe
import Control.Concurrent

isAssocDistinctTimes (**) =
  forAll arbitrary $ \ (Later va ta) ->
  forAll arbitrary $ \ (Later vb tb) ->
  forAll arbitrary $ \ (Later vc tc) ->
  ta /= tb && tb /= tc && ta /= tc ==>
  (va ** vb) ** vc =-= va ** (vb ** vc)

isCommutDistinctTimes (**) =
  forAll arbitrary $ \ (Later va ta) ->
  forAll arbitrary $ \ (Later vb tb) ->
  ta /= tb ==> (va ** vb) =-= vb ** va

-- The second argument is the number of milliseconds it will take for
-- before first argument comes to fruitation
data Later a = Later a Int 
  deriving Show

later :: a -> Int -> Later a
later a t = Later (delay a t) t

instance Arbitrary a => Arbitrary (Later a) where
  -- The 17ms seems to be the minimum amount of time for two
  -- threadDelay's to be different.
  arbitrary = liftA2 later arbitrary (liftA (*17) $ genR (1,10))
  coarbitrary = undefined

delay :: a ->
         Int -> -- milliseconds
         a
delay i d = unsafePerformIO $ do
  v <- newEmptyMVar
  forkIO $ do
            threadDelay (d*1000)
            putMVar v i
  takeMVar v
