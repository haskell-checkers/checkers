module Test.QuickCheck.Bottoms (bottom,infiniteComp) where

import Test.QuickCheck

import Control.Monad (forever)
import System.IO.Unsafe
import Control.Concurrent

bottom :: Gen a
bottom = return undefined

infiniteComp :: Gen a
infiniteComp = return hang

-- Without using unsafePerformIO, is there a way to define a
-- non-terminating but non-erroring pure value that consume very little
-- resources while not terminating?

-- | Never yield an answer.  Like 'undefined' or 'error "whatever"', but
-- don't raise an error, and don't consume computational resources.
hang :: a
hang = unsafePerformIO hangIO

-- | Block forever
hangIO :: IO a
hangIO = do -- putStrLn "warning: blocking forever."
            -- Any never-terminating computation goes here
            -- This one can yield an exception "thread blocked indefinitely"
            -- newEmptyMVar >>= takeMVar
            -- sjanssen suggests this alternative:
            forever $ threadDelay maxBound
            -- forever's return type is (), though it could be fully
            -- polymorphic.  Until it's fixed, I need the following line.
            return undefined
