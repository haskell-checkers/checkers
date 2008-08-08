module Test.QuickCheck.Bottoms (bottom,infiniteComp) where

import Test.QuickCheck

bottom :: Gen a
bottom = return undefined

infiniteComp :: Gen a
infiniteComp = last (repeat undefined)
