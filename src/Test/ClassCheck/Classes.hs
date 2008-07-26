{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}
{-# OPTIONS_GHC -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      :  Test.ClassCheck.Classes
-- Copyright   :  (c) Conal Elliott 2008
-- License     :  BSD3
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Some QuickCheck properties for standard type classes
----------------------------------------------------------------------

module Test.ClassCheck.Classes
--   (
--     monoidProps
--   )
  where

import Data.Monoid
import Test.QuickCheck

import Test.QCHelp

-- | Properties to check that the 'Monoid' 'a' satisfies the monoid properties.
monoidProps :: forall a. (Monoid a, Show a, Arbitrary a, EqProp a) =>
               a -> TestBatch
monoidProps = const ( "monoidProps"
              , [ ("left  identity", leftId  mappend (mempty :: a))
                , ("right identity", rightId mappend (mempty :: a))
                , ("associativity" , isAssoc (mappend :: Binop a))
                ]
              )


-- | Properties to check that the 'Monad' 'm' satisfies the monad properties.
monadProps :: forall m a b c.
              ( Monad m
              , Show a, Arbitrary a, Arbitrary b
              , Arbitrary (m a), EqProp (m a), Show (m a)
              , Arbitrary (m b), EqProp (m b), Show (a -> m b)
              , Arbitrary (m c), EqProp (m c), Show (b -> m c)
              ) =>
              m (a,b) -> TestBatch
monadProps _ = ( "monad laws"
                , [ ("left  identity", property left)
                  , ("right identity", property right)
                  , ("associativity" , property assoc)
                  ]
                )
 where
   left  :: (a -> m b) -> a -> Property
   right :: m a -> Property
   assoc :: m a -> (a -> m b) -> (b -> m c) -> Property
   
   left f a    = (return a >>= f)  =-= f a
   right m     = (m >>= return)    =-=  m
   assoc m f g = ((m >>= f) >>= g) =-= (m >>= (\x -> f x >>= g))
