{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, KindSignatures
           , Rank2Types
  #-}

{-# OPTIONS_GHC -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      :  Test.Checkers.Classes
-- Copyright   :  (c) Conal Elliott 2008
-- License     :  BSD3
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Some QuickCheck properties for standard type classes
----------------------------------------------------------------------

module Test.Checkers.Classes
--   (
--     monoid
--   )
  where

import Data.Monoid
import Control.Applicative
import Test.QuickCheck
import Text.Show.Functions ()

import Test.QCHelp

-- | Properties to check that the 'Monoid' 'a' satisfies the monoid properties.
monoid :: forall a. (Monoid a, Show a, Arbitrary a, EqProp a) =>
          a -> TestBatch
monoid = const ( "monoid"
               , [ ("left  identity", leftId  mappend (mempty :: a))
                 , ("right identity", rightId mappend (mempty :: a))
                 , ("associativity" , isAssoc (mappend :: Binop a))
                 ]
               )


-- | Explicit 'Monoid' dictionary.  Doesn't have to correspond to an
-- actual 'Monoid' instance.
data MonoidD a = MonoidD a (a -> a -> a)

-- | 'Monoid' dictionary built from the 'Monoid' methods.
monoidD :: Monoid a => MonoidD a
monoidD = MonoidD mempty mappend

endoMonoidD :: MonoidD (a -> a)
endoMonoidD = MonoidD id (.)

-- | Homomorphism properties with respect to given monoid dictionaries.
-- See also 'monoidMorphism'.
homomorphism :: (EqProp b, Show a, Arbitrary a) =>
                MonoidD a -> MonoidD b -> (a -> b) -> [(String,Property)]
homomorphism (MonoidD ida opa) (MonoidD idb opb) q =
  [ ("identity" , q ida =-= idb)
  , ("binop", property $ \ u v -> q (u `opa` v) =-= q u `opb` q v)
  ]

-- | Monoid homomorphism properties.  See also 'homomorphism'.
monoidMorphism :: (Monoid a, Monoid b, EqProp b, Show a, Arbitrary a) =>
                  (a -> b) -> TestBatch
monoidMorphism q = ("monoid morphism", homomorphism monoidD monoidD q)



-- | Properties to check that the 'Functor' @m@ satisfies the functor
-- properties.
functor :: forall m a b c.
           ( Functor m
           , Arbitrary a, Arbitrary b, Arbitrary c
           , Show (m a), Arbitrary (m a), EqProp (m a), EqProp (m c)) =>
           m (a,b,c) -> TestBatch
functor = const ( "functor"
                , [ ("identity", property identity)
                  , ("compose" , property compose) ]
                )
 where
   identity = fmap id =-= (id :: m a -> m a)
   compose :: (b -> c) -> (a -> b) -> Property
   compose g f = fmap g . fmap f =-= (fmap (g.f) :: m a -> m c)

-- Note the similarity between 'functor' and 'monoidMorphism'.  The
-- functor laws say that 'fmap' is a homomorphism w.r.t '(.)':
-- 
--   functor = const ("functor", homomorphism endoMonoidD endoMonoidD fmap)
-- 
-- However, I don't think the types can work out, since 'fmap' is used at
-- three different types.


functorMorphism :: forall f g.
                   ( Functor f, Functor g
                   , Arbitrary (f X), Show (f X), EqProp (g Y)
                   ) =>
                  (forall c. f c -> g c) -> TestBatch
functorMorphism q =
  ( "functor morphism"
  , [("fmap", property fmapP)]
  )
 where
   -- fmapP :: (X -> Y) -> f X -> Property
   -- fmapP h l = q (fmap h l) =-= fmap h (q l)
   fmapP :: (X -> Y) -> Property
   fmapP h = q . fmap h =-= fmap h . q

-- or
--   property $ \ h -> q . fmap h =-= fmap h . q

-- Note: monomorphism prevent us from saying @commutes (.) q (fmap h)@,
-- since @fmap h@ is used at two different types.

-- | Properties to check that the 'Monad' @m@ satisfies the monad properties.
monad :: forall m a b c.
         ( Monad m
         , Show a, Arbitrary a, Arbitrary b
         , Arbitrary (m a), EqProp (m a), Show (m a)
         , Arbitrary (m b), EqProp (m b)
         , Arbitrary (m c), EqProp (m c)
         ) =>
         m (a,b) -> TestBatch
monad _ = ( "monad laws"
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


applicativeMorphism :: ( Applicative f, Applicative g
                       , Arbitrary a, Show a, EqProp (g a)) =>
                       (f a -> g a) -> TestBatch
applicativeMorphism q =
  ( "applicative morphism"
  , [ ("pure" , property $ \ a -> q (pure a) =-= pure a)
    -- , ("apply", property $ \ mf mx -> q (mf <*> mx) =-= (q mf <*> q mx))
    ]
  )
