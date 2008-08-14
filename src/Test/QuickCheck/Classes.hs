{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, KindSignatures
           , Rank2Types
  #-}

{-# OPTIONS_GHC -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      :  Test.QuickCheck.Classes
-- Copyright   :  (c) Conal Elliott 2008
-- License     :  BSD3
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Some QuickCheck properties for standard type classes
----------------------------------------------------------------------

module Test.QuickCheck.Classes
  (
    monoid, monoidMorphism
  , functor, functorMorphism
  , applicative, applicativeMorphism
  , monad, monadMorphism
  , functorMonoid
  , semanticMonoid
  , semanticFunctor
  , semanticApplicative
  )
  where

import Data.Monoid
import Control.Applicative
import Control.Monad (join)
import Control.Arrow (first)
import Test.QuickCheck
import Text.Show.Functions ()

import Test.QuickCheck.Help
import Test.QuickCheck.Instances.Char ()


-- | Properties to check that the 'Monoid' 'a' satisfies the monoid
-- properties.  The argument value is ignored and is present only for its
-- type.
monoid :: forall a. (Monoid a, Show a, Arbitrary a, EqProp a) =>
          a -> TestBatch
monoid = const ( "monoid"
               , [ ("left  identity", leftId  mappend (mempty :: a))
                 , ("right identity", rightId mappend (mempty :: a))
                 , ("associativity" , isAssoc (mappend :: Binop a))
                 ]
               )

-- | Monoid homomorphism properties.  See also 'homomorphism'.
monoidMorphism :: (Monoid a, Monoid b, EqProp b, Show a, Arbitrary a) =>
                  (a -> b) -> TestBatch
monoidMorphism q = ("monoid morphism", homomorphism monoidD monoidD q)

functorMonoid :: forall m a b.
  ( Functor m
  , Monoid (m a)
  , Monoid (m b)
  , Arbitrary (a->b)
  , Arbitrary (m a)
  , Show (m a)
  , EqProp (m b)) =>
  m (a,b) -> TestBatch
functorMonoid = const ("functor-monoid"
                      , [ ( "identity",property identityP )
                        , ( "binop", property binopP )
                        ]
                      )
  where
    identityP :: (a->b) -> Property
    identityP f = (fmap f) (mempty :: m a) =-= (mempty :: m b)
    binopP :: (a->b) -> (m a) -> (m a) -> Property
    binopP f u v = (fmap f) (u `mappend` v) =-= (fmap f u) `mappend` (fmap f v)

semanticMonoid :: forall a b.
  ( Model a b
  , Monoid a
  , Monoid b
  , Show a
  , Arbitrary a
  , EqProp b
  ) =>
  a -> TestBatch
semanticMonoid = const (first (const "semantic-monoid")
                              (monoidMorphism (model:: a -> b)))

-- | Properties to check that the 'Functor' @m@ satisfies the functor
-- properties.
functor :: forall m a b c.
           ( Functor m
           , Arbitrary a, Arbitrary b, Arbitrary c
           , Show (m a), Arbitrary (m a), EqProp (m a), EqProp (m c)) =>
           m (a,b,c) -> TestBatch
functor = const ( "functor"
                , [ ("identity", property identityP)
                  , ("compose" , property composeP) ]
                )
 where
   identityP :: Property
   composeP  :: (b -> c) -> (a -> b) -> Property
   
   identityP = fmap id =-= (id :: m a -> m a)
   composeP g f = fmap g . fmap f =-= (fmap (g.f) :: m a -> m c)

-- Note the similarity between 'functor' and 'monoidMorphism'.  The
-- functor laws say that 'fmap' is a homomorphism w.r.t '(.)':
-- 
--   functor = const ("functor", homomorphism endoMonoidD endoMonoidD fmap)
-- 
-- However, I don't think the types can work out, since 'fmap' is used at
-- three different types.


-- | 'Functor' morphism (natural transformation) properties
functorMorphism :: forall f g.
                   ( Functor f, Functor g
                   , Arbitrary (f X), Show (f X), EqProp (g Y)
                   ) =>
                  (forall a. f a -> g a) -> TestBatch
functorMorphism q = ("functor morphism", [("fmap", property fmapP)])
 where
   -- fmapP :: (X -> Y) -> f X -> Property
   -- fmapP h l = q (fmap h l) =-= fmap h (q l)
   fmapP :: (X -> Y) -> Property
   fmapP h = q . fmap h =-= fmap h . q

semanticFunctor :: forall f g.
  ( Model (f X) (g X)
  , Model (f Y) (g Y)
  , Functor f
  , Functor g
  , EqProp (g Y)
  , Show (f X)
  , Arbitrary (f X)
  ) =>
  f X -> TestBatch
semanticFunctor = const ("semantic-functor", [("fmap", property fmapP)])
  where
    fmapP :: (X -> Y) -> f X -> Property
    fmapP h l = model (fmap h l) =-= fmap h ((model :: f X -> g X) l)

-- Alternate definition? I couldn't get the types to work out here. I'm not
-- sure why. -DJS
--
-- semanticFunctor :: forall f g a.
--   ( Model (f a) (g a)
--   , Functor f
--   , Functor g
--   , Arbitrary (f X)
--   , Show (f X)
--   , EqProp (g Y)
--   ) =>
--   f X -> TestBatch
-- semanticFunctor = const (functorMorphism (model::forall b. f b -> g b))

-- Note: monomorphism prevent us from saying @commutes (.) q (fmap h)@,
-- since @fmap h@ is used at two different types.

-- | Properties to check that the 'Applicative' @m@ satisfies the monad properties
applicative :: forall m a b c.
               ( Applicative m
               , Arbitrary a, Arbitrary b, Arbitrary (m a)
               , Arbitrary (m (b -> c)), Show (m (b -> c))
               , Arbitrary (m (a -> b)), Show (m (a -> b))
               , Show a, Show (m a)
               , EqProp (m a), EqProp (m b), EqProp (m c)
               ) =>
               m (a,b,c) -> TestBatch
applicative = const ( "applicative"
                    , [ ("identity"    , property identityP)
                      , ("composition" , property compositionP)
                      , ("homomorphism", property homomorphismP)
                      , ("interchange" , property interchangeP)
                      , ("functor"     , property functorP)
                      ]
                    )
 where
   identityP     :: m a -> Property
   compositionP  :: m (b -> c) -> m (a -> b) -> m a -> Property
   homomorphismP :: (a -> b) -> a -> Property
   interchangeP  :: m (a -> b) -> a -> Property
   functorP      :: (a -> b) -> m a -> Property
   
   identityP v        = (pure id <*> v) =-= v
   compositionP u v w = (pure (.) <*> u <*> v <*> w) =-= (u <*> (v <*> w))
   homomorphismP f x  = (pure f <*> pure x) =-= (pure (f x) :: m b)
   interchangeP u y   = (u <*> pure y) =-= (pure ($ y) <*> u)
   functorP f x       = (fmap f x) =-= (pure f <*> x)


-- | 'Applicative' morphism properties
applicativeMorphism :: forall f g.
                       ( Applicative f, Applicative g
                       , Show (f X), Arbitrary (f X), EqProp (g X), EqProp (g Y)
                       , Show (f (X -> Y)), Arbitrary (f (X -> Y))) =>
                       (forall a. f a -> g a) -> TestBatch
applicativeMorphism q =
  ( "applicative morphism"
  , [("pure", property pureP), ("apply", property applyP)] )
 where
   pureP  :: X -> Property
   applyP :: f (X->Y) -> f X -> Property
   
   pureP a = q (pure a) =-= pure a
   applyP mf mx = q (mf <*> mx) =-= (q mf <*> q mx)

semanticApplicative :: forall f g.
                       ( Applicative f, Applicative g
                       , Model (f X) (g X)
                       , Model (f (X->Y)) (g (X->Y))
                       , Model (f Y) (g Y)
                       , Show (f X), Arbitrary (f X), EqProp (g X), EqProp (g Y)
                       , Show (f (X -> Y)), Arbitrary (f (X -> Y))) =>
                       f X -> TestBatch
semanticApplicative = const
  ( "semantic-applicative"
  , [("pure", property pureP), ("apply", property applyP)] )
 where
   pureP  :: X -> Property
   applyP :: f (X->Y) -> f X -> Property
   
   pureP a = (model:: f X -> g X) (pure a) =-= pure a
   applyP mf mx = model (mf <*> mx) =-= (model mf <*> model mx)


-- | Properties to check that the 'Monad' @m@ satisfies the monad properties
monad :: forall m a b c.
         ( Monad m
         , Show a, Arbitrary a, Arbitrary b
         , Arbitrary (m a), EqProp (m a), Show (m a)
         , Arbitrary (m b), EqProp (m b)
         , Arbitrary (m c), EqProp (m c)
         ) =>
         m (a,b,c) -> TestBatch
monad = const ( "monad laws"
              , [ ("left  identity", property leftP)
                , ("right identity", property rightP)
                , ("associativity" , property assocP)
                ]
              )
 where
   leftP  :: (a -> m b) -> a -> Property
   rightP :: m a -> Property
   assocP :: m a -> (a -> m b) -> (b -> m c) -> Property
   
   leftP f a    = (return a >>= f)  =-= f a
   rightP m     = (m >>= return)    =-=  m
   assocP m f g = ((m >>= f) >>= g) =-= (m >>= (\x -> f x >>= g))

-- | 'Monad' morphism properties

-- | 'Applicative' morphism properties
monadMorphism :: forall f g.
                 ( Monad f, Monad g, Functor g
                 , Show (f X), Show (f (X -> Y)), Show (f (f (X -> Y)))
                 , Arbitrary (f X), Arbitrary (f Y)
                 , Arbitrary (f (X -> Y)) , Arbitrary (f (f (X -> Y)))
                 , EqProp (g X), EqProp (g Y), EqProp (g (X -> Y))
                 ) =>
                (forall a. f a -> g a) -> TestBatch
monadMorphism q =
  ( "monad morphism"
  , [ ("return", property returnP), ("bind", property bindP), ("join", property joinP) ] )
 where
   returnP :: X -> Property
   bindP :: f X -> (X -> f Y) -> Property
   joinP :: f (f (X->Y)) -> Property
   
   returnP a = q (return a) =-= return a
   bindP u k = q (u >>= k)  =-= (q u >>= q . k)
   joinP uu  = q (join uu)  =-= join (fmap q (q uu))

-- The join and bind properties are redundant.  Pick one.

--      q (join uu)
--   == q (uu >>= id)
--   == q uu >>= q . id
--   == q uu >>= q
--   == join (fmap q (q uu))

--      q (u >>= k)
--   == q (fmap k (join u))
--   == fmap k (q (join u))  -- if also a functor morphism
--   == fmap k (join (fmap q (q uu)))
--   == fmap k (q u >>= q)
--   == ???

-- I'm stuck at the end here.  What's missing?
