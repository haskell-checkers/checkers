{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, KindSignatures
           , Rank2Types, TypeOperators, CPP
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
  ( ordRel, ord, ordMorphism, semanticOrd
  , semigroup
  , monoid, monoidMorphism, semanticMonoid
  , functor, functorMorphism, semanticFunctor, functorMonoid
  , apply, applyMorphism, semanticApply
  , applicative, applicativeMorphism, semanticApplicative
  , bind, bindMorphism, semanticBind, bindApply
  , monad, monadMorphism, semanticMonad, monadFunctor
  , monadApplicative, arrow, arrowChoice, foldable, foldableFunctor, traversable
  , monadPlus, monadOr, alt, alternative
  )
  where

import Control.Applicative ((<$>))
import Data.Foldable (Foldable(..))
import Data.Functor.Apply (Apply ((<.>)))
import Data.Functor.Alt (Alt ((<!>)))
import Data.Functor.Bind (Bind ((>>-)), apDefault)
import qualified Data.Functor.Bind as B (Bind (join))
import Data.Semigroup (Semigroup ((<>)))
import Data.Monoid (Monoid (mappend, mempty), Endo(..), Dual(..), Sum(..), Product(..))
import Data.Traversable (Traversable (..), fmapDefault, foldMapDefault)
import Control.Applicative
import Control.Monad (MonadPlus (..), ap, join)
import Control.Arrow (Arrow,ArrowChoice,first,second,left,right,(>>>),arr)
import Test.QuickCheck
import Text.Show.Functions ()

import Test.QuickCheck.Checkers
import Test.QuickCheck.Instances.Char ()


-- | Total ordering.  @gen a@ ought to generate values @b@ satisfying @a
-- `rel` b@ fairly often.
ordRel :: forall a. (Ord a, Show a, Arbitrary a, EqProp a) =>
          BinRel a -> (a -> Gen a) -> TestBatch
ordRel rel gen =
  ( "ord"
  , [ ("reflexive"    , reflexive     rel    )
    , ("transitive"   , transitive    rel gen)
    , ("antiSymmetric", antiSymmetric rel gen)
    ]
  )

-- | Total ordering
ord :: forall a. (Ord a, Show a, Arbitrary a, EqProp a) =>
       (a -> Gen a) -> TestBatch
ord = ordRel (<=)



-- | 'Ord' morphism properties.  @h@ is an 'Ord' morphism iff:
--
-- >    a <= b = h a <= h b
-- >
-- >    h (a `min` b) = h a `min` h b
-- >    h (a `max` b) = h a `max` h b
ordMorphism :: (Ord a, Ord b, EqProp b, Show a, Arbitrary a) =>
               (a -> b) -> TestBatch

ordMorphism h = ( "ord morphism"
                , [ ("(<=)", distrib' (<=))
                  , ("min" , distrib  min )
                  , ("max" , distrib  max )
                  ]
                )
 where
   distrib  :: (forall c. Ord c => c -> c -> c) -> Property
   distrib  op = property $ \ u v -> h (u `op` v) =-= h u `op` h v

   distrib' :: EqProp d => (forall c. Ord c => c -> c -> d) -> Property
   distrib' op = property $ \ u v -> u `op` v =-= h u `op` h v

-- | The semantic function ('model') for @a@ is an 'ordMorphism'.
semanticOrd :: forall a b.
  ( Model a b
  , Ord a, Ord b
  , Show a
  , Arbitrary a
  , EqProp b
  ) =>
  a -> TestBatch
semanticOrd = const (first ("semantic " ++)
                      (ordMorphism (model :: a -> b)))


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

-- | Properties to check that the 'Semigroup' 'a' satisfies the semigroup
-- properties.  The argument value is ignored and is present only for its
-- type.
semigroup :: forall a. (Semigroup a, Show a, Arbitrary a, EqProp a) =>
             a -> TestBatch
semigroup = const ( "semigroup"
                  , [("associativity", isAssoc ((<>) :: Binop a))
                    ]
                  )

-- | Monoid homomorphism properties.  See also 'homomorphism'.
monoidMorphism :: (Monoid a, Monoid b, EqProp b, Show a, Arbitrary a) =>
                  (a -> b) -> TestBatch
monoidMorphism q = ("monoid morphism", homomorphism monoidD monoidD q)

semanticMonoid :: forall a b.
  ( Model a b
  , Monoid a, Monoid b
  , Show a
  , Arbitrary a
  , EqProp b
  ) =>
  a -> TestBatch

-- | The semantic function ('model') for @a@ is a 'monoidMorphism'.
semanticMonoid = const (first ("semantic " ++)
                              (monoidMorphism (model:: a -> b)))

functorMonoid :: forall m a b.
  ( Functor m
  , Monoid (m a)
  , Monoid (m b)
  , CoArbitrary a
  , Arbitrary b
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

-- <camio> There I have an attempt at doing this. I eventually implemented
-- those semanticMorphisms as their own functions. I'm not too thrilled with
-- that implementation, but it works.

-- TODO: figure out out to eliminate the redundancy.

-- | Properties to check that the 'Functor' @m@ satisfies the functor
-- properties.
functor :: forall m a b c.
           ( Functor m
           , Arbitrary a, Arbitrary b, Arbitrary c
           , CoArbitrary a, CoArbitrary b
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
                   , Arbitrary (f NumT), Show (f NumT)
                   , EqProp (g T)
                   ) =>
                  (forall a. f a -> g a) -> TestBatch
functorMorphism q = ("functor morphism", [("fmap", property fmapP)])
 where
   -- fmapP :: (NumT -> T) -> f NumT -> Property
   -- fmapP h l = q (fmap h l) =-= fmap h (q l)
   fmapP :: (NumT -> T) -> Property
   fmapP h = q . fmap h =-= fmap h . q

-- Note: monomorphism prevent us from saying @commutes (.) q (fmap h)@,
-- since @fmap h@ is used at two different types.

-- | The semantic function ('model1') for @f@ is a 'functorMorphism'.
semanticFunctor :: forall f g.
  ( Model1 f g
  , Functor f
  , Functor g
  , Arbitrary (f NumT)
  , Show (f NumT)
  , EqProp (g T)
  ) =>
  f () -> TestBatch
semanticFunctor = const (functorMorphism (model1 :: forall b. f b -> g b))


-- | Properties to check that the 'Apply' @m@ satisfies the apply
-- properties
apply :: forall m a b c.
         ( Apply m
         , Arbitrary a, CoArbitrary a, Arbitrary b, CoArbitrary b
         , Arbitrary c, Arbitrary (m a)
         , Arbitrary (m (b -> c)), Show (m (b -> c))
         , Arbitrary (m (a -> b)), Show (m (a -> b))
         , Show a, Show (m a)
         , EqProp (m a), EqProp (m b), EqProp (m c)
         ) =>
         m (a,b,c) -> TestBatch
apply = const ( "apply"
              , [ ("associativity", property associativityP)
                , ("left"       , property leftP)
                , ("right"      , property rightP)
                ]
              )
 where
   associativityP :: m (b -> c) -> m (a -> b) -> m a -> Property
   rightP         :: (b -> c) -> m (a -> b) -> m a -> Property
   leftP          :: (a -> b) -> m (b -> c) -> m a -> Property

   associativityP u v w = ((.) <$> u <.> v <.> w) =-= (u <.> (v <.> w))
   leftP f x y          = (x <.> (f <$> y)) =-= ((. f) <$> x <.> y)
   rightP f x y         = (f <$> (x <.> y)) =-= ((f .) <$> x <.> y)


-- | 'Apply' morphism properties
applyMorphism :: forall f g.
                 ( Apply f, Apply g
                 , Show (f NumT), Arbitrary (f NumT)
                 , EqProp (g NumT), EqProp (g T)
                 , Show (f (NumT -> T))
                 , Arbitrary (f (NumT -> T))
                 ) =>
                 (forall a. f a -> g a) -> TestBatch
applyMorphism q =
  ( "apply morphism"
  , [ ("apply", property applyP)] )
 where
   applyP :: f (NumT->T) -> f NumT -> Property
   applyP mf mx = q (mf <.> mx) =-= (q mf <.> q mx)


-- | The semantic function ('model1') for @f@ is an 'applyMorphism'.
semanticApply :: forall f g.
                 ( Model1 f g
                 , Apply f, Apply g
                 , Arbitrary (f NumT), Arbitrary (f (NumT -> T))
                 , EqProp (g NumT), EqProp (g T)
                 , Show (f NumT), Show (f (NumT -> T))
                 ) =>
                 f () -> TestBatch
semanticApply =
  const (applyMorphism (model1 :: forall b. f b -> g b))


-- | Properties to check that the 'Applicative' @m@ satisfies the applicative
-- properties
applicative :: forall m a b c.
               ( Applicative m
               , Arbitrary a, CoArbitrary a, Arbitrary b, Arbitrary (m a)
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
                       , Show (f NumT), Arbitrary (f NumT)
                       , EqProp (g NumT), EqProp (g T)
                       , Show (f (NumT -> T))
                       , Arbitrary (f (NumT -> T))
                       ) =>
                       (forall a. f a -> g a) -> TestBatch
applicativeMorphism q =
  ( "applicative morphism"
  , [("pure", property pureP), ("apply", property applyP)] )
 where
   pureP  :: NumT -> Property
   applyP :: f (NumT->T) -> f NumT -> Property

   pureP a = q (pure a) =-= pure a
   applyP mf mx = q (mf <*> mx) =-= (q mf <*> q mx)


-- | The semantic function ('model1') for @f@ is an 'applicativeMorphism'.
semanticApplicative :: forall f g.
  ( Model1 f g
  , Applicative f, Applicative g
  , Arbitrary (f NumT), Arbitrary (f (NumT -> T))
  , EqProp (g NumT), EqProp (g T)
  , Show (f NumT), Show (f (NumT -> T))
  ) =>
  f () -> TestBatch
semanticApplicative =
  const (applicativeMorphism (model1 :: forall b. f b -> g b))


-- | Properties to check that the 'bind' @m@ satisfies the bind properties
bind :: forall m a b c.
        ( Bind m
        , Show a, Arbitrary a, CoArbitrary a, Arbitrary b, CoArbitrary b
        , Arbitrary (m a), EqProp (m a), Show (m a)
        , Arbitrary (m b), EqProp (m b)
        , Arbitrary (m c), EqProp (m c)
        , Arbitrary (m (m (m a))), Show (m (m (m a)))
        ) =>
        m (a,b,c) -> TestBatch
bind = const ( "bind laws"
              , [ ("join associativity", property joinAssocP)
                , ("bind associativity", property bindAssocP)
                ]
              )
 where
   bindAssocP :: m a -> (a -> m b) -> (b -> m c) -> Property
   joinAssocP :: m (m (m a)) -> Property

   bindAssocP m f g = ((m >>- f) >>- g) =-= (m >>- (\x -> f x >>- g))
   joinAssocP mmma = B.join (B.join mmma) =-= B.join (fmap B.join mmma)

bindApply :: forall m a b.
             ( Apply m, Bind m
             , EqProp (m a), EqProp (m b)
             , Show a, Arbitrary a
             , Show (m a), Arbitrary (m a)
             , Show (m (a -> b)), Arbitrary (m (a -> b))) =>
             m (a, b) -> TestBatch
bindApply = const ( "bind apply"
                  , [ ("ap", property apP) ]
                  )
 where
   apP :: m (a -> b) -> m a -> Property
   apP f x = (f <.> x) =-= (f `apDefault` x)

-- | 'bind' morphism properties
bindMorphism :: forall f g.
                ( Bind f, Bind g, Functor g
                , Show (f NumT)
                , Show (f (NumT -> T))
                , Show (f (f (NumT -> T)))
                , Arbitrary (f NumT), Arbitrary (f T)
                , Arbitrary (f (NumT -> T))
                , Arbitrary (f (f (NumT -> T)))
                , EqProp (g NumT), EqProp (g T)
                , EqProp (g (NumT -> T))
                ) =>
                (forall a. f a -> g a) -> TestBatch
bindMorphism q =
  ( "bind morphism"
  , [ ("bind", property bindP), ("join", property joinP) ] )
 where
   bindP :: f NumT -> (NumT -> f T) -> Property
   joinP :: f (f (NumT->T)) -> Property

   bindP u k = q (u >>- k)  =-= (q u >>- q . k)
   joinP uu  = q (B.join uu)  =-= B.join (fmap q (q uu))

-- | The semantic function ('model1') for @f@ is a 'bindMorphism'.
semanticBind :: forall f g.
  ( Model1 f g
  , Bind f, Bind g
  , EqProp (g T) , EqProp (g NumT)
  , EqProp (g (NumT -> T))
  , Arbitrary (f T) , Arbitrary (f NumT)
  , Arbitrary (f (f (NumT -> T)))
  , Arbitrary (f (NumT -> T))
  , Show (f (f (NumT -> T)))
  , Show (f (NumT -> T)) , Show (f NumT)
  , Functor g
  ) =>
  f () -> TestBatch
semanticBind = const (bindMorphism (model1 :: forall b. f b -> g b))


-- | Properties to check that the 'Monad' @m@ satisfies the monad properties
monad :: forall m a b c.
         ( Monad m
         , Show a, Arbitrary a, CoArbitrary a, Arbitrary b, CoArbitrary b
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

-- | Law for monads that are also instances of 'Functor'.
monadFunctor :: forall m a b.
                ( Functor m, Monad m
                , Arbitrary a, Arbitrary b, CoArbitrary a
                , Arbitrary (m a), Show (m a), EqProp (m b)) =>
                m (a, b) -> TestBatch
monadFunctor = const ( "monad functor"
                     , [("bind return", property bindReturnP)])
 where
   bindReturnP :: (a -> b) -> m a -> Property
   bindReturnP f xs = fmap f xs =-= (xs >>= return . f)

monadApplicative :: forall m a b.
                    ( Applicative m, Monad m
                    , EqProp (m a), EqProp (m b)
                    , Show a, Arbitrary a
                    , Show (m a), Arbitrary (m a)
                    , Show (m (a -> b)), Arbitrary (m (a -> b))) =>
                    m (a, b) -> TestBatch
monadApplicative = const ( "monad applicative"
                         , [ ("pure", property pureP)
                           , ("ap", property apP)
                           ]
                         )
 where
   pureP :: a -> Property
   apP :: m (a -> b) -> m a -> Property

   pureP x = (pure x :: m a) =-= return x
   apP f x = (f <*> x) =-= (f `ap` x)

-- | 'Monad' morphism properties

-- | 'Applicative' morphism properties
monadMorphism :: forall f g.
                 ( Monad f, Monad g, Functor g
                 , Show (f NumT)
                 , Show (f (NumT -> T))
                 , Show (f (f (NumT -> T)))
                 , Arbitrary (f NumT), Arbitrary (f T)
                 , Arbitrary (f (NumT -> T))
                 , Arbitrary (f (f (NumT -> T)))
                 , EqProp (g NumT), EqProp (g T)
                 , EqProp (g (NumT -> T))
                 ) =>
                (forall a. f a -> g a) -> TestBatch
monadMorphism q =
  ( "monad morphism"
  , [ ("return", property returnP), ("bind", property bindP), ("join", property joinP) ] )
 where
   returnP :: NumT -> Property
   bindP :: f NumT -> (NumT -> f T) -> Property
   joinP :: f (f (NumT->T)) -> Property

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

-- | The semantic function ('model1') for @f@ is a 'monadMorphism'.
semanticMonad :: forall f g.
  ( Model1 f g
  , Monad f, Monad g
  , EqProp (g T) , EqProp (g NumT)
  , EqProp (g (NumT -> T))
  , Arbitrary (f T) , Arbitrary (f NumT)
  , Arbitrary (f (f (NumT -> T)))
  , Arbitrary (f (NumT -> T))
  , Show (f (f (NumT -> T)))
  , Show (f (NumT -> T)) , Show (f NumT)
  , Functor g
  ) =>
  f () -> TestBatch
semanticMonad = const (monadMorphism (model1 :: forall b. f b -> g b))

-- | Laws for MonadPlus instances with left distribution.
monadPlus :: forall m a b.
             ( MonadPlus m, Show (m a)
             , Arbitrary a, CoArbitrary a, Arbitrary (m a), Arbitrary (m b)
             , EqProp (m a), EqProp (m b)) =>
             m (a, b) -> TestBatch
monadPlus = const ( "MonadPlus laws"
                  , [ ("left zero", property leftZeroP)
                    , ("left identity", leftId mplus (mzero :: m a))
                    , ("right identity", rightId mplus (mzero :: m a))
                    , ("associativity" , isAssoc (mplus :: Binop (m a)))
                    , ("left distribution", property leftDistP)
                    ]
                  )
 where
   leftZeroP :: (a -> m b) -> Property
   leftDistP :: m a -> m a -> (a -> m b) -> Property

   leftZeroP k = (mzero >>= k) =-= mzero
   leftDistP a b k = (a `mplus` b >>= k) =-= ((a >>= k) `mplus` (b >>= k))

-- | Laws for MonadPlus instances with left catch.
monadOr :: forall m a b.
           ( MonadPlus m, Show a, Show (m a)
           , Arbitrary a, CoArbitrary a, Arbitrary (m a), Arbitrary (m b)
           , EqProp (m a), EqProp (m b)) =>
           m (a, b) -> TestBatch
monadOr = const ( "MonadOr laws"
                , [ ("left zero", property leftZeroP)
                  , ("left identity", leftId mplus (mzero :: m a))
                  , ("right identity", rightId mplus (mzero :: m a))
                  , ("associativity" , isAssoc (mplus :: Binop (m a)))
                  , ("left catch", property leftCatchP)
                  ]
                )
 where
   leftZeroP :: (a -> m b) -> Property
   leftCatchP :: a -> m a -> Property

   leftZeroP k = (mzero >>= k) =-= mzero
   leftCatchP a b = return a `mplus` b =-= return a

-- | Check Alt Semigroup law
alt :: forall f a. ( Alt f, Arbitrary a, Arbitrary (f a)
                   , EqProp (f a), Show (f a)) =>
       f a -> TestBatch
alt = const ( "Alt laws"
            , [ ("associativity", isAssoc ((<!>) :: Binop (f a))) ] )


-- | Check Alternative Monoid laws
alternative :: forall f a. ( Alternative f, Arbitrary a, Arbitrary (f a)
                           , EqProp (f a), Show (f a)) =>
               f a -> TestBatch
alternative = const ( "Alternative laws"
                    , [ ("left identity", leftId (<|>) (empty :: f a))
                      , ("right identity", rightId (<|>) (empty :: f a))
                      , ("associativity", isAssoc ((<|>) :: Binop (f a)))
                      ]
                    )


arrow :: forall a b c d e.
         ( Arrow a
         , Show (a d e), Show (a c d), Show (a b c)
         , Show b, Show c, Show d, Show e
         , Arbitrary (a d e), Arbitrary (a c d), Arbitrary (a b c)
         , Arbitrary b, Arbitrary c, Arbitrary d, Arbitrary e
         , CoArbitrary b, CoArbitrary c, CoArbitrary d
         , EqProp (a b e), EqProp (a b d)
         , EqProp (a (b,d) c)
         , EqProp (a (b,d) (c,d)), EqProp (a (b,e) (d,e))
         , EqProp (a (b,d) (c,e))
         , EqProp b, EqProp c, EqProp d, EqProp e
         ) =>
         a b (c,d,e) -> TestBatch
arrow = const ("arrow laws"
              , [ ("associativity"           , property assocP)
                , ("arr distributes"         , property arrDistributesP)
-- TODO: how to define h is onto or one-to-one?
--                , ("extensionality principle"     , property extensionalityP)
--                , ("extensionality dual"          , property extensionalityDualP)
                 , ("first works as funs"    , property firstAsFunP)
                 , ("first keeps composition", property firstKeepCompP)
                 , ("first works as fst"     , property firstIsFstP)
                 , ("second can move"        , property secondMovesP)
                 ]
              )
  where
    assocP :: a b c -> a c d -> a d e -> Property
    assocP f g h = ((f >>> g) >>> h) =-= (f >>> (g >>> h))

    arrDistributesP :: (b -> c) -> (c -> d) -> Property
    arrDistributesP f g = ((arr (f >>> g)) :: a b d) =-= (arr f >>> arr g)

    firstAsFunP :: (b -> c) -> Property
    firstAsFunP f = (first (arr f) :: a (b,d) (c,d)) =-= arr (first f)

    firstKeepCompP :: a b c -> a c d -> Property
    firstKeepCompP f g =
      ((first (f >>> g)) :: (a (b,e) (d,e))) =-= (first f >>> first g)

    firstIsFstP :: a b c -> Property
    firstIsFstP f = ((first f :: a (b,d) (c,d)) >>> arr fst)
                      =-= (arr fst >>> f)

    secondMovesP :: (a b c) -> (d -> e) -> Property
    secondMovesP f g = (first f >>> second (arr g))
                         =-= ((second (arr g)) >>> first f)

arrowChoice :: forall a b c d e.
               ( ArrowChoice a
               , Show (a b c)
               , Arbitrary (a b c)
               , Arbitrary b, Arbitrary c, Arbitrary d, Arbitrary e
               , CoArbitrary b, CoArbitrary d
               , EqProp (a (Either b d) (Either c e))
               , EqProp (a (Either b d) (Either c d))
               ) =>
               a b (c,d,e) -> TestBatch
arrowChoice = const ("arrow choice laws"
                    , [ ("left works as funs", property leftAsFunP)
                      , ("right can move"    , property rightMovesP)
                      ]
                    )
  where
    leftAsFunP :: (b -> c) -> Property
    leftAsFunP f = (left (arr f) :: a (Either b d) (Either c d))
                     =-= arr (left f)

    rightMovesP :: (a b c) -> (d -> e) -> Property
    rightMovesP f g = (left f >>> right (arr g))
                        =-= ((right (arr g)) >>> left f)

traversable :: forall f a b m.
               ( Traversable f, Monoid m, Show (f a)
               , Arbitrary (f a), Arbitrary b, Arbitrary a, Arbitrary m
               , CoArbitrary a
               , EqProp (f b), EqProp m) =>
               f (a, b, m) -> TestBatch
traversable = const ( "traversable"
                    , [ ("fmap", property fmapP)
                      , ("foldMap", property foldMapP)
                      ]
                    )
 where
   fmapP :: (a -> b) -> f a -> Property
   foldMapP :: (a -> m) -> f a -> Property

   fmapP f x = f `fmap` x =-= f `fmapDefault` x
   foldMapP f x = f `foldMap` x =-= f `foldMapDefault` x

-- | Note that 'foldable' doesn't check the strictness of 'foldl'', `foldr'' and `foldMap''.
foldable :: forall t a b m n o.
            ( Foldable t, CoArbitrary a, CoArbitrary b, Arbitrary b
            , Arbitrary (t a), Show b, Show (t a), EqProp b, Monoid m
            , Arbitrary (t m), Show (t m), EqProp m, Arbitrary (t n)
            , Show (t n), Num n, EqProp n, Arbitrary a, Show a, Eq a
            , Ord o, Arbitrary (t o), Show (t o), EqProp o, EqProp a) =>
            t (a, b, m, n, o) -> TestBatch
foldable = const ( "Foldable"
                 , [ ("foldr and foldMap", property foldrFoldMapP)
                   , ("foldl and foldMap", property foldlFoldMapP)
                   , ("fold and foldMap", property foldFoldMapP)
                   , ("length", property lengthP)
#if MIN_VERSION_base(4,13,0)
                   , ("foldMap'", property foldMap'P)
#endif
                   , ("foldr'", property foldr'P)
                   , ("foldl'", property foldl'P)
                   , ("foldr1", property foldr1P)
                   , ("foldl1", property foldl1P)
                   , ("toList", property toListP)
                   , ("null", property nullP)
                   , ("elem", property elemP)
                   , ("maximum", property maximumP)
                   , ("minimum", property minimumP)
                   , ("sum", property sumP)
                   , ("product", property productP)
                   ]
                 )
  where
    foldrFoldMapP :: (a -> b -> b) -> b -> t a -> Property
    foldrFoldMapP f z t = foldr f z t =-= appEndo (foldMap (Endo . f) t ) z
    foldlFoldMapP :: (b -> a -> b) -> b -> t a -> Property
    foldlFoldMapP f z t = foldl f z t =-= appEndo (getDual (foldMap (Dual . Endo . flip f) t)) z
    foldFoldMapP :: t m -> Property
    foldFoldMapP t = fold t =-= foldMap id t
    lengthP :: t a -> Property
    lengthP t = length t =-= (getSum . foldMap (Sum . const  1)) t
#if MIN_VERSION_base(4,13,0)
    -- TODO: Check strictness
    foldMap'P :: (a -> m) -> t a -> Property
    foldMap'P f t = foldMap' f t =-= foldl' (\acc a -> acc <> f a) mempty t
#endif
    sumP :: t n -> Property
    sumP t = sum t =-= (getSum . foldMap Sum) t
    productP :: t n -> Property
    productP t = product t =-= (getProduct . foldMap Product) t
    maximumP :: t o -> Property
    maximumP t = not (null t) ==> maximum t =-= maximum (toList t)
    minimumP :: t o -> Property
    minimumP t = not (null t) ==> minimum t =-= minimum (toList t)
    foldr1P :: (a -> a -> a) -> t a -> Property
    foldr1P f t = not (null t) ==> foldr1 f t =-= foldr1 f (toList t)
    foldl1P :: (a -> a -> a) -> t a -> Property
    foldl1P f t = not (null t) ==> foldl1 f t =-= foldl1 f (toList t)
    toListP :: t a -> Property
    toListP t = toList t =-= foldr (:) [] t
    nullP :: t a -> Property
    nullP t = null t =-= foldr (const (const False)) True t
    -- TODO: Check strictness
    foldr'P :: (a -> b -> b) -> b -> t a -> Property
    foldr'P f z t = foldr' f z t =-= foldr' f z (toList t)
    -- TODO: Check strictness
    foldl'P :: (b -> a -> b) -> b -> t a -> Property
    foldl'P f z t = foldl' f z t =-= foldl' f z (toList t)
    elemP :: a -> t a -> Property
    elemP a t = elem a t =-= elem a (toList t)

foldableFunctor :: forall t a m.
                   ( CoArbitrary a, Foldable t, EqProp m, Functor t, Arbitrary m
                   , Monoid m, Arbitrary (t a), Show (t a)) =>
                   t (a, m) -> TestBatch
foldableFunctor = const ( "Foldable Functor"
                        , [ ("foldMap f = fold . fmap f", property foldMapP) ]
                        )
  where
    foldMapP :: (a -> m) -> t a -> Property
    foldMapP f t = foldMap f t =-= fold (fmap f t)
