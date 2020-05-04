{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances
           , FlexibleContexts, TypeSynonymInstances, GeneralizedNewtypeDeriving
           , UndecidableInstances, ScopedTypeVariables, DefaultSignatures
           , TypeOperators, CPP, TypeFamilies
  #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

----------------------------------------------------------------------
-- |
-- Module      :  Test.QuickCheck.Checkers
-- Copyright   :  (c) Conal Elliott 2007,2008
-- License     :  BSD3
--
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
--
-- Some QuickCheck helpers
----------------------------------------------------------------------

module Test.QuickCheck.Checkers
  (
  -- * Misc
    Test, TestBatch, unbatch, checkBatch, quickBatch, verboseBatch
  -- , probablisticPureCheck
  , Unop, Binop, genR, involution, inverseL, inverse
  , FracT, NumT, OrdT, T
  -- * Generalized equality
  , EqProp(..), eq
  , BinRel, reflexive, transitive, symmetric, antiSymmetric
  , leftId, rightId, bothId, isAssoc, isCommut, commutes
  , MonoidD, monoidD, endoMonoidD, homomorphism
  , idempotent, idempotent2, idemElem
  -- , funEq, AsFun(..)
  -- * Model-based (semantics-based) testing
  , Model(..)
  , meq, meq1, meq2, meq3, meq4, meq5
  , eqModels, denotationFor
  , Model1(..)
  -- * Some handy testing types
  -- , Positive, NonZero(..), NonNegative(..)
  -- , suchThat, suchThatMaybe
  , arbs, gens
  , (.&.)
  , arbitrarySatisfying
  ) where

import Data.Word
import Data.List.NonEmpty (NonEmpty)
import Data.Bifunctor (bimap)
import Data.Function (on)
import Control.Applicative
import Control.Arrow (first)
import qualified Control.Exception as Ex
import Data.List (foldl')
import Data.List.NonEmpty (NonEmpty (..))
import Data.Monoid hiding (
#if __GLASGOW_HASKELL__ < 810
    First, Last
#endif
                          )

import Data.Complex
import Data.Proxy
import Data.Ratio
import Data.Functor.Identity

#if __GLASGOW_HASKELL__ >= 800
import Data.Functor.Compose
import qualified Data.Functor.Product as F
import qualified Data.Functor.Sum as F
#endif
import Data.Semigroup
import GHC.Generics
import System.Random
import Test.QuickCheck hiding (generate)
import Test.QuickCheck.Random (QCGen, newQCGen)
-- import System.IO.Unsafe

import Test.QuickCheck.Gen      (Gen (..)) -- for rand
-- import Test.QuickCheck.Property (Prop(..)) -- for evaluate

import Test.QuickCheck.Utils

-- import Test.QuickCheck.Utils
-- import Test.QuickCheck.Instances.Num
-- import Control.Monad.Extensions


-- import qualified Data.Stream as S


{----------------------------------------------------------
    Misc
----------------------------------------------------------}

-- | Named test
type Test = (String,Property)

-- | Named batch of tests
type TestBatch = (String,[Test])

-- | Flatten a test batch for inclusion in another
unbatch :: TestBatch -> [Test]
unbatch (batchName,props) = map (first ((batchName ++ ": ")++)) props

-- TODO: consider a tree structure so that flattening is unnecessary.

-- | Run a batch of tests.  See 'quickBatch' and 'verboseBatch'.
checkBatch :: Args -> TestBatch -> IO ()
checkBatch args (name,tests) =
  do putStrLn $ "\n" ++ name ++ ":"
     mapM_ pr tests
 where
   pr (s,p) = do putStr (padTo (width + 4) ("  "++s ++ ":"))
                 Ex.catch (quickCheckWith args p)
                          (print :: Ex.SomeException -> IO ())
   width    = foldl' max 0 (map (length.fst) tests)


padTo :: Int -> String -> String
padTo n = take n . (++ repeat ' ')

-- | Check a batch tersely.
quickBatch :: TestBatch -> IO ()
quickBatch = checkBatch quick'

-- | Check a batch verbosely.
verboseBatch :: TestBatch -> IO ()
verboseBatch = checkBatch verbose'

quick', verbose' :: Args
quick'   = stdArgs { maxSuccess = 500 }
verbose' = quick'
           -- quick' { configEvery = \ n args -> show n ++ ":\n" ++ unlines args }

-- TODO: Restore verbose functionality.  How in QC2?

{-

-- TODO: change TestBatch to be hierarchical/recursive, rather than
-- two-level.

data Batch n t = Test t | Batch [LBatch n t]
type LBatch n t = (n, Batch n t)

-- | Run a batch of tests.  See 'quickBatch' and 'verboseBatch'.
checkL :: Config -> LBatch -> IO ()
checkL config = checkL' 0
 where
   checkL' :: Int -> LBatch -> IO ()
   ...
-}

-- | Unary function, handy for type annotations
type Unop a = a -> a

-- | Binary function, handy for type annotations
type Binop a = a -> a -> a

-- Testing types

-- | Token 'Fractional' type for tests
type FracT = Float
-- | Token 'Num' type for tests
type NumT  = Int
-- | Token 'Ord' type for tests
type OrdT  = Int -- Char -- randomR is broken on Char
-- | Token uninteresting type for tests
type T     = Char

genR :: Random a => (a, a) -> Gen a
genR (lo,hi) = fmap (fst . randomR (lo,hi)) rand

-- | @f@ is its own inverse. See also 'inverse'.
involution :: (Show a, Arbitrary a, EqProp a) =>
              (a -> a) -> Property
involution f = f `inverseL` f

-- | @f@ is a left inverse of @g@.  See also 'inverse'.
inverseL :: (EqProp b, Arbitrary b, Show b) =>
            (a -> b) -> (b -> a) -> Property
f `inverseL` g = f . g =-= id

-- | @f@ is a left and right inverse of @g@.  See also 'inverseL'.
inverse :: ( EqProp a, Arbitrary a, Show a
           , EqProp b, Arbitrary b, Show b ) =>
           (a -> b) -> (b -> a) -> Property
f `inverse` g = f `inverseL` g .&. g `inverseL` f


{----------------------------------------------------------
    Generalized equality
----------------------------------------------------------}

infix  4 =-=

-- | Types of values that can be tested for equality, perhaps through
-- random sampling.
class EqProp a where
  (=-=) :: a -> a -> Property
  default (=-=) :: (Generic a, GEqProp (Rep a)) => a -> a -> Property
  (=-=) = geq `on` from
  {-# INLINEABLE (=-=) #-}

class GEqProp g where
  geq :: g x -> g x -> Property

instance GEqProp g => GEqProp (M1 _1 _2 g) where
  geq = geq `on` unM1
  {-# INLINEABLE geq #-}

instance (GEqProp g1, GEqProp g2) => GEqProp (g1 :*: g2) where
  geq (g1a :*: g1b) (g2a :*: g2b) = geq g1a g2a .&&. geq g1b g2b
  {-# INLINEABLE geq #-}

instance (GEqProp g1, GEqProp g2) => GEqProp (g1 :+: g2) where
  geq (L1 g1) (L1 g2) = geq g1 g2
  geq (R1 g1) (R1 g2) = geq g1 g2
  geq _ _             = property False
  {-# INLINEABLE geq #-}

instance EqProp a => GEqProp (K1 _1 a) where
  geq = (=-=) `on` unK1
  {-# INLINEABLE geq #-}

instance GEqProp U1 where
  geq U1 U1 = property True
  {-# INLINEABLE geq #-}

instance GEqProp V1 where
  geq _ _ = property True
  {-# INLINEABLE geq #-}

-- | For 'Eq' types as 'EqProp' types
eq :: Eq a => a -> a -> Property
a `eq` a' = property (a == a')


-- Template: fill in with Eq types for a
--   instance EqProp a where (=-=) = eq
-- E.g.,

instance EqProp ()
instance EqProp Bool
instance EqProp Char where (=-=) = eq
instance EqProp Ordering

-- Numeric
instance EqProp Int     where (=-=) = eq
instance EqProp Float   where (=-=) = eq
instance EqProp Double  where (=-=) = eq
instance EqProp Integer where (=-=) = eq
instance Eq a => EqProp (Complex a) where (=-=) = eq
instance Eq a => EqProp (Ratio a) where (=-=) = eq

-- Semigroups
instance EqProp a => EqProp (Min a)
instance EqProp a => EqProp (Max a)
instance EqProp a => EqProp (First a)
instance EqProp a => EqProp (Last a)

-- Monoids
instance EqProp a => EqProp (Dual a)
instance (Show a, Arbitrary a, EqProp a) => EqProp (Endo a)
instance EqProp All
instance EqProp Any
instance EqProp a => EqProp (Sum a)
instance EqProp a => EqProp (Product a)
instance EqProp (f a) => EqProp (Alt f a)
#if __GLASGOW_HASKELL__ >= 806
instance EqProp (f a) => EqProp (Ap f a)
#endif

-- Lists
instance EqProp a => EqProp [a]
instance EqProp a => EqProp (NonEmpty a)
instance EqProp a => EqProp (ZipList a)

-- Maybe
instance EqProp a => EqProp (Maybe a)

-- Pairing
instance (EqProp a, EqProp b) => EqProp (a,b)
instance (EqProp a, EqProp b, EqProp c) => EqProp (a,b,c)
instance (EqProp a, EqProp b, EqProp c, EqProp d) => EqProp (a,b,c,d)

-- Either
instance (EqProp a, EqProp b) => EqProp (Either a b)

-- Functors
#if __GLASGOW_HASKELL__ >= 800
instance EqProp (f (g a)) => EqProp (Compose f g a)
instance (EqProp (f a), EqProp (g a)) => EqProp (F.Sum f g a)
instance (EqProp (f a), EqProp (g a)) => EqProp (F.Product f g a)
#endif
instance EqProp a => EqProp (Identity a)
instance EqProp a => EqProp (Const a b)
instance EqProp (Proxy a)

-- Function equality
instance (Show a, Arbitrary a, EqProp b) => EqProp (a -> b) where
  f =-= f' = property (liftA2 (=-=) f f')
-- Alternative definition:
-- instance (Show a, Arbitrary a, EqProp b) => EqProp (a -> b) where
--   f =-= f' = property (probablisticPureCheck defaultConfig
--                                              (\x -> f x =-= g x))

eqModels :: (Model a, EqProp (ModelOf a)) => a -> a -> Property
eqModels = (=-=) `on` model


-- | @f `'denotationFor'` g@ proves that @f@ is a model for @g@, ie that
-- @'model' . g '=-=' f@.
denotationFor
    :: (Model b, Arbitrary a, EqProp (ModelOf b), Show a)
    => (a -> ModelOf b)
    -> (a -> b)
    -> TestBatch
denotationFor f g =
  ( "denotation"
  , [("eq", model . g =-= f)]
  )

-- Other types
-- instance EqProp a => EqProp (S.Stream a) where (=-=) = eqModels

-- Binary relation
type BinRel  a = a -> a -> Bool

-- | Reflexive property: @a `rel` a@
reflexive :: (Arbitrary a, Show a) =>
             BinRel a -> Property
reflexive rel = property $ \ a -> a `rel` a

-- | Transitive property: @a `rel` b && b `rel` c ==> a `rel` c@.
-- Generate @a@ randomly, but use @gen a@ to generate @b@ and @gen b@ to
-- generate @c@.  @gen@ ought to satisfy @rel@ fairly often.
transitive :: (Arbitrary a, Show a) =>
              BinRel a -> (a -> Gen a) -> Property
transitive rel gen =
  property $ \ a ->
    forAll (gen a) $ \ b ->
      forAll (gen b) $ \ c ->
        (a `rel` b) && (b `rel` c) ==> (a `rel` c)

-- | Symmetric property: @a `rel` b ==> b `rel` a@.  Generate @a@
-- randomly, but use @gen a@ to generate @b@.  @gen@ ought to satisfy
-- @rel@ fairly often.
symmetric :: (Arbitrary a, Show a) =>
             BinRel a -> (a -> Gen a) -> Property
symmetric rel gen =
  property $ \ a ->
    forAll (gen a) $ \ b ->
      (a `rel` b) ==> (b `rel` a)

-- | Antisymmetric property: @(a `rel` b) && (a /= b) ==> not (b `rel` a)@.
--
-- @since 0.5.0
antiSymmetric :: (Arbitrary a, Show a, Eq a) =>
                 BinRel a -> Property
antiSymmetric rel =
  property $ \ a b -> (a `rel` b) && (a /= b) ==> not (b `rel` a)

-- | Has a given left identity, according to '(=-=)'
leftId :: (Show a, Arbitrary a, EqProp a) => (i -> a -> a) -> i -> Property
leftId  op i = (i `op`) =-= id

-- | Has a given right identity, according to '(=-=)'
rightId :: (Show a, Arbitrary a, EqProp a) => (a -> i -> a) -> i -> Property
rightId op i = (`op` i) =-= id

-- | Has a given left and right identity, according to '(=-=)'
bothId :: (Show a, Arbitrary a, EqProp a) => (a -> a -> a) -> a -> Property
bothId = (liftA2.liftA2) (.&.) leftId rightId

-- bothId op i = leftId op i .&. rightId op i

-- | Associative, according to '(=-=)'
isAssoc :: (EqProp a, Show a, Arbitrary a) => (a -> a -> a) -> Property
isAssoc = isAssociativeBy (=-=) arbitrary

-- | Commutative, according to '(=-=)'
commutes :: EqProp z => (a -> a -> z) -> a -> a -> Property
commutes (#) a b = a # b =-= b # a

-- | Commutative, according to '(=-=)'
isCommut :: (EqProp a, Show a, Arbitrary a) => (a -> a -> a) -> Property
isCommut = isCommutableBy (=-=) arbitrary

-- | Explicit 'Monoid' dictionary.  Doesn't have to correspond to an
-- actual 'Monoid' instance, though see 'monoidD'.
data MonoidD a = MonoidD a (a -> a -> a)

-- | 'Monoid' dictionary built from the 'Monoid' methods.
monoidD :: Monoid a => MonoidD a
monoidD = MonoidD mempty mappend

-- | Monoid dictionary for an unwrapped endomorphism.  See also 'monoidD'
-- and 'Endo'.
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

-- | The unary function @f@ is idempotent, i.e., @f . f == f@
idempotent :: (Show a, Arbitrary a, EqProp a) =>
               (a -> a) -> Property
idempotent f = idemElem (.) f

-- | A binary function @op@ is idempotent, i.e., @x `op` x == x@, for all @x@
idempotent2 :: (Show a, Arbitrary a, EqProp a) =>
               (a -> a -> a) -> Property
idempotent2 = property . idemElem

-- | A binary function @op@ is has an idempotent element @x@, i.e.,
-- @x `op` x == x@
idemElem :: EqProp a => (a -> a -> a) -> a -> Property
idemElem op x = x `op` x =-= x

{-
-- TODO: phase out AsFun, in favor of Model. withArray

-- | Types that can be modeled as functions.
class AsFun h a b | h -> a b where
  asFun :: h -> (a -> b)

instance AsFun (a->b) a b where asFun = id

-- | Equality of function-like types
funEq :: (AsFun h a b, EqProp (a -> b)) => h -> h -> Property
h `funEq` h' = asFun h =-= asFun h'
-}


{----------------------------------------------------------
    Model-based (semantics-based) testing
----------------------------------------------------------}

---- From bytestring

class Model a where
  type ModelOf a
  model :: a -> ModelOf a  -- get the model from a concrete value

-- note: bytestring doesn't make the fundep

---- Compare representation-level and model-level operations (commuting diagrams)

meq  :: (Model a, EqProp (ModelOf a)) => a -> ModelOf a -> Property
meq1 :: (Model a, Model a1, EqProp (ModelOf a)) =>
        (a1 -> a) -> (ModelOf a1 -> ModelOf a) -> a1 -> Property
meq2 :: (Model a, Model a1, Model a2, EqProp (ModelOf a)) =>
        (a1 -> a2 -> a) -> (ModelOf a1 -> ModelOf a2 -> ModelOf a) -> a1 -> a2 -> Property
meq3 :: (Model a, Model a1, Model a2, Model a3, EqProp (ModelOf a)) =>
        (a1 -> a2 -> a3 -> a)
     -> (ModelOf a1 -> ModelOf a2 -> ModelOf a3 -> ModelOf a)
     -> a1 -> a2 -> a3 -> Property
meq4 :: ( Model a, Model a1, Model a2
        , Model a3, Model a4, EqProp (ModelOf a)) =>
        (a1 -> a2 -> a3 -> a4 -> a)
     -> (ModelOf a1 -> ModelOf a2 -> ModelOf a3 -> ModelOf a4 -> ModelOf a)
     -> a1 -> a2 -> a3 -> a4 -> Property
meq5 :: ( Model a, Model a1, Model a2, Model a3
        , Model a4, Model a5, EqProp (ModelOf a)) =>
        (a1 -> a2 -> a3 -> a4 -> a5 -> a)
     -> (ModelOf a1 -> ModelOf a2 -> ModelOf a3 -> ModelOf a4 -> ModelOf a5 -> ModelOf a)
     -> a1 -> a2 -> a3 -> a4 -> a5 -> Property

meq  a b =
     model a             =-= b
meq1 f g = \a         ->
     model (f a)         =-= g (model a)
meq2 f g = \a b       ->
     model (f a b)       =-= g (model a) (model b)
meq3 f g = \a b c     ->
     model (f a b c)     =-= g (model a) (model b) (model c)
meq4 f g = \a b c d   ->
     model (f a b c d)   =-= g (model a) (model b) (model c) (model d)
meq5 f g = \a b c d e ->
     model (f a b c d e) =-= g (model a) (model b) (model c) (model d) (model e)


---- Some model instances

instance Model () where
  type ModelOf () = ()
  model = id
instance Model Bool where
  type ModelOf Bool = Bool
  model = id
instance Model Char where
  type ModelOf Char = Char
  model = id
instance Model Int where
  type ModelOf Int = Int
  model = id
instance Model Integer where
  type ModelOf Integer = Integer
  model = id
instance Model Float where
  type ModelOf Float = Float
  model = id
instance Model Double where
  type ModelOf Double = Double
  model = id
instance Model Word where
  type ModelOf Word = Word
  model = id
instance Model Word8 where
  type ModelOf Word8 = Word8
  model = id
instance Model Word16 where
  type ModelOf Word16 = Word16
  model = id
instance Model Word32 where
  type ModelOf Word32 = Word32
  model = id
instance Model Word64 where
  type ModelOf Word64 = Word64
  model = id

-- These next several require UndecidableInstances
instance (Model a1, Model a2) => Model (a1, a2) where
  type ModelOf (a1, a2) = (ModelOf a1, ModelOf a2)
  model (a1, a2) = (model a1, model a2)

instance (Model a1, Model a2, Model a3) => Model (a1, a2, a3) where
  type ModelOf (a1, a2, a3) = (ModelOf a1, ModelOf a2, ModelOf a3)
  model (a1, a2, a3) = (model a1, model a2, model a3)

instance (Model a1, Model a2, Model a3, Model a4) => Model (a1, a2, a3, a4) where
  type ModelOf (a1, a2, a3, a4) = (ModelOf a1, ModelOf a2, ModelOf a3, ModelOf a4)
  model (a1, a2, a3, a4) = (model a1, model a2, model a3, model a4)

instance Model b => Model (a -> b) where
  type ModelOf (a -> b) = a -> ModelOf b
  model f = model . f

instance Model a => Model (Maybe a) where
  type ModelOf (Maybe a) = Maybe (ModelOf a)
  model = fmap model

instance Model a => Model [a] where
  type ModelOf [a] = [ModelOf a]
  model = fmap model

instance Model a => Model (NonEmpty a) where
  type ModelOf (NonEmpty a) = NonEmpty (ModelOf a)
  model = fmap model

instance (Model a, Model b) => Model (Either a b) where
  type ModelOf (Either a b) = Either (ModelOf a) (ModelOf b)
  model = bimap model model

-- instance Model (S.Stream a) (NonNegative Int -> a) where
--   model s (NonNegative i) = s S.!! i


-- | Like 'Model' but for unary type constructors.
class Model1 f g | f -> g where
  model1 :: forall a. f a -> g a


{----------------------------------------------------------
    Some handy testing types
----------------------------------------------------------}

-- from QC2, plus tweaks

-- type Positive a = NonZero (NonNegative a)

arbitrarySatisfying :: Arbitrary a => (a -> Bool) -> Gen a
arbitrarySatisfying = (arbitrary `suchThat`)

-- -- | Generates a value that satisfies a predicate.
-- suchThat :: Gen a -> (a -> Bool) -> Gen a
-- gen `suchThat` p = satisfiesM p gen

-- -- | Tries to generate a value that satisfies a predicate.
-- suchThatMaybe :: Gen a -> (a -> Bool) -> Gen (Maybe a)
-- gen `suchThatMaybe` p = sized (try 0 . max 1)
--  where
--   try _ 0 = return Nothing
--   try k n = do x <- resize (2*k+n) gen
--                if p x then return (Just x) else try (k+1) (n-1)

-- | Generate n arbitrary values
arbs :: Arbitrary a => Int -> IO [a]

arbs n = fmap (\ rnd -> generate n rnd (vector n)) newQCGen

-- | Produce n values from a generator
gens :: Int -> Gen a -> IO [a]
gens n gen =
  fmap (\ rnd -> generate 1000 rnd (sequence (replicate n gen))) newQCGen

-- The next two are from twanvl:

instance Testable a => Testable [a] where
  property []    = property True
  property props = property $ \n -> props !! (n `mod` len)
    where len = length props

instance (Testable a, Testable b) => Testable (a,b) where
  property = uncurry (.&.)

{-
probablisticPureCheck :: Testable a => Args -> a -> Bool
probablisticPureCheck args a = unsafePerformIO $
  do rnd <- newStdGen
     probablisticPureTests args (evaluate a) rnd 0 0 []


probablisticPureTests :: Args
                      -> Gen Result
                      -> StdGen
                      -> Int
                      -> Int
                      -> [[String]]
                      -> IO Bool
probablisticPureTests args gen rnd0 ntest nfail stamps
  | ntest == maxSuccess args = return True
  | nfail == maxDiscard args = return True
  | otherwise                     =
      case ok result of
        Nothing    ->
          probablisticPureTests args gen rnd1 ntest (nfail+1) stamps
        Just True  ->
          probablisticPureTests args gen rnd1 (ntest+1) nfail
                                (stamp result:stamps)
        Just False ->
          return False
     where
      result      = generate (maxSize config ntest) rnd2 gen
      (rnd1,rnd2) = split rnd0

-}

-- TODO: resurrect probablistic stuff.  bob?


{--------------------------------------------------------------------
    Copied (& tweaked) from QC1
--------------------------------------------------------------------}

-- TODO: are there QC2 replacements for these QC1 operations?

rand :: Gen QCGen
rand = MkGen (\r _ -> r)

generate :: Int -> QCGen -> Gen a -> a
generate n rnd (MkGen m) = m rnd' size
 where
  (size, rnd') = randomR (0, n) rnd

-- evaluate :: Testable a => a -> Gen Result
-- evaluate a = gen where MkProp gen = property a
