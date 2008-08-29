{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances
           , FlexibleContexts, TypeSynonymInstances, GeneralizedNewtypeDeriving
           , UndecidableInstances, ScopedTypeVariables
  #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

----------------------------------------------------------------------
-- |
-- Module      :  Test.QuickCheck.Help
-- Copyright   :  (c) Conal Elliott 2007,2008
-- License     :  BSD3
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Some QuickCheck helpers
----------------------------------------------------------------------

module Test.QuickCheck.Help
  (
  -- * Misc
    Test, TestBatch, unbatch, checkBatch, quickBatch, verboseBatch
  , probablisticPureCheck
  , Unop, Binop, genR, inverseL, inverse
  , FractionalType, NumericType, OrderableType, NoPropertyType
  -- * Generalized equality
  , EqProp(..), eq
  , leftId, rightId, bothId, isAssoc, isCommut, commutes
  , MonoidD, monoidD, endoMonoidD, homomorphism
  , idempotent, idempotent2, idemElem
  -- , funEq, AsFun(..)
  -- * Model-based (semantics-based) testing
  , Model(..)
  , meq, meq1, meq2, meq3, meq4, meq5
  , eqModels
  , Model1(..)
  -- * Some handy testing types
  , Positive, NonZero(..), NonNegative(..)
  , suchThat, suchThatMaybe
  , arbs, gens
  , (.&.)
  , arbitrarySatisfying
  ) where

-- import Data.Function (on)
import Data.Monoid
import Data.Function (on)
import Control.Applicative
import Control.Arrow ((***),first)
import Data.List (foldl')
import System.Random
import Test.QuickCheck
import System.IO.Unsafe

import Test.QuickCheck.Utils
import Test.QuickCheck.Applicative ()
import Test.QuickCheck.Instances.Num
import Control.Monad.Extensions


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
checkBatch :: Config -> TestBatch -> IO ()
checkBatch config (name,tests) =
  do putStrLn $ "\n" ++ name ++ ":"
     mapM_ pr tests
 where
   pr (s,p) = do putStr (padTo (width + 4) ("  "++s ++ ":"))
                 catch (check config p) (const $ putStrLn "Jam!")
   width    = foldl' max 0 (map (length.fst) tests)

padTo :: Int -> String -> String
padTo n = take n . (++ repeat ' ')

-- | Check a batch tersely.
quickBatch :: TestBatch -> IO ()
quickBatch = checkBatch quick'
 
-- | Check a batch verbosely.
verboseBatch :: TestBatch -> IO ()
verboseBatch = checkBatch verbose'

quick', verbose' :: Config
quick'   = defaultConfig { configMaxTest = 500 }
verbose' = quick' { configEvery = \ n args -> show n ++ ":\n" ++ unlines args }

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
type FractionalType = Float
type NumericType = Int
type OrderableType = Char
type NoPropertyType = Char

genR :: Random a => (a, a) -> Gen a
genR (lo,hi) = fmap (fst . randomR (lo,hi)) rand


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
class EqProp a where (=-=) :: a -> a -> Property

-- | For 'Eq' types as 'EqProp' types
eq :: Eq a => a -> a -> Property
a `eq` a' = property (a == a')

-- Template: fill in with Eq types for a
--   instance EqProp a where (=-=) = eq
-- E.g.,

instance         EqProp Bool      where (=-=) = eq
instance         EqProp Char      where (=-=) = eq
instance         EqProp Int       where (=-=) = eq
instance         EqProp Double    where (=-=) = eq
instance Eq a => EqProp [a]       where (=-=) = eq
instance Eq a => EqProp (Maybe a) where (=-=) = eq

-- Pairing
instance (EqProp a, EqProp b) => EqProp (a,b) where
  (a,b) =-= (a',b') = a =-= a' .&. b =-= b'

-- Either
instance (EqProp a, EqProp b) => EqProp (Either a b) where
  (Left x)  =-=  (Left x') = x =-= x'
  (Right x) =-= (Right x') = x =-= x'
  _         =-=          _ = property False

-- Function equality
instance (Show a, Arbitrary a, EqProp b) => EqProp (a -> b) where
  f =-= f' = property (liftA2 (=-=) f f')
-- Alternative definition:
-- instance (Show a, Arbitrary a, EqProp b) => EqProp (a -> b) where
--   f =-= f' = property (probablisticPureCheck defaultConfig
--                                              (\x -> f x =-= g x))

eqModels :: (Model a b, EqProp b) => a -> a -> Property
eqModels = (=-=) `on` model

-- Other types
-- instance EqProp a => EqProp (S.Stream a) where (=-=) = eqModels


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

class Model a b | a -> b where
  model :: a -> b  -- get the model from a concrete value

-- note: bytestring doesn't make the fundep

---- Compare representation-level and model-level operations (commuting diagrams)

meq  :: (Model a b, EqProp b) => a -> b -> Property
meq1 :: (Model a b, Model a1 b1, EqProp b) =>
	(a1 -> a) -> (b1 -> b) -> a1 -> Property
meq2 :: (Model a b, Model a1 b1, Model a2 b2, EqProp b) =>
	(a1 -> a2 -> a) -> (b1 -> b2 -> b) -> a1 -> a2 -> Property
meq3 :: (Model a b, Model a1 b1, Model a2 b2, Model a3 b3, EqProp b) =>
	(a1 -> a2 -> a3 -> a)
     -> (b1 -> b2 -> b3 -> b)
     -> a1 -> a2 -> a3 -> Property
meq4 :: ( Model a b, Model a1 b1, Model a2 b2
        , Model a3 b3, Model a4 b4, EqProp b) =>
        (a1 -> a2 -> a3 -> a4 -> a)
     -> (b1 -> b2 -> b3 -> b4 -> b)
     -> a1 -> a2 -> a3 -> a4 -> Property
meq5 :: ( Model a b, Model a1 b1, Model a2 b2, Model a3 b3
        , Model a4 b4, Model a5 b5, EqProp b) =>
	(a1 -> a2 -> a3 -> a4 -> a5 -> a)
     -> (b1 -> b2 -> b3 -> b4 -> b5 -> b)
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

instance Model Bool   Bool   where model = id
instance Model Char   Char   where model = id
instance Model Int    Int    where model = id
instance Model Float  Float  where model = id
instance Model Double Double where model = id
instance Model String String where model = id

-- This next one requires UndecidableInstances
instance (Model a b, Model a' b') => Model (a,a') (b,b') where
  model = model *** model

-- instance Model (S.Stream a) (NonNegative Int -> a) where
--   model s (NonNegative i) = s S.!! i


-- | Like 'Model' but for unary type constructors.
class Model1 f g | f -> g where
  model1 :: forall a. f a -> g a


{----------------------------------------------------------
    Some handy testing types
----------------------------------------------------------}

-- from QC2, plus tweaks

type Positive a = NonZero (NonNegative a)

newtype NonZero a = NonZero { unNonZero :: a }
 deriving ( Eq, Ord, Num, Integral, Real, Enum, Show, Read )

instance (Num a, Arbitrary a) => Arbitrary (NonZero a) where
  arbitrary   = fmap NonZero $ arbitrary `suchThat` (/= 0)
  coarbitrary = coarbitrary . unNonZero

newtype NonNegative a = NonNegative { unNonNegative :: a }
 deriving ( Eq, Ord, Num, Integral, Real, Enum, Show, Read )

instance (Num a, Arbitrary a) => Arbitrary (NonNegative a) where
  arbitrary   = nonNegative
  coarbitrary = coarbitrary . unNonNegative

arbitrarySatisfying :: Arbitrary a => (a -> Bool) -> Gen a
arbitrarySatisfying = (arbitrary `suchThat`)

-- | Generates a value that satisfies a predicate.
suchThat :: Gen a -> (a -> Bool) -> Gen a
gen `suchThat` p = satisfiesM p gen

-- | Tries to generate a value that satisfies a predicate.
suchThatMaybe :: Gen a -> (a -> Bool) -> Gen (Maybe a)
gen `suchThatMaybe` p = sized (try 0 . max 1)
 where
  try _ 0 = return Nothing
  try k n = do x <- resize (2*k+n) gen
               if p x then return (Just x) else try (k+1) (n-1)

-- | Generate n arbitrary values
arbs :: Arbitrary a => Int -> IO [a]
arbs n = fmap (\ rnd -> generate n rnd (vector n)) newStdGen

-- | Produce n values from a generator
gens :: Int -> Gen a -> IO [a]
gens n gen =
  fmap (\ rnd -> generate 1000 rnd (sequence (replicate n gen))) newStdGen

-- The next two are from twanvl:

infixr 3 .&.
-- | Property conjunction
(.&.) :: (Testable prop1, Testable prop2) => prop1 -> prop2 -> Property
p1 .&. p2 = property $ \b -> if b then property p1 else property p2

instance Testable a => Testable [a] where
  property []    = property True
  property props = property $ \n -> props !! (n `mod` len)
    where len = length props

instance (Testable a, Testable b) => Testable (a,b) where
  property = uncurry (.&.)

probablisticPureCheck :: Testable a => Config -> a -> Bool
probablisticPureCheck config a = unsafePerformIO $
  do rnd <- newStdGen
     probablisticPureTests config (evaluate a) rnd 0 0 []

probablisticPureTests :: Config
                      -> Gen Result
                      -> StdGen
                      -> Int
                      -> Int
                      -> [[String]]
                      -> IO Bool
probablisticPureTests config gen rnd0 ntest nfail stamps
  | ntest == configMaxTest config = return True
  | nfail == configMaxFail config = return True
  | otherwise                     =
      case ok result of
        Nothing    ->
          probablisticPureTests config gen rnd1 ntest (nfail+1) stamps
        Just True  ->
          probablisticPureTests config gen rnd1 (ntest+1) nfail
                                (stamp result:stamps)
        Just False ->
          return False
     where
      result      = generate (configSize config ntest) rnd2 gen
      (rnd1,rnd2) = split rnd0
