{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import           Data.Constraint
import           Data.Proxy
import           Data.Singletons
{-import           Data.Singletons.Decide-}
import           Data.Singletons.Prelude
import           Data.Singletons.TH
import           GHC.TypeLits            (Nat, SomeNat (SomeNat), someNatVal)
{-import           Data.Kind (Type)-}
--import Lib

-- Witnesses
--------------------------------------------------------------------------------

-- Generally, the best practice is to avoid the functions
-- and exports from GHC.TypeLits and instead use the *singletons*
-- infrastructure because the native TypeLits operations don't
-- interoperate with singletons by default.
--
-- So:
--   - Don't use KnownNat or KnownSymbol,
--     and don't use the ugly "(KnownNat n) => Proxy n -> t" idiom
--   - An explicit parameter is "Sing x"
--   - An implicit parameter is "SingI x"
--   - Convert from explicit to implicit with "singInstance" or "withSingI"
--   - Convert from implicit to explicit with "sing"
--
-- The conversion from explicit to implicit essentially works the same way
-- as in Edward Kmett's Data.Reflection library.
--
-- The upshot here is that once we have singleton functions, we automatically
-- have all the explicit witnesses for function results, and we also get
-- all the implicit witnesses by the general "explicit->implicit" conversion.

-- TypeNat witnesses

-- explicit witnesses are given by singleton functions

n1 :: Sing 2
n1 = sing :: Sing 2
n2 :: Sing 3
n2 = sing :: Sing 3

--n3 = n1 %:+ n2 -- addition
n4 :: Sing 6
n4 = n1 %:* n2 -- multiplication

--n3' :: Integer
--n3' = fromSing n3 -- 5

n4' :: Integer
n4' = fromSing n4 -- 6

-- implicit witnesses

-- Note the funky Num class on the kind level
-- Also, we only use "Dict" here for illustration purposes, in real code
-- we would just create the SingI instances on the fly, as needed.
implicitWitnessTest :: SNum (KindOf a) => Sing a -> Sing b -> Dict (SingI (a :+ b))
implicitWitnessTest a b = withSingI (a %:+ b) Dict

-- leave "a" implicit plus return the singletons analogue of Dict:
implicitWitnessTest2 ::
  forall a b. SingI (a :: Nat) => Sing b -> SingInstance (a :+ b)
implicitWitnessTest2 b = singInstance ((sing :: Sing a) %:+ b)

-- Also: convert a piece of demoted data to some singleton
-- This is a just a sometimes nicer version of "toSing"
withSomeSingtest :: IO ()
withSomeSingtest =
  withSomeSing True (\case STrue -> print "got true"; SFalse -> print "got false")


--So, the witness generators are just singleton functions + conversion.
--
--Note here that "Sing" is often a lot more convenient to use that "SingI".
--That's because if we do any sort of computation on singleton values, we
--need explicit parameters. Also, if there's any sort of ambiguity about
--which parameters we pass to which functions, or about the order of
--parameters, we need to specify it with type annotations, proxies or
--explicit singletons, and explicit singletons are again the cleanest way.
--
--95% of the time singleton arguments are operationally just like normal
--arguments, so there's no reason to make them implicit. Similarly,
--"withNatOp" is more straightforwardly expressed with explicit Sing
--parameters. In short: most of the time use Sing instead of SingI or Proxy.

-- List singletons & manipulation
--------------------------------------------------------------------------------

type KnownNats (xs :: [Nat]) = SingI xs
type NatList (xs :: [Nat]) = Sing xs
type SomeNats = SomeSing [Nat]

---- This one throws error on negative input, although not on conversion,
---- only on subsequent operations.
--someNatsValPos :: [Integer] -> SomeNats
--someNatsValPos = toSing

-- Since the safe conversion is missing from *singletons*, let's
-- implement it
--safeIntegerSing :: Integer -> Maybe (SomeSing (KindOf 0))
safeIntegerSing :: Integer -> Maybe (SomeSing Nat)
safeIntegerSing =
  fmap (\(SomeNat (_ :: Proxy n)) -> SomeSing (sing :: Sing n)) . someNatVal

sequenceSome :: [SomeSing k] -> SomeSing [k]
sequenceSome = foldr (\(SomeSing x) (SomeSing xs) -> SomeSing (SCons x xs)) (SomeSing SNil)

someNatsVal :: [Integer] -> Maybe SomeNats
someNatsVal = fmap sequenceSome . traverse safeIntegerSing


-- Note : "KnownNats ns" is equivalent to "NatList ns", so it's redundant here
--reifyNats :: [Integer] -> (forall ns. KnownNats ns => NatList ns -> r) -> r
--reifyNats :: [Integer] -> (forall (ns::[Nat]). Sing ns -> r) -> r
reifyNats :: [Integer] -> (forall ns. NatList ns -> r) -> r
reifyNats = withSomeSing

data Dim where
  ND :: Dim -- no size
  D  :: Nat -> Dim
 deriving (Eq)

data DimTerm where
  NDTerm :: DimTerm -- no size
  DTerm  :: Integer -> DimTerm
 deriving (Eq, Show)

-- $(singletons [d|
--   data Dim where
--     ND :: Dim -- no size
--     D  :: Bool -> Dim
--    deriving (Eq, Show)
--   |])

data instance Sing (d :: Dim) where
  SND :: Sing 'ND
  SD  :: Sing n -> Sing ('D n)

-- What the heck is happenning HERE! TODO: explain!
instance SingI ('ND :: Dim) where
  sing = SND
instance SingI n => SingI ('D n :: Dim) where
  sing = SD sing

instance SingKind Dim where
  type Demote Dim = DimTerm
  fromSing SND    = NDTerm
  fromSing (SD n) = DTerm $ fromSing n
  toSing NDTerm    = SomeSing SND
  toSing (DTerm n) = case toSing n of
                   SomeSing n' -> SomeSing (SD n')

--type family KnownDim (dim :: Dim) where
--  KnownDim n = SingI n
type KnownDim (d :: Dim) = SingI d
type DimElem  (d :: Dim) = Sing d
type SomeDim = SomeSing Dim
--type KnownDims (xs :: [Dim]) = SingI xs
--type DimList (xs :: [Dim]) = Sing xs
--type SomeDims = SomeSing [Dim]

safeDimSing :: Integer -> Maybe (SomeSing Dim)
safeDimSing d
  | d == -1   = Just $ SomeSing SND
  | otherwise = fmap (\(SomeNat (_ :: Proxy n)) -> SomeSing (sing :: (KnownDim ('D n)) => Sing ('D n)))
                . someNatVal $ d

someDimsVal :: [Integer] -> Maybe (SomeSing [Dim])
someDimsVal = fmap sequenceSome . traverse safeDimSing

dimToInt :: DimTerm -> Integer
dimToInt NDTerm    = -1
dimToInt (DTerm n) = n

dimsToInts :: [DimTerm] -> [Integer]
dimsToInts = fmap dimToInt

--reifyNats' :: [Integer] -> r -> (forall ns. NatList ns -> r) -> r
--reifyNats' ns r f = maybe r (\(SomeSing ns_) -> f ns_) (someNatsVal ns)
--
---- "Decision" is a bit more informative than "Maybe"
--sameNats :: NatList ns -> NatList ms -> Decision (ns :~: ms)
--sameNats = (%~)
--
---- We do the elimination generally.
--listElim ::
--     forall (p :: [a] -> Type).
--     p '[]
--  -> (forall x xs. Sing (x ': xs) -> p xs -> p (x ': xs))
--  -> forall (xs :: [a]). Sing xs -> p xs
--listElim z _ SNil         = z
--listElim z f (SCons x xs) = f (SCons x xs) (listElim z f xs)
--
---- Actually, we can do even more generally if we switch to proper
---- type functions. Basically, this lets us fold with type families.
--listElim' ::
--     forall (p :: TyFun [a] Type -> Type).
--     Proxy p
--  -> p @@ '[]
--  -> (forall x xs. Sing (x ': xs) -> p @@ xs -> p @@ (x ': xs))
--  -> forall (xs :: [a]). Sing xs -> p @@ xs
--listElim' _ z _ SNil         = z
--listElim' p z f (SCons x xs) = f (SCons x xs) (listElim' p z f xs)
--
--
--
---- In general, SomeNat is equivalent to Integer,
---- SomeSymbol is equivalent to String, etc...
---- A demoted value carries the same amount of information as
---- a boxed-up singleton.
--
---- This is the reason why "traverseSList" below is more general
---- than your "traverseNatList" (even if we specialize on Nat)
--traverseSList ::
--     forall (xs :: [a]) f b.
--     (Applicative f, SingKind a)
--  => (Demote a -> f b)
--  -> SList xs -> f [b]
--traverseSList f = traverse f . fromSing
--
---- Closer to original type with more "SomeSing"
--traverseSList' ::
--     forall (xs :: [a]) f b.
--     (Applicative f, SingKind a)
--  => (Demote a -> f (SomeSing b))
--  -> SList xs -> f (SomeSing [b])
--traverseSList' f = fmap sequenceSome . traverseSList f
--
---- The standard singleton map; this one preserves the most information
--mapNatList ::
--  Sing (f :: TyFun Nat b -> Type) -> Sing (xs :: [Nat]) -> Sing (MapSym2 f xs)
--mapNatList = sMap
--
--mapNatList' :: (Integer -> Integer) -> Sing (xs :: [Nat]) -> [Integer]
--mapNatList' f = map f . fromSing
--
--mapNatList'' :: (Integer -> Integer) -> Sing (xs :: [Nat]) -> SomeNats
--mapNatList'' f = toSing . fmap f . fromSing


-- The functions are the same for SymbolList etc, everything can be
-- generalized to cover the Symbol stuff too.
--------------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "hi, yo!"
  print n4'
  let a = case someNatsVal [10,20,3] of
            --Just (SomeSing (_::Sing xs)) -> undefined -- show $ fromSing xs
            Just (SomeSing xs) -> show $ fromSing xs
            Nothing            -> "oops"
  putStrLn a

  let b = case toSing (DTerm 1) of
            (SomeSing d) -> show $ fromSing d
  putStrLn b

  let c = case safeDimSing 12 of
            Just (SomeSing d) -> show $ fromSing d
            Nothing           -> "Invalid Size"
  putStrLn c

  let d = (\(SomeSing s)-> fromSing s) <$> someDimsVal [10,20,3,-1]
  print d
  print $ dimsToInts <$> d

  let e = fromSing (sing :: Sing '[ 'D 10,'D 20,'D 3,'ND ])
  print e
  print $ dimsToInts e
