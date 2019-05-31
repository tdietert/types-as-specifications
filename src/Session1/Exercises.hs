{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module Session1.Exercises where

import           Data.Kind          (Type, Constraint)
import           Data.List          ((++))
import           GHC.TypeLits       (ErrorMessage (..), TypeError (..))
import           GHC.TypeNats       ()
import           Prelude            (Int, Read, Show (..), id)

import           Data.Type.Equality ((:~:) (..))

--------------------------------------------------------------------------------
-- Exercises P1.A
--
--   Type-level Boolean operations
--------------------------------------------------------------------------------

data Bool
  = False
  | True

-- | Exercise P1.A1
--
-- AND operation between two types of kind Bool

-- Note:
--   Linter may complain about using wildcards in def:
--   TAnd _ _ = False
type family TAnd a b where
  TAnd True True = True
  TAnd a b = False

-- | Type synonym using type operators
--
-- First type operator defined:
--   type (&&) = TAnd
--   ^ This can't work because type families must be fully applied
type (&&) a b = TAnd a b

-- | Exercise P1.A2
type family IfThenElse (c :: Bool) (a :: k) (b :: k) where
  IfThenElse 'True a b = a
  IfThenElse 'False a b = b

testIfThenElse1 = Refl :: IfThenElse 'True Int Bool :~: Int
testIfThenElse2 = Refl :: IfThenElse 'False Int Bool :~: Bool

--------------------------------------------------------------------------------
-- Exercise P1.B
--
--   Type-level Natural Number operations
--------------------------------------------------------------------------------

-- | The kind of Type-level Natural numbers
data Nat
  = Z
  | S Nat

type family Add (n :: Nat) (m :: Nat) :: Nat where
  Add n 'Z = n
  Add 'Z m = m
  Add ('S n) m = 'S (Add n m)

type x + y = Add x y

----------------------------------------
-- Exercise P1.B1
--
--   Type level multiplication of type level natural numbers
----------------------------------------

-- | Multiplication of type-level naturals
type family Mult x y where
  Mult 'Z m = 'Z
  Mult ('S n) m = m + (Mult n m)

testMult1 = TAssertEq @(Mult One Zero) @Zero
testMult2 = TAssertEq @(Mult One One) @One
testMult3 = TAssertEq @(Mult Three Two) @Six

----------------------------------------
-- Exercise P1.B2
--
--   Type level exponentiation of type level natural numbers
----------------------------------------

-- | Exponentiation of type-level naturals
type family Exp (x :: Nat) (y :: Nat) :: Nat where
  Exp 'Z y = 'Z
  Exp x 'Z = 'S 'Z
  Exp x ('S y) = Mult x (Exp x y)

testExp1 = Refl :: Exp Zero Five :~: Zero
testExp2 = Refl :: Exp Six Zero :~: One
testExp3 = Refl :: Exp Three Two :~: Nine

--------------------------------------------------------------------------------
-- Exercise P1.C
--
--   Type level lists: The H-list and its operations.
--------------------------------------------------------------------------------

data HList (a :: [k]) where
  HNil :: HList '[]
  HCons :: x -> HList xs -> HList (x ': xs)

instance Show (HList '[]) where
  show HNil = "[]"

instance (Show x, Show (HList xs)) => Show (HList (x ': xs)) where
  show (HCons x xs) = show x ++ " .:. " ++ show (xs)

----------------------------------------
-- Exercise P1.C1
--
--   Type-level head & tail of heterogenous list
--
-- Note: We don't need tests for these. Why not?
----------------------------------------

hhead :: HList (x ': xs) -> x
hhead (HCons x xs) = x

htail :: HList (x ': xs) -> HList xs
htail (HCons x xs) = xs

----------------------------------------
-- Exercise P1.C2
--
--   Type-level filter
----------------------------------------

type family Map (f :: k -> j) (xs :: [k]) :: [j] where
  Map f '[] = '[]
  Map f (x ': xs) = f x ': Map f xs

-- | Note, you might have to change the kind signature of the
-- IfThenElse type family above, from Exercise P1.A2
type family Filter (p :: k -> Bool) (xs :: [k]) :: [k] where
  Filter p '[] = '[]
  Filter p (x ': xs) =
    IfThenElse (p x)
      (x ': Filter p xs)
      (Filter p xs)

-- filterTest1 = Refl :: Filter

----------------------------------------
-- Exercise P1.C2.A
--
--   Constraint that all elements of a type level list satisfy a Constraint
----------------------------------------

type family All (c :: k -> Constraint) (xs :: [k]) :: Constraint where
  All _ '[] = ()
  All c (x ': xs) = (c x, All c xs)

----------------------------------------
-- Exercise P1.C3
--
--   Type-level concatenation of Heterogenous lists
----------------------------------------

type family Append (xs :: [Type]) (ys :: [Type]) :: [Type] where
  Append '[] ys = ys
  Append xs '[] = xs
  Append (x ': xs) ys = x ': Append xs ys

concatLemma1 :: HList '[] -> HList ys -> HList (Append '[] ys) :~: HList ys
concatLemma1 _ _ = Refl

concatLemma2 :: HList xs -> HList '[] -> HList (Append xs '[]) :~: HList xs
concatLemma2 _ _ = Refl

concatLemma3 :: HList (x ': xs) -> HList ys -> HList (x ': Append xs ys) :~: HList (Append (x ': xs) ys)
concatLemma3 h1@(HCons x xs) HNil         = concatLemma2 h1 HNil
concatLemma3 h1@(HCons x xs) (HCons y ys) = Refl -- here we trivially assert equality: "trust me, GHC"

happend :: HList xs -> HList ys -> HList (Append xs ys)
happend HNil ys         = ys
happend xs HNil         = xs
happend h1@(HCons x xs) h2 =
  case concatLemma3 h1 h2 of
    Refl -> HCons x (happend xs h2)

-- | This is an interesting way to represent heterogenous lists, in which the
-- structure of the list matches the structure of the type, thus we don't need
-- type equality proofs/lemmas.
--
-- reference: http://okmij.org/ftp/Haskell/HList-ext.pdf
--
--   In fact, this implementation is isomophic with the GADT implementation...
--   wow! Are there any down sides to this representation aside from typeclasses
--   being "open" and extensible? It's so curious that we do not have to prove
--   anything about HAppend.
--
--     GADT + Type families
--       vs
--     Unary datatypes + Type Classes
--
-- data HNil = HNil deriving (Show, Read)
-- data HCons e l = HCons e l deriving (Show, Read)
--
-- class HList l
-- instance HList HNil
-- instance HList l => HList (HCons e l)
--
-- (.:.) :: HList l => e -> l -> HCons e l
-- (.:.) = HCons
--
-- class HAppend l1 l2 l3 | l1 l2 -> l3 where
--   happend :: l1 -> l2 -> l3
--
-- instance HList l => HAppend HNil l l where
--   happend HNil = id
--
-- instance (HList l1, HAppend l1 l2 l3)
--       => HAppend (HCons x l1) l2 (HCons x l3) where
--   happend (HCons x l1) l2 = x .:. (happend l1 l2)

----------------------------------------
-- Exercise P1.C4
--
--   Why can't we write a function `hconcat` over heterogenous lists?
--
--  hconcat :: HList xss -> HList xs
----------------------------------------


--------------------------------------------------------------------------------
-- Testing Helpers
--------------------------------------------------------------------------------

-- | Type level equality
type family TEq a b :: Bool where
  TEq a a = 'True
  TEq a b = 'False

-- | Type level equality
--
-- https://hackage.haskell.org/package/base-4.12.0.0/docs/src/Data.Type.Equality.html#%3D%3D
--
-- The idea here is to recognize equality of *applications* using
-- the first case, and of *constructors* using the second and third
-- ones. It would be wonderful if GHC recognized that the
-- first and second cases are compatible, which would allow us to
-- prove
--
-- a ~ b => a == b
--
-- but it (understandably) does not.
--
-- It is absolutely critical that the three cases occur in precisely
-- this order. In particular, if
--
-- a == a = 'True
--
-- came first, then the type application case would only be reached
-- (uselessly) when GHC discovered that the types were not equal.
--
-- One might reasonably ask what's wrong with a simpler version:
--
-- type family (a :: k) == (b :: k) where
--  a == a = True
--  a == b = False
--
-- Consider
-- data Nat = Zero | Succ Nat
--
-- Suppose I want
-- foo :: (Succ n == Succ m) ~ True => ((n == m) :~: True)
-- foo = Refl
--
-- This would not type-check with the simple version. `Succ n == Succ m`
-- is stuck. We don't know enough about `n` and `m` to reduce the family.
-- With the recursive version, `Succ n == Succ m` reduces to
-- `Succ == Succ && n == m`, which can reduce to `'True && n == m` and
-- finally to `n == m`.
type family (a :: k) == (b :: k) :: Bool where
  f a == g b = f == g && a == b
  a == a = 'True
  a == b = 'False

type family AssertEq (a :: k) (b :: k) :: * where
  AssertEq a b =
    IfThenElse (a == b)
      ()
      (TypeError (Text "Type " :<>: ShowType a :<>: Text " not equal to type " :<>: ShowType b))

type family (:*:) a b where
  x :*: y = Mult x y

type Zero  = 'Z
type One   = 'S 'Z
type Two   = 'S ('S 'Z )
type Three = 'S ('S ('S 'Z ))
type Four  = Mult Two Two
type Five  = Add Two Three
type Six   = Add Three Three
type Seven = Add Four Three
type Eight = Mult Four Two
type Nine = Mult Three Three

-- | Proof that asserts two types are equal.
--
-- Note:
--
--   GHC does not full evaluate type families, thus we must create a value that
--   has the type that we expect to fail. Types are only checked for _values_;
--   i.e. a type family application that results in a type error will not throw
--   the type error unless a value is instantiated with that type. We cannot
--   simply create a type synonymn with the invalid type.
--
-- NOTE:
--
--   This can be replaced with Data.Type.Equality.(:~:) and the Refl constructor
--
data TAssertEq where
  TAssertEq :: forall a b. AssertEq a b ~ () => TAssertEq
