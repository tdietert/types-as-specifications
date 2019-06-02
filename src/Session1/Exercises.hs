{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Session1.Exercises where

import           Data.Kind          (Type, Constraint)
import           Data.List          ((++))
import           GHC.TypeNats       ()
import           Prelude            (Int, Show (..))

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

type family (:*:) a b where
  x :*: y = Mult x y

testMult1 = Refl :: Mult One Zero :~: Zero
testMult2 = Refl :: Mult One One :~: One
testMult3 = Refl :: Mult Three Two :~: Six

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

----------------------------------------
-- Helpers
----------------------------------------

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
