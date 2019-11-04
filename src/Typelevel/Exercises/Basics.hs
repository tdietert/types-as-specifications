{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Typelevel.Exercises.Basics where

import           Data.Kind          (Type, Constraint)
import           Data.List          ((++))
import           GHC.TypeNats       ()
import           Prelude            (Int, Show (..))

import           Data.Type.Equality ((:~:) (..))

--------------------------------------------------------------------------------
-- Type-level Booleans
--------------------------------------------------------------------------------

data Bool
  = False
  | True

type family Or a b where
  Or 'True 'True   = 'True
  Or 'True 'False  = 'True
  Or 'False 'True  = 'True
  Or 'False 'False = 'True

infixl 9 ||
type (||) a b = Or a b

----------------------------------------
-- Exercise 1
--
--   Implement the type-level AND operation between two types of kind Bool
----------------------------------------
type family And a b where
  And 'True 'True = 'True
  And _ _ = 'False


-- | Type synonym using type operators
--
-- First type operator defined:
--   type (&&) = TAnd
--   ^ This can't work because type families must be fully applied
type (&&) a b = And a b

testAnd1 = Refl :: And 'True 'True   :~: 'True
testAnd2 = Refl :: And 'False 'True  :~: 'False
testAnd3 = Refl :: And 'True 'False  :~: 'False
testAnd4 = Refl :: And 'False 'False :~: 'False

----------------------------------------
-- Exercise 2
--
--   Implement the IfThenElse type family that returns the 'a' type if the 'c'
--   type is 'True, and the 'b' type if the 'c' type is 'False
----------------------------------------
type family IfThenElse c a b where
  IfThenElse 'True a _ = a
  IfThenElse 'False _ b = b

testIfThenElse1 = Refl :: IfThenElse 'True Int Bool :~: Int
testIfThenElse2 = Refl :: IfThenElse 'False Int Bool :~: Bool

--------------------------------------------------------------------------------
-- Type-level Natural Numbers
--------------------------------------------------------------------------------

-- | The kind of Type-level Natural numbers
data Nat
  = Zero
  | Succ Nat

type family Add (n :: Nat) (m :: Nat) :: Nat where
  Add n 'Zero = n
  Add 'Zero m = m
  Add ('Succ n) m = 'Succ (Add n m)

type x + y = Add x y

----------------------------------------
-- Exercise 3
--
--   Type level multiplication of type level natural numbers
----------------------------------------

-- | Multiplication of type-level naturals
type family Mult (x :: Nat) (y :: Nat) where
  Mult ('Zero) m = 'Zero
  Mult ('Succ 'Zero) m = m
  Mult ('Succ n) m = Add m (Mult n m)

type family (:*:) a b where
  x :*: y = Mult x y

testMult1 = Refl :: Mult One Zero :~: Zero
testMult2 = Refl :: Mult One One :~: One
testMult3 = Refl :: Mult Three Two :~: Six

----------------------------------------
-- Exercise 4
--
--   Type level exponentiation of type level natural numbers
----------------------------------------

-- | Exponentiation of type-level naturals
type family Exp (x :: Nat) (y :: Nat) :: Nat where
  Exp n ('Zero) = 'Succ 'Zero
  Exp m ('Succ 'Zero) = m
  Exp m ('Succ n) = Mult m (Exp m n)

type (^) x y = Exp x y

testExp1 = Refl :: Exp Zero Five :~: Zero
testExp2 = Refl :: Exp Six Zero :~: One
testExp3 = Refl :: Exp Three Two :~: Nine

----------------------------------------
-- Exercise 5
--
--   Annotate the kind signature for 'IfThenElse' so that 'testIfThenElse3'
--   compiles and typechecks.
--
--   Note: If this causes a type error in the 'Filter' typeclass, you've done it
--   wrong.
----------------------------------------

testIfThenElse3 = Refl :: IfThenElse ('False && 'True) (Three + Two) (Seven :*: Five) :~: (Eight :*: Four + Three)

----------------------------------------
-- Helpers
----------------------------------------

type One   = 'Succ 'Zero
type Two   = 'Succ ('Succ 'Zero )
type Three = 'Succ ('Succ ('Succ 'Zero ))
type Four  = Mult Two Two
type Five  = Add Two Three
type Six   = Add Three Three
type Seven = Add Four Three
type Eight = Mult Four Two
type Nine = Mult Three Three
