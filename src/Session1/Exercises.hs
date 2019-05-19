{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Session1.Exercises where

import           GHC.TypeLits (ErrorMessage (..), TypeError (..))
import           GHC.TypeNats ()

data Bool
  = False
  | True

type family TAnd a b where
  TAnd True True = True
  TAnd a b = False
  -- Linter may complain about using wildcards in def:
  -- TAnd _ _ = False

-- |
--
-- First type operator defined:
--   type (&&) = TAnd
--   ^ This can't work because type families must be fully applied
type (&&) a b = TAnd a b

-- | The kind of Type-level Natural numbers
data Nat
  = Z
  | S Nat

-- | Addition of type-level naturals
type family Add x y where
  Add 'Z n = n
  Add ('S n) m = 'S (Add n m)

addTest1 = TAssertEq @(Add One Zero) @One
addTest2 = TAssertEq @(Add Zero One) @One
addTest3 = TAssertEq @(Add Four Two) @Six

-- | Multiplication of type-level naturals
type family Mult x y where
  Mult 'Z m = 'Z
  Mult ('S n) m = Add m (Mult n m)


-- Why do these not fail? Something to do with type family application happens
-- during type checking
-- type MultTest1 = AssertEq (Mult ('S 'Z) 'Z          ) ('S ('S 'Z))
-- type MultTest2 = AssertEq (Mult 'Z      ('S 'Z)     ) ('S ('S 'Z))
-- type MultTest3 = AssertEq (Mult ('S 'Z) ('S ('S 'Z))) ('S ('S ('S 'Z)))

-- |
--
-- For some reason, GHC doesn't seem to fully evaluate type families if
data TAssertEq where
  TAssertEq :: forall a b. AssertEq a b ~ () => TAssertEq

multTest1 = TAssertEq @(Mult One Zero) @Zero
multTest2 = TAssertEq @(Mult One One) @One
multTest3 = TAssertEq @(Mult Three Two) @Six

--------------------------------------------------------------------------------
-- Type-level Helpers
--------------------------------------------------------------------------------

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

type family IfThenElse c a b where
  IfThenElse 'True a b = a
  IfThenElse 'False a b = b

-- Testing Helpers

type Zero  = 'Z
type One   = 'S 'Z
type Two   = 'S ('S 'Z )
type Three = 'S ('S ('S 'Z ))
type Four  = Mult Two Two
type Five  = Add Two Three
type Six   = Add Three Three
