{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Helpers where

import           Prelude            hiding (Bool(..))
import           GHC.TypeLits       (ErrorMessage (..), TypeError)

--------------------------------------------------------------------------------
-- Testing Helpers
--------------------------------------------------------------------------------

data Bool = True | False

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

type family IfThenElse (c :: Bool) (a :: k) (b :: k) where
  IfThenElse 'True a b = a
  IfThenElse 'False a b = b

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

-- | Proof that asserts two types are equal.
--
-- Note:
--
--   GHC does not fully evaluate type families, thus we must create a value that
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
