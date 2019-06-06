{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances#-}

module Playground.StateMachine where

import Control.Monad.Indexed
import Control.Monad.Indexed.State

import Data.Kind (Type)
import Data.Row.Records
import Data.Row.Internal
import Data.Type.Equality

import GHC.TypeNats
import GHC.Exts (Constraint)

--------------------------------------------------------------------------------
-- Experiments with 'row-types' library
--------------------------------------------------------------------------------

r :: Rec Empty
r = empty

r2 :: Rec ("x" .== Int)
r2 = Label .== 7

getXr2 ::  Rec ("x" .== Int) -> Int
getXr2 r = r .! (Label :: Label "x")
-- Written with OverloadedLabels:
-- getXr2 r = r .! #x

data Test = Test

-- To me, these seem like the same types, and the same functions. Can these two
-- type signatures be resolved? i.e. Can we swap the type signatures for each
-- other and have it still compile? No! ...but why not?

addY :: Lacks "y" r => Rec r -> Rec (Extend "y" Test r)
addY r = extend #y Test r

addY2 :: Lacks "y" r => Rec r -> Rec (r .+ "y" .== Test)
addY2 r = r .+ #y .== Test

-- Note:
--   Without diving further down the rabit hole, we should state that if
--   'extend' is used to add a key/val pair to the record, the resulting type
--   should be 'Extend ...'; Likewise, when extending a record using the (.+)
--   operator, the resulting type should also use the (.+) type level operator.

--------------------------------------------------------------------------------
-- Type-level Maps
--------------------------------------------------------------------------------

-- infix 7 :->
-- data TMapping k v = k :-> v
--
-- -- Approach #1:
-- ---------------
-- -- Utilize existing type level lists to build, _essentially_, a type level
-- -- association list. This is the Kind of Type-level maps
-- type KMap k v = [TMapping k v]
--
-- data TMap (m :: KMap k v) where
--   TEmpty :: TMap '[]
--   TExtended :: k -> v -> TMap m -> TMap (k :-> v ': m)
--
-- data KMaybe (a :: Type) where
--   TNothing :: KMaybe a
--   TJust :: a -> KMaybe a
--
-- type family IsJust t :: Bool where
--   IsJust TNothing = 'False
--   IsJust (TJust _) = 'True
--
-- -- | Type level lookup of a value in an association list
-- --
-- -- Note: It's hard to give type variable names and kind variable names at the
-- -- same time. Maybe 't<varName>' and 'k<varName>' scheme works?
-- --
-- -- TODO I don't totally understand kind signatures implied/inferred by the
-- -- PolyKinds extension: https://downloads.haskell.org/~ghc/7.8.1/docs/html/users_guide/kind-polymorphism.html
-- type family TLookup a (b :: KMap k v) :: KMaybe v where
--   TLookup k '[] = TNothing
--   TLookup k (k :-> v ': _) = TJust v
--   TLookup k (kvp ': rest) = TLookup k rest
--
-- type family TMember (a :: k) (b :: KMap k v) :: Bool where
--   TMember k m = IsJust (TLookup k m)
--
-- -- Here we cannot write the definition for this function because technically
-- -- TMap (k :-> v ': m) is a different type than TMap (l :-> v ': m)...
-- --
-- -- This means that we have to implement a type class for type-safe value level
-- -- lookups of values in type level association lists.
-- -- tlookup :: forall k m v. TMember k m ~ True => k -> TMap (k :-> v ': m) -> v
-- -- tlookup c (TExtended k v rest) = c == k = v
-- -- tlookup c (TExtended l ... <-- Doesn't work!
-- -- instead...
--
-- class IsMember k v m where
--   tlookup :: k -> TMap m -> v
--
-- -- TODO Exercise: Implement the kind signatures for these type families:
--
-- type family TInsert k v m where
--   TInsert k v m = (k :-> v) ': m
--
-- -- | Delete the first ocurrence of a key in a Type-level Map
-- type family TDelete k m where
--   TDelete k '[]            = '[]
--   TDelete k (k :-> v ': m) = m
--   TDelete k (kvp     ': m) = TDelete k m
--
-- --------------------------------------------------------------------------------
-- -- Type-level State Machines
-- --------------------------------------------------------------------------------
--
-- type UserId = Nat
-- type Balance = Nat
--
-- type Holdings = KMap UserId Balance
--
-- type BankM i j a = IxState i j a
--
-- userBalance :: BankM i i i
-- userBalance = iget
