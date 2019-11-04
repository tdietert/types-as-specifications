{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Typelevel.Exercises.Lists where

import Data.Kind (Type, Constraint)
import Data.Type.Equality ((:~:) (..))
import GHC.TypeNats
import Prelude hiding (Bool(..))
import Typelevel.Exercises.Basics hiding (Nat)

----------------------------------------
-- Exercise 1
--
--   A. Implement a GADT that encodes a recursive list
--   structure indexed by its length (as type Nat)
--   B. Implement the nappend function for length indexed lists
----------------------------------------

data NList (n :: Nat) (a :: *) where

-- Note: You will have to modify the type signature
nappend :: NList n a -> NList m b -> Nlist o c
nappend = undefined

----------------------------------------
-- Exercise 2
--
--   Implement the Append type family for type-level lists
----------------------------------------

type family Append (xs :: [Type]) (ys :: [Type]) :: [Type] where

----------------------------------------

type family Map (f :: k -> j) (xs :: [k]) :: [j] where
  Map f '[] = '[]
  Map f (x ': xs) = f x ': Map f xs

type family Filter (p :: k -> Bool) (xs :: [k]) :: [k] where
  Filter p '[] = '[]
  Filter p (x ': xs) =
    IfThenElse (p x)
      (x ': Filter p xs)
      (Filter p xs)

-- Note how this won't compile, because '<=?' expects to be fully saturated
--
-- filterTest1 = Refl :: Filter (<=? 5) '[3,5,7] :~: '[5]

--------------------------------------------------------------------------------
-- Heterogenous Lists ("n-ary tuples"):
--
--   A datatype ranging over list values that can differ in the type of their
--   elements.
--------------------------------------------------------------------------------

data HList (a :: [*]) where
  HNil :: HList '[]
  HCons :: x -> HList xs -> HList (x ': xs)

----------------------------------------
-- Exercise 2
--
--   Implement a 'Show' instance for the HList datatype
----------------------------------------

----------------------------------------
-- Exercise 3
--
--   Type-level head & tail of heterogenous list
--
-- Note: We don't need tests for these. Why not?
----------------------------------------

hhead :: HList (x ': xs) -> x
hhead = undefined

htail :: HList (x ': xs) -> HList xs
htail = undefined

----------------------------------------

type family All (c :: k -> Constraint) (xs :: [k]) :: Constraint where
  All _ '[] = ()
  All c (x ': xs) = (c x, All c xs)

----------------------------------------
-- Exercise 4
--
--   Re-implement the `Show` instance for HLists making use of the 'All' type
--   family.
----------------------------------------

----------------------------------------
-- Live Programming
----------------------------------------

----------------------------------------
--   Type-level concatenation of Heterogenous lists
--
--     - Write the function
--     - Look at the type error & discuss
--     - Discuss the :~: in Data.Type.Equality
--     - Write the proofs (lemmas)
--     - Fix the instance!
----------------------------------------

happend :: HList xs -> HList ys -> HList (Append xs ys)
happend = undefined

----------------------------------------
-- Discussion Question:
--
--   Why can't we write a function `hconcat` over heterogenous lists?
--     hconcat :: HList xss -> HList xs
----------------------------------------
