{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module TypeFamilies where

{-

This module illustrates a question I had about type family evaluation.
See the reddit discussion here:
  https://www.reddit.com/r/haskell/comments/bqmloq/question_type_family_evaluation/


-}

import           GHC.TypeLits                   ( ErrorMessage(..)
                                                , TypeError(..)
                                                )
import           Data.Type.Bool                 ( type (&&) )

type family IfThenElse (c :: Bool) (a :: k) (b :: k) where
  IfThenElse 'True a b = a
  IfThenElse 'False a b = b

type family (a :: k) == (b :: k) :: Bool where
  f a == g b = f == g && a == b
  a == a = 'True
  a == b = 'False

type family AssertEq (a :: k) (b :: k) :: * where
  AssertEq a b =
    IfThenElse (a == b)
      ()
      (TypeError (Text "Type " :<>: ShowType a :<>: Text " not equal to type " :<>: ShowType b))

type ShouldFail = AssertEq 'True 'False

data TAssertEq where
  TAssertEq :: forall a b. AssertEq a b ~ () => TAssertEq

test = TAssertEq @'True @'True
