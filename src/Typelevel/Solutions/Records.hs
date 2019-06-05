{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE GADTs         #-}
{-# LANGUAGE PolyKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Typelevel.Solutions.Records where

import GHC.TypeLits

----------------------------------------
-- Extensible Records
--
--   - What are extensible records?
--   - Write the datatype (similar to HList)
--     - define the Field type
--   - Point out Boolean type-level equality in Data.Type.Equality
--   - Write the IsField type family (Constraint)
--   - Write the accessor function
--   - Write the extend function
-----------------------------------------

newtype l :-> a = Field a

data Rec (xs :: [*]) where
  RecEmpty :: Rec '[]
  RecCons  :: l :-> a -> Rec r -> Rec (l :-> a ': r)
