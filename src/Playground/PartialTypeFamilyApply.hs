{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies   #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}

module Playground.PartialTypeFamilyApply where

data DConst a b = DConst a

-- | We can partially apply a Type constructor...
type family CurryDConst (a :: *) :: (* -> *) where
  CurryDConst Int = DConst Int

-- | But _not_ a type family!
-- type family TConst a b where TConst a b = a
-- type family CurryTConst (a :: *) :: (* -> *) where
--   CurryTConst Int = TConst Int

data True
data False

type family TOr (b :: *) (b' :: *) :: * where
  TOr True _ = True
  TOr _ True = True
  TOr Int Char = Maybe (IO ())
  TOr _ _    = False

-- data Bool   -- Declares both the _type_  and _kind_ 'Bool'
--   = True    -- Declares both the _value_ and _type_ 'True'
--   | False   -- Declares both the _value_ and _type_ 'False'
--
