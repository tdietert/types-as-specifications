{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE TypeFamilies      #-}

module Playground.Maybe where

import Data.Bool

data Maybe (a :: *)
  = Just a
  | Nothing

type family
  IsJust
    (f :: Maybe a)
    :: Bool
  where
    IsJust ('Just _) = 'True
    IsJust 'Nothing  = 'False
