{-# LANGUAGE DeriveGeneric, DeriveTraversable, PolyKinds, TypeOperators #-}
module Data.Syntax.Sum
( (:+:)(..)
) where

import GHC.Generics (Generic, Generic1)

data (f :+: g) t a
  = L (f t a)
  | R (g t a)
  deriving (Eq, Foldable, Functor, Generic, Generic1, Ord, Show, Traversable)
