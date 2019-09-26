{-# LANGUAGE DeriveGeneric, DeriveTraversable, PolyKinds, TypeOperators #-}
module Data.Syntax.Sum
( -- * Sum syntax
  (:+:)(..)
) where

import Data.Syntax.Functor
import GHC.Generics (Generic, Generic1)

data (f :+: g) t a
  = L (f t a)
  | R (g t a)
  deriving (Eq, Foldable, Functor, Generic, Generic1, Ord, Show, Traversable)

instance (HFunctor f, HFunctor g) => HFunctor (f :+: g)
