{-# LANGUAGE DeriveGeneric, DeriveTraversable, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, PolyKinds, TypeOperators #-}
module Data.Syntax.Sum
( -- * Sum syntax
  (:+:)(..)
  -- * Membership
, Inject(..)
) where

import Data.Syntax.Functor
import GHC.Generics (Generic, Generic1)

data (f :+: g) t a
  = L (f t a)
  | R (g t a)
  deriving (Eq, Foldable, Functor, Generic, Generic1, Ord, Show, Traversable)

infixr 4 :+:

instance (HFunctor f, HFunctor g) => HFunctor (f :+: g)


class Inject t u where
  inj :: t m a -> u m a

instance {-# OVERLAPPABLE #-}
         Inject t t where
  inj = id

instance {-# OVERLAPPABLE #-}
         Inject t (t :+: r) where
  inj = L

instance {-# OVERLAPPABLE #-}
         Inject t (l1 :+: l2 :+: r)
      => Inject t ((l1 :+: l2) :+: r) where
  inj = reassoc . inj where
    reassoc (L l)     = L (L l)
    reassoc (R (L l)) = L (R l)
    reassoc (R (R r)) = R r

instance {-# OVERLAPPABLE #-}
         Inject t r
      => Inject t (l :+: r) where
  inj = R . inj
