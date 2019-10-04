{-# LANGUAGE DeriveGeneric, DeriveTraversable, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, PolyKinds, TypeOperators #-}
module Syntax.Sum
( -- * Sum syntax
  (:+:)(..)
, unSum
  -- * Membership
, Inject(..)
, Project(..)
) where

import GHC.Generics (Generic, Generic1)
import Syntax.Functor
import Syntax.Module

data (f :+: g) t a
  = L (f t a)
  | R (g t a)
  deriving (Eq, Foldable, Functor, Generic, Generic1, Ord, Show, Traversable)

infixr 4 :+:

instance (HFunctor f, HFunctor g) => HFunctor (f :+: g)

instance (RightModule f, RightModule g) => RightModule (f :+: g) where
  L l >>=* f = L (l >>=* f)
  R r >>=* f = R (r >>=* f)


unSum :: (f t a -> b) -> (g t a -> b) -> (f :+: g) t a -> b
unSum f _ (L l) = f l
unSum _ g (R r) = g r


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


class Project t u where
  prj :: u m a -> Maybe (t m a)

instance {-# OVERLAPPABLE #-}
         Project t t where
  prj = Just

instance {-# OVERLAPPABLE #-}
         Project t (t :+: r) where
  prj (L l) = Just l
  prj _     = Nothing

instance {-# OVERLAPPABLE #-}
         Project t (l1 :+: l2 :+: r)
      => Project t ((l1 :+: l2) :+: r) where
  prj = prj . reassoc where
    reassoc (L (L l)) = L l
    reassoc (L (R l)) = R (L l)
    reassoc (R r)     = R (R r)

instance {-# OVERLAPPABLE #-}
         Project t r
      => Project t (l :+: r) where
  prj (R r) = prj r
  prj _     = Nothing
