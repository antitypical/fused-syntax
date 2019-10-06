{-# LANGUAGE ConstraintKinds, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, PolyKinds, TypeOperators #-}
module Syntax.Sum
( -- * Sum syntax
  (:+:)(..)
, unSum
  -- * Membership
, Member
, Inject(..)
, Project(..)
) where

import Control.Effect.Sum ((:+:)(..))

unSum :: (f t a -> b) -> (g t a -> b) -> (f :+: g) t a -> b
unSum f _ (L l) = f l
unSum _ g (R r) = g r


type Member t u = (Inject t u, Project t u)


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
