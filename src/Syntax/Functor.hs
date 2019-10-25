{-# LANGUAGE DefaultSignatures, QuantifiedConstraints, RankNTypes, TypeOperators #-}
module Syntax.Functor
( HFunctor(..)
) where

import Control.Effect.Class (Effect(..))
import Control.Effect.Sum
import Data.Functor.Identity

class (forall f . Functor f => Functor (syn f)) => HFunctor syn where
  hmap
    :: (Monad m, Monad n)
    => (forall a . m a -> n a)
    -> syn m a
    -> syn n a
  default hmap
    :: (Monad m, Monad n, Effect syn)
    => (forall a . m a -> n a)
    -> syn m a
    -> syn n a
  hmap f = fmap runIdentity . handle (Identity ()) (fmap Identity . f . runIdentity)

instance (HFunctor l, HFunctor r) => HFunctor (l :+: r) where
  hmap f (L l) = L (hmap f l)
  hmap f (R r) = R (hmap f r)
