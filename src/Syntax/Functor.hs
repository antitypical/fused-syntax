{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
module Syntax.Functor
( HFunctor(..)
) where

import Control.Effect.Sum ((:+:)(..))

class (forall f . Functor f => Functor (h f)) => HFunctor h where
  hmap :: Functor f => (forall x . f x -> g x) -> (h f a -> h g a)

instance (HFunctor l, HFunctor r) => HFunctor (l :+: r) where
  hmap f = \case
    L l -> L (hmap f l)
    R r -> R (hmap f r)
  {-# INLINE hmap #-}
