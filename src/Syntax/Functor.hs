{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
module Syntax.Functor
( HFunctor(..)
) where

import qualified Control.Effect.Sum as Sum

class (forall f . Functor f => Functor (h f)) => HFunctor h where
  hmap :: Functor f => (forall x . f x -> g x) -> (h f a -> h g a)

instance (HFunctor l, HFunctor r) => HFunctor (l Sum.:+: r) where
  hmap f = \case
    Sum.L l -> Sum.L (hmap f l)
    Sum.R r -> Sum.R (hmap f r)
  {-# INLINE hmap #-}
