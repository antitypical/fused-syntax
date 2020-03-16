{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
module Syntax.Functor
( HFunctor(..)
) where

class (forall f . Functor f => Functor (h f)) => HFunctor h where
  hmap :: (forall x . f x -> g x) -> (h f a -> h g a)
