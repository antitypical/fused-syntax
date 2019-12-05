{-# LANGUAGE RankNTypes, QuantifiedConstraints #-}
module Syntax.Traversable
( HTraversable(..)
) where

import Syntax.Functor

class ( HFunctor sig
      , forall g . Foldable g    => Foldable    (sig g)
      , forall g . Functor  g    => Functor     (sig g)
      , forall g . Traversable g => Traversable (sig g)
      )
   => HTraversable sig where
  htraverse :: (Monad f, Traversable g) => (forall a . g a -> f (h a)) -> sig g a -> f (sig h a)
