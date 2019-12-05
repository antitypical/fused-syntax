{-# LANGUAGE RankNTypes, QuantifiedConstraints, TypeOperators #-}
module Syntax.Traversable
( HTraversable(..)
) where

import Syntax.Functor
import Syntax.Sum

class ( HFunctor sig
      , forall g . Foldable g    => Foldable    (sig g)
      , forall g . Functor  g    => Functor     (sig g)
      , forall g . Traversable g => Traversable (sig g)
      )
   => HTraversable sig where
  htraverse :: (Monad f, Traversable g) => (forall a . g a -> f (h a)) -> sig g a -> f (sig h a)

instance (HTraversable l, HTraversable r) => HTraversable (l :+: r) where
  htraverse f = unSum (fmap L . htraverse f) (fmap R . htraverse f)
