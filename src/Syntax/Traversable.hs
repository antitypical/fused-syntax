{-# LANGUAGE MultiParamTypeClasses, RankNTypes, QuantifiedConstraints, TypeOperators #-}
module Syntax.Traversable
( HTraversable(..)
, GHTraversable(..)
) where

import Syntax.Functor
import qualified Syntax.Sum as Sum

class ( HFunctor sig
      , forall g . Foldable g    => Foldable    (sig g)
      , forall g . Functor  g    => Functor     (sig g)
      , forall g . Traversable g => Traversable (sig g)
      )
   => HTraversable sig where
  htraverse
    :: (Monad f, Traversable g)
    => (forall a . g a -> f (h a))
    -> (sig g a -> f (sig h a))

instance (HTraversable l, HTraversable r) => HTraversable (l Sum.:+: r) where
  htraverse f = Sum.unSum (fmap Sum.L . htraverse f) (fmap Sum.R . htraverse f)


class GHTraversable g h rep rep' where
  ghtraverse :: (Monad f, Traversable g) => (forall x . g x -> f (h x)) -> rep a -> f (rep' a)
