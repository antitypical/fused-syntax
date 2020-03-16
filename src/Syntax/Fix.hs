{-# LANGUAGE DeriveTraversable, QuantifiedConstraints, RankNTypes, ScopedTypeVariables, StandaloneDeriving, UndecidableInstances #-}
module Syntax.Fix
( Fix(..)
, hoistFix
, prjFix
, cata
) where

import Control.Applicative (Alternative(..))
import Syntax.Functor
import Syntax.Sum

newtype Fix sig a = Fix { unFix :: sig (Fix sig) a }

deriving instance (Eq   a, forall g x . (Eq   x, forall y . Eq   y => Eq   (g y)) => Eq   (sig g x)) => Eq   (Fix sig a)
deriving instance (Ord  a, forall g x . (Eq   x, forall y . Eq   y => Eq   (g y)) => Eq   (sig g x)
                         , forall g x . (Ord  x, forall y . Ord  y => Ord  (g y)) => Ord  (sig g x)) => Ord  (Fix sig a)
deriving instance (Show a, forall g x . (Show x, forall y . Show y => Show (g y)) => Show (sig g x)) => Show (Fix sig a)

deriving instance (forall g . Foldable    g => Foldable    (sig g)) => Foldable (Fix sig)
deriving instance (forall g . Functor     g => Functor     (sig g)) => Functor  (Fix sig)
deriving instance (forall g . Foldable    g => Foldable    (sig g)
                 , forall g . Functor     g => Functor     (sig g)
                 , forall g . Traversable g => Traversable (sig g)) => Traversable  (Fix sig)


hoistFix
  :: HFunctor sig
  => (forall m x . sig m x -> sig' m x)
  -> (Fix sig a -> Fix sig' a)
hoistFix f = cata (Fix . f)


prjFix :: (Alternative m, Project sub sig) => Fix sig a -> m (sub (Fix sig) a)
prjFix = maybe empty pure . prj . unFix


cata
  :: forall sig m a
  .  HFunctor sig
  => (forall x . sig m x -> m x)
  -> Fix sig a
  -> m a
cata alg = go where
  go :: forall a . Fix sig a -> m a
  go = alg . hmap go . unFix
