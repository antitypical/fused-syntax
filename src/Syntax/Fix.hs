{-# LANGUAGE DeriveTraversable, QuantifiedConstraints, StandaloneDeriving, UndecidableInstances #-}
module Syntax.Fix
( Fix(..)
) where

newtype Fix sig a = Fix { unFix :: sig (Fix sig) a }

deriving instance (forall g . Foldable    g => Foldable    (sig g)) => Foldable (Fix sig)
deriving instance (forall g . Functor     g => Functor     (sig g)) => Functor  (Fix sig)
deriving instance (forall g . Foldable    g => Foldable    (sig g)
                 , forall g . Functor     g => Functor     (sig g)
                 , forall g . Traversable g => Traversable (sig g)) => Traversable  (Fix sig)
