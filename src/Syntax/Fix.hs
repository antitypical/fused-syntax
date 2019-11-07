{-# LANGUAGE DeriveFoldable, DeriveFunctor, QuantifiedConstraints, StandaloneDeriving, UndecidableInstances #-}
module Syntax.Fix
( Fix(..)
) where

newtype Fix sig a = Fix { unFix :: sig (Fix sig) a }

deriving instance (forall g . Foldable g => Foldable (sig g)) => Foldable (Fix sig)
deriving instance (forall g . Functor  g => Functor  (sig g)) => Functor  (Fix sig)
