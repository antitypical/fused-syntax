{-# LANGUAGE QuantifiedConstraints, RankNTypes, TypeOperators #-}
module Syntax.Foldable
( HFoldable(..)
) where

import Control.Applicative
import qualified Syntax.Sum as Sum

class (forall g . Foldable g => Foldable (sig g))
   => HFoldable sig where
  hfoldMap
    :: (Alternative m, Monad m, Foldable g)
    => (forall a . g a -> m a)
    -> (sig g a -> m a)

instance (HFoldable l, HFoldable r) => HFoldable (l Sum.:+: r) where
  hfoldMap f = Sum.unSum (hfoldMap f) (hfoldMap f)
