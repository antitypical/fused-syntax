{-# LANGUAGE QuantifiedConstraints, RankNTypes #-}
module Syntax.Foldable
( HFoldable(..)
) where

import Control.Applicative

class (forall g . Foldable g => Foldable (sig g))
   => HFoldable sig where
  hfoldMap
    :: (Alternative m, Monad m, Foldable g)
    => (forall a . g a -> m a)
    -> (sig g a -> m a)
