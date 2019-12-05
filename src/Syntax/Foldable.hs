{-# LANGUAGE EmptyCase, FlexibleInstances, LambdaCase, MultiParamTypeClasses, QuantifiedConstraints, RankNTypes, TypeOperators #-}
module Syntax.Foldable
( HFoldable(..)
, GHFoldable(..)
) where

import Control.Applicative
import GHC.Generics
import qualified Syntax.Sum as Sum

class (forall g . Foldable g => Foldable (sig g))
   => HFoldable sig where
  hfoldMap
    :: (Alternative m, Monad m, Foldable g)
    => (forall a . g a -> m a)
    -> (sig g a -> m a)

instance (HFoldable l, HFoldable r) => HFoldable (l Sum.:+: r) where
  hfoldMap f = Sum.unSum (hfoldMap f) (hfoldMap f)


class GHFoldable g rep where
  ghfoldMap :: (Alternative m, Monad m, Foldable g) => (forall x . g x -> m x) -> rep a -> m a

instance GHFoldable g V1 where
  ghfoldMap _ = \case {}

instance GHFoldable g U1 where
  ghfoldMap _ _ = empty
