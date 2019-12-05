{-# LANGUAGE EmptyCase, FlexibleInstances, LambdaCase, MultiParamTypeClasses, QuantifiedConstraints, RankNTypes, TypeOperators #-}
module Syntax.Foldable
( HFoldable(..)
, GHFoldable(..)
) where

import Control.Applicative
import Data.Monoid (Alt(..))
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

instance GHFoldable g (K1 R r) where
  ghfoldMap _ _ = empty

instance GHFoldable g Par1 where
  ghfoldMap _ _ = empty

instance (GHFoldable g l, GHFoldable g r) => GHFoldable g (l :*: r) where
  ghfoldMap f (l :*: r) = ghfoldMap f l <|> ghfoldMap f r

instance (Traversable f, GHFoldable g sig) => GHFoldable g (f :.: sig) where
  ghfoldMap f = getAlt . foldMap (Alt . ghfoldMap f) . unComp1
