{-# LANGUAGE DeriveGeneric, DeriveTraversable, FlexibleContexts, QuantifiedConstraints, StandaloneDeriving #-}
module Example.Lam
( Lam(..)
, lam
, ($$)
) where

import Control.Monad.Module
import Data.Syntax.Algebra
import Data.Syntax.Functor
import Data.Syntax.Scope
import GHC.Generics (Generic1)

data Lam t a
  = Abs (Scope () t a)
  | t a :$ t a
  deriving (Foldable, Functor, Generic1, Traversable)

infixl 9 :$

deriving instance (Eq   a, forall a . Eq   a => Eq   (f a), Monad f) => Eq   (Lam f a)
deriving instance (Ord  a, forall a . Eq   a => Eq   (f a)
                         , forall a . Ord  a => Ord  (f a), Monad f) => Ord  (Lam f a)
deriving instance (Show a, forall a . Show a => Show (f a))          => Show (Lam f a)

instance HFunctor Lam

instance RightModule Lam where
  Abs b  >>=* f = Abs (b >>=* f)
  g :$ a >>=* f = (g >>= f) :$ (a >>= f)


lam :: (Eq a, Has Lam sig t) => a -> t a -> t a
lam v b = term (Abs (abstract1 v b))

($$) :: Has Lam sig t => t a -> t a -> t a
f $$ a = term (f :$ a)

infixl 9 $$
