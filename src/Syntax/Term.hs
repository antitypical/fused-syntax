{-# LANGUAGE DeriveTraversable, FlexibleInstances, LambdaCase, MultiParamTypeClasses, QuantifiedConstraints, RankNTypes, ScopedTypeVariables, StandaloneDeriving, UndecidableInstances #-}
module Syntax.Term
( Term(..)
, hoistTerm
, unTerm
, prjTerm
, iter
, cata
, cataM
  -- * Pretty-printing
, foldTerm
) where

import Control.Applicative (Alternative(..))
import Control.Effect.Carrier (Carrier(..))
import Control.Monad ((<=<), ap)
import Syntax.Fin
import Syntax.Functor
import Syntax.Module
import Syntax.Sum
import Syntax.Traversable
import Syntax.Var
import Syntax.Vec

data Term sig a
  = Var a
  | Alg (sig (Term sig) a)

deriving instance ( Eq a
                  , RightModule sig
                  , forall g x . (Eq  x, Monad g, forall y . Eq  y => Eq  (g y)) => Eq  (sig g x)
                  )
               => Eq  (Term sig a)
deriving instance ( Ord a
                  , RightModule sig
                  , forall g x . (Eq  x, Monad g, forall y . Eq  y => Eq  (g y)) => Eq  (sig g x)
                  , forall g x . (Ord x, Monad g, forall y . Eq  y => Eq  (g y)
                                                , forall y . Ord y => Ord (g y)) => Ord (sig g x)
                  )
               => Ord (Term sig a)
deriving instance (Show a, forall g x . (Show x, forall y . Show y => Show (g y)) => Show (sig g x)) => Show (Term sig a)

deriving instance ( forall g . Foldable    g => Foldable    (sig g)) => Foldable    (Term sig)
deriving instance ( forall g . Functor     g => Functor     (sig g)) => Functor     (Term sig)
deriving instance ( forall g . Foldable    g => Foldable    (sig g)
                  , forall g . Functor     g => Functor     (sig g)
                  , forall g . Traversable g => Traversable (sig g)) => Traversable (Term sig)

instance RightModule sig
      => Applicative (Term sig) where
  pure = Var
  (<*>) = ap

instance RightModule sig
      => Monad (Term sig) where
  Var a >>= f = f a
  Alg t >>= f = Alg (t >>=* f)


instance RightModule sig
      => Carrier sig (Term sig) where
  eff = Alg


hoistTerm
  :: ( HFunctor sig
     , forall g . Functor g => Functor (sig g)
     )
  => (forall m x . sig m x -> sig' m x)
  -> (Term sig a -> Term sig' a)
hoistTerm f = cata Var (Alg . f)


unTerm :: Alternative m => Term sig a -> m (sig (Term sig) a)
unTerm (Alg t) = pure t
unTerm _       = empty

prjTerm :: (Alternative m, Project sub sig) => Term sig a -> m (sub (Term sig) a)
prjTerm = maybe empty pure . (prj <=< unTerm)


iter :: (Carrier sig m, forall f . Functor f => Functor (sig f)) => Term sig a -> m a
iter = \case
  Var a -> pure a
  Alg t -> eff (hmap iter t)


cata
  :: forall sig m a
  .  ( HFunctor sig
     , forall g . Functor g => Functor (sig g)
     )
  => (forall x . x -> m x)
  -> (forall x . sig m x -> m x)
  -> (Term sig a -> m a)
cata var alg = go where
  go :: forall a . Term sig a -> m a
  go = \case
    Var v -> var v
    Alg t -> alg (hmap go t)

cataM
  :: forall sig m f a
  .  ( HTraversable sig
     , Monad m
     )
  => (forall x . x -> m (f x))
  -> (forall x . sig f x -> m (f x))
  -> (Term sig a -> m (f a))
cataM var alg = go where
  go :: forall a . Term sig a -> m (f a)
  go = \case
    Var v -> var v
    Alg t -> alg =<< htraverse go t


foldTerm
  :: forall sig n a var res
  .  (forall n . Vec n var -> Var (Fin n) a -> res)
  -> (forall n . (forall n . Vec n var -> Term sig (Var (Fin n) a) -> res) -> Vec n var -> sig (Term sig) (Var (Fin n) a) -> res)
  -> Vec n var
  -> Term sig (Var (Fin n) a)
  -> res
foldTerm var alg = go where
  go :: forall n . Vec n var -> Term sig (Var (Fin n) a) -> res
  go ctx = \case
    Var v -> var    ctx v
    Alg t -> alg go ctx t
