{-# LANGUAGE DeriveTraversable, FlexibleInstances, LambdaCase, MultiParamTypeClasses, QuantifiedConstraints, RankNTypes, ScopedTypeVariables, StandaloneDeriving, UndecidableInstances #-}
module Syntax.Term
( Term(..)
, hoistTerm
, unTerm
, prjTerm
, iter
  -- * Pretty-printing
, foldTerm
, foldTermInContext
) where

import Control.Applicative (Alternative(..))
import Control.Effect.Carrier (Carrier(..))
import Control.Monad ((<=<), ap)
import Syntax.Algebra
import Syntax.Fin
import Syntax.Functor
import Syntax.Module
import Syntax.Sum
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


instance ( HFunctor sig
         , forall f . Functor f => Functor (sig f)
         )
      => Algebra sig (Term sig) where
  var = Var
  alg = Alg

instance RightModule sig
      => Carrier sig (Term sig) where
  eff = Alg


hoistTerm
  :: forall sig sig' a
  .  ( HFunctor sig
     , forall g . Functor g => Functor (sig g)
     )
  => (forall m x . sig m x -> sig' m x)
  -> (Term sig a -> Term sig' a)
hoistTerm f = go where
  go :: forall a . Term sig a -> Term sig' a
  go (Var v) = Var v
  go (Alg t) = Alg (f (hmap go t))


unTerm :: Alternative m => Term sig a -> m (sig (Term sig) a)
unTerm (Alg t) = pure t
unTerm _       = empty

prjTerm :: (Alternative m, Project sub sig) => Term sig a -> m (sub (Term sig) a)
prjTerm = maybe empty pure . (prj <=< unTerm)


iter :: Algebra sig m => Term sig a -> m a
iter = \case
  Var a -> var a
  Alg t -> alg (hmap iter t)


foldTerm
  :: (forall g . Functor g => Functor (sig g))
  => (a -> doc)
  -> (forall n . (forall n . Vec n doc -> Term sig (Var (Fin n) a) -> doc) -> Vec n doc -> sig (Term sig) (Var (Fin n) a) -> doc)
  -> Term sig a
  -> doc
foldTerm var alg = foldTermInContext var alg VZ . fmap F

foldTermInContext
  :: forall sig n a doc
  .  (a -> doc)
  -> (forall n . (forall n . Vec n doc -> Term sig (Var (Fin n) a) -> doc) -> Vec n doc -> sig (Term sig) (Var (Fin n) a) -> doc)
  -> Vec n doc
  -> Term sig (Var (Fin n) a)
  -> doc
foldTermInContext var alg = go where
  go :: forall n . Vec n doc -> Term sig (Var (Fin n) a) -> doc
  go ctx = \case
    Var v -> unVar (ctx !) var v
    Alg t -> alg go ctx t
