{-# LANGUAGE DeriveTraversable, FlexibleInstances, MultiParamTypeClasses, QuantifiedConstraints, RankNTypes, StandaloneDeriving, UndecidableInstances #-}
module Syntax.Term
( Term(..)
, hoistTerm
, unTerm
, prjTerm
) where

import Control.Applicative (Alternative(..))
import Control.Monad ((<=<), ap)
import Syntax.Algebra
import Syntax.Functor
import Syntax.Module
import Syntax.Sum

data Term sig a
  = Var a
  | Term (sig (Term sig) a)

deriving instance ( Eq a
                  , RightModule sig
                  , forall f . Functor f => Functor (sig f)
                  , forall g x . (Eq  x, Monad g, forall y . Eq  y => Eq  (g y)) => Eq  (sig g x)
                  )
               => Eq  (Term sig a)
deriving instance ( Ord a
                  , RightModule sig
                  , forall f . Functor f => Functor (sig f)
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

instance ( RightModule sig
         , forall f . Functor f => Functor (sig f)
         )
      => Applicative (Term sig) where
  pure = Var
  (<*>) = ap

instance ( RightModule sig
         , forall f . Functor f => Functor (sig f)
         )
      => Monad (Term sig) where
  Var  a >>= f = f a
  Term t >>= f = Term (t >>=* f)


instance ( Syntax sig
         , forall f . Functor f => Functor (sig f)
         )
      => Algebra sig (Term sig) where
  gen = Var
  alg = Term


hoistTerm :: (HFunctor sig, forall g . Functor g => Functor (sig g)) => (forall m x . sig m x -> sig' m x) -> Term sig a -> Term sig' a
hoistTerm f = go
  where go (Var v)  = Var v
        go (Term t) = Term (f (hmap (hoistTerm f) t))


unTerm :: Alternative m => Term sig a -> m (sig (Term sig) a)
unTerm (Term t) = pure t
unTerm _        = empty

prjTerm :: (Alternative m, Project sub sig) => Term sig a -> m (sub (Term sig) a)
prjTerm = maybe empty pure . (prj <=< unTerm)
