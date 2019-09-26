{-# LANGUAGE DeriveFunctor, QuantifiedConstraints, StandaloneDeriving, UndecidableInstances #-}
module Data.Syntax.Term
( Term(..)
) where

import Control.Monad (ap)
import Control.Monad.Module

data Term sig a
  = Var a
  | Term (sig (Term sig) a)

deriving instance ( forall g . Functor     g => Functor     (sig g)) => Functor     (Term sig)

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
