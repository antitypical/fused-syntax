{-# LANGUAGE DeriveFunctor, QuantifiedConstraints, StandaloneDeriving, UndecidableInstances #-}
module Data.Syntax.Term
( Term(..)
) where

data Term sig a
  = Var a
  | Term (sig (Term sig) a)

deriving instance ( forall g . Functor     g => Functor     (sig g)) => Functor     (Term sig)
