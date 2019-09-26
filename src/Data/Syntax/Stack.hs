{-# LANGUAGE DeriveTraversable #-}
module Data.Syntax.Stack
( Stack(..)
) where

data Stack a
  = Nil
  | Stack a :> a
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

infixl 4 :>
