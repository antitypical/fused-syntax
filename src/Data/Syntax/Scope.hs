{-# LANGUAGE DeriveTraversable #-}
module Data.Syntax.Scope
( Incr(..)
) where

data Incr a b
  = Z a
  | S b
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)
