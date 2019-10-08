module Syntax.Nat
( Nat(..)
) where

data Nat = Z | S Nat
  deriving (Eq, Ord, Show)
