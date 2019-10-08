{-# LANGUAGE DataKinds, GADTs, KindSignatures #-}
module Syntax.Vec
( Vec(..)
) where

import Syntax.Nat

data Vec (n :: Nat) a where
  VZ :: Vec 'Z a
  (:#) :: a -> Vec n a -> Vec ('S n) a

infixr 5 :#
