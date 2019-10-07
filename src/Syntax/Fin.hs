{-# LANGUAGE DataKinds, GADTs, KindSignatures #-}
module Syntax.Fin
( Fin(..)
) where

import Syntax.Nat

data Fin (n :: Nat) where
  FZ :: Fin n
  FS :: Fin n -> Fin ('S n)
