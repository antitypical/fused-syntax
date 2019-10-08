{-# LANGUAGE DataKinds, EmptyCase, GADTs, KindSignatures, StandaloneDeriving #-}
module Syntax.Fin
( Fin(..)
, absurd
) where

import Syntax.Nat

data Fin (n :: Nat) where
  FZ :: Fin ('S n)
  FS :: Fin n -> Fin ('S n)

deriving instance Eq   (Fin n)
deriving instance Ord  (Fin n)
deriving instance Show (Fin n)

absurd :: Fin 'Z -> a
absurd v = case v of {}
