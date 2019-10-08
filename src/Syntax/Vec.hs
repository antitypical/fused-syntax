{-# LANGUAGE DataKinds, DeriveTraversable, GADTs, KindSignatures, StandaloneDeriving #-}
module Syntax.Vec
( Vec(..)
, (!)
) where

import Syntax.Fin
import Syntax.Nat

data Vec (n :: Nat) a where
  VZ :: Vec 'Z a
  (:#) :: a -> Vec n a -> Vec ('S n) a

infixr 5 :#

deriving instance Eq   a => Eq   (Vec n a)
deriving instance Ord  a => Ord  (Vec n a)
deriving instance Show a => Show (Vec n a)

deriving instance Foldable    (Vec n)
deriving instance Functor     (Vec n)
deriving instance Traversable (Vec n)

(!) :: Vec n a -> Fin n -> a
(h :# _) ! FZ   = h
(_ :# t) ! FS n = t ! n
VZ       ! n    = absurd n

infixl 9 !
