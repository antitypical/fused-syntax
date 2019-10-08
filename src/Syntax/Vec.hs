{-# LANGUAGE DataKinds, GADTs, KindSignatures, StandaloneDeriving #-}
module Syntax.Vec
( Vec(..)
) where

import Syntax.Nat

data Vec (n :: Nat) a where
  VZ :: Vec 'Z a
  (:#) :: a -> Vec n a -> Vec ('S n) a

infixr 5 :#

deriving instance Eq   a => Eq   (Vec n a)
deriving instance Ord  a => Ord  (Vec n a)
deriving instance Show a => Show (Vec n a)
