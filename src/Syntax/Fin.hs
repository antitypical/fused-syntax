{-# LANGUAGE DataKinds, EmptyCase, GADTs, KindSignatures, StandaloneDeriving #-}
module Syntax.Fin
( Fin(..)
, absurd
, toNum
, strengthen
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

toNum :: Num a => Fin n -> a
toNum FZ     = 0
toNum (FS n) = 1 + toNum n

strengthen :: Fin ('S n) -> Maybe (Fin n)
strengthen FZ     = Nothing
strengthen (FS n) = Just n
