module Control.Monad.Module
( RightModule(..)
) where

class RightModule f where
  (>>=*) :: Monad m => f m a -> (a -> m b) -> f m b
