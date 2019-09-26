module Control.Monad.Module
( RightModule(..)
, (>=>*)
, (<=<*)
, joinr
) where

class RightModule f where
  (>>=*) :: Monad m => f m a -> (a -> m b) -> f m b
  infixl 1 >>=*


(>=>*) :: (RightModule f, Monad m) => (a -> f m b) -> (b -> m c) -> (a -> f m c)
f >=>* g = \x -> f x >>=* g

infixl 1 >=>*

(<=<*) :: (RightModule f, Monad m) => (b -> m c) -> (a -> f m b) -> (a -> f m c)
g <=<* f = \x -> f x >>=* g

infixl 1 <=<*

joinr :: (RightModule f, Monad m) => f m (m a) -> f m a
joinr = (>>=* id)
