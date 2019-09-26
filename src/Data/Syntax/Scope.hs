{-# LANGUAGE DeriveTraversable #-}
module Data.Syntax.Scope
( Incr(..)
, matchEither
) where

data Incr a b
  = Z a
  | S b
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

instance Applicative (Incr a) where
  pure = S
  Z a <*> _ = Z a
  S f <*> a = f <$> a

instance Monad (Incr a) where
  Z a >>= _ = Z a
  S a >>= f = f a

matchEither :: Applicative f => (b -> Either a c) -> b -> Incr a (f c)
matchEither f x = either Z (S . pure) (f x)
