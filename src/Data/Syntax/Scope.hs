{-# LANGUAGE DeriveTraversable, LambdaCase #-}
module Data.Syntax.Scope
( Incr(..)
, incr
, matchEither
, matchMaybe
, closed
, Scope(..)
) where

import Control.Applicative (liftA2)
import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable
import Data.Syntax.Functor

data Incr a b
  = Z a
  | S b
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

instance Bifoldable Incr where
  bifoldMap f g = \case
    Z a -> f a
    S a -> g a

instance Bifunctor Incr where
  bimap f g = \case
    Z a -> Z (f a)
    S a -> S (g a)

instance Bitraversable Incr where
  bitraverse f g = \case
    Z a -> Z <$> f a
    S a -> S <$> g a

instance Applicative (Incr a) where
  pure = S
  Z a <*> _ = Z a
  S f <*> a = f <$> a

instance Monad (Incr a) where
  Z a >>= _ = Z a
  S a >>= f = f a

incr :: (a -> c) -> (b -> c) -> Incr a b -> c
incr z s = \case { Z a -> z a ; S b -> s b }

matchEither :: Applicative f => (b -> Either a c) -> b -> Incr a (f c)
matchEither f x = either Z (S . pure) (f x)

matchMaybe :: (b -> Maybe a) -> (b -> Either a b)
matchMaybe f a = maybe (Right a) Left (f a)


closed :: Traversable f => f a -> Maybe (f b)
closed = traverse (const Nothing)


newtype Scope a f b = Scope { unScope :: f (Incr a (f b)) }
  deriving (Foldable, Functor, Traversable)

instance HFunctor (Scope a) where
  hmap f = Scope . f . fmap (fmap f) . unScope

instance Applicative f => Applicative (Scope a f) where
  pure = Scope . pure . S . pure
  Scope f <*> Scope a = Scope (liftA2 (liftA2 (<*>)) f a)

instance Monad f => Monad (Scope a f) where
  Scope e >>= f = Scope (e >>= incr (pure . Z) (>>= unScope . f))
