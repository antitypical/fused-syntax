{-# LANGUAGE DeriveGeneric, DeriveTraversable, LambdaCase #-}
module Syntax.Var
( -- * Variables
  Var(..)
, unVar
, toEither
, matchEither
, matchMaybe
, closed
) where

import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable
import GHC.Generics (Generic, Generic1)
import Syntax.Algebra

data Var a b
  = B a
  | F b
  deriving (Eq, Foldable, Functor, Generic, Generic1, Ord, Show, Traversable)

instance Bifoldable Var where
  bifoldMap f g = \case
    B a -> f a
    F a -> g a

instance Bifunctor Var where
  bimap f g = \case
    B a -> B (f a)
    F a -> F (g a)

instance Bitraversable Var where
  bitraverse f g = \case
    B a -> B <$> f a
    F a -> F <$> g a

instance Applicative (Var a) where
  pure = F
  B a <*> _ = B a
  F f <*> a = f <$> a

instance Monad (Var a) where
  B a >>= _ = B a
  F a >>= f = f a

unVar :: (a -> c) -> (b -> c) -> Var a b -> c
unVar z s = \case { B a -> z a ; F b -> s b }

toEither :: Var a b -> Either a b
toEither = unVar Left Right

matchEither :: Algebra sig f => (b -> Either a c) -> b -> Var a (f c)
matchEither f x = either B (F . gen) (f x)

matchMaybe :: (b -> Maybe a) -> (b -> Either a b)
matchMaybe f a = maybe (Right a) Left (f a)


closed :: Traversable f => f a -> Maybe (f b)
closed = traverse (const Nothing)
