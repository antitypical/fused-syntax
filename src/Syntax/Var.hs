{-# LANGUAGE DataKinds, DeriveGeneric, DeriveTraversable, LambdaCase #-}
module Syntax.Var
( -- * Variables
  Var(..)
  -- * Eliminating
, unVar
  -- * Converting
, toEither
, fromEither
, fromMaybe
, closed
, strengthen
) where

import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable
import GHC.Generics (Generic, Generic1)
import Syntax.Fin hiding (strengthen)

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
{-# INLINE unVar #-}


toEither :: Var a b -> Either a b
toEither = unVar Left Right

fromEither :: Either a b -> Var a b
fromEither = either B F

fromMaybe :: (b -> Maybe a) -> (b -> Var a b)
fromMaybe f a = maybe (F a) B (f a)


closed :: Traversable f => f a -> Maybe (f b)
closed = traverse (const Nothing)


strengthen :: Functor f => f (Var (Fin 'Z) a) -> f a
strengthen = fmap (unVar absurd id)
