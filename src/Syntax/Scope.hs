{-# LANGUAGE DataKinds, DeriveGeneric, DeriveTraversable, FlexibleInstances, LambdaCase, MultiParamTypeClasses, QuantifiedConstraints, StandaloneDeriving, UndecidableInstances #-}
module Syntax.Scope
( -- * Scopes
  Scope(..)
, unScope
  -- * Abstraction
, toScope
, toScopeFin
, abstract1
, abstract
, abstractVar
  -- * Instantiation
, fromScope
, fromScopeFin
, instantiate1
, instantiate
, instantiateVar
) where

import Control.Algebra (Effect(..))
import Control.Applicative (liftA2)
import Control.Monad ((<=<), guard)
import Control.Monad.Trans.Class
import Data.Bifunctor (first)
import Data.Function (on)
import GHC.Generics (Generic, Generic1)
import Syntax.Fin as Fin
import Syntax.Functor
import Syntax.Module
import Syntax.Var

newtype Scope a f b = Scope (f (Var a (f b)))
  deriving (Foldable, Functor, Generic, Generic1, Traversable)

unScope :: Scope a f b -> f (Var a (f b))
unScope (Scope s) = s

instance HFunctor (Scope a) where
  hmap f = Scope . f . fmap (fmap f) . unScope

instance Effect Traversable (Scope a) where
  handle ctx dst = toScope . fmap sequenceA . dst . (<$ ctx) . fromScope

instance (Eq  a, Eq  b, forall a . Eq  a => Eq  (f a), Monad f) => Eq  (Scope a f b) where
  (==) = (==) `on` fromScope

instance (Ord a, Ord b, forall a . Eq  a => Eq  (f a)
                      , forall a . Ord a => Ord (f a), Monad f) => Ord (Scope a f b) where
  compare = compare `on` fromScope

deriving instance (Show a, Show b, forall a . Show a => Show (f a)) => Show (Scope a f b)

instance Applicative f => Applicative (Scope a f) where
  pure = Scope . pure . F . pure
  Scope f <*> Scope a = Scope (liftA2 (liftA2 (<*>)) f a)

instance Monad f => Monad (Scope a f) where
  Scope e >>= f = Scope (e >>= unVar (pure . B) (>>= unScope . f))

instance MonadTrans (Scope a) where
  lift = Scope . pure . F

instance RightModule (Scope a) where
  Scope m >>=* f = Scope (fmap (>>= f) <$> m)


toScope :: Applicative f => f (Var a b) -> Scope a f b
toScope = abstractVar id

toScopeFin :: Applicative f => f (Var (Fin ('S n)) b) -> Scope () f (Var (Fin n) b)
toScopeFin = abstractVar (unVar (maybe (B ()) (F . B) . Fin.strengthen) (F . F))


-- | Bind occurrences of a variable in a term, producing a term in which the variable is bound.
abstract1 :: (Applicative f, Eq a) => a -> f a -> Scope () f a
abstract1 n = abstract (guard . (== n))

abstract :: Applicative f => (b -> Maybe a) -> f b -> Scope a f b
abstract f = abstractVar (fromMaybe f)

abstractVar :: Applicative f => (b -> Var a c) -> f b -> Scope a f c
abstractVar f = Scope . fmap (fmap pure . f) -- FIXME: succ as little of the expression as possible, cf https://twitter.com/ollfredo/status/1145776391826358273


fromScope :: Monad f => Scope a f b -> f (Var a b)
fromScope = instantiateVar pure

fromScopeFin :: Monad f => Scope () f (Var (Fin n) b) -> f (Var (Fin ('S n)) b)
fromScopeFin = instantiateVar (unVar (const (pure (B FZ))) (pure . first FS))


-- | Substitute a term for the free variable in a given term, producing a closed term.
instantiate1 :: Monad f => f b -> Scope a f b -> f b
instantiate1 t = instantiate (const t)

instantiate :: Monad f => (a -> f b) -> Scope a f b -> f b
instantiate f = instantiateVar (unVar f pure)

instantiateVar :: Monad f => (Var a b -> f c) -> Scope a f b -> f c
instantiateVar f = unVar (f . B) (f . F =<<) <=< unScope
