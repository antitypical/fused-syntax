{-# LANGUAGE DeriveGeneric, DeriveTraversable, LambdaCase, QuantifiedConstraints, StandaloneDeriving #-}
module Syntax.Scope
( -- * Scopes
  Scope(..)
, unScope
, fromScope
, toScope
, abstract1
, abstract
, abstractEither
, instantiate1
, instantiate
, instantiateEither
) where

import Control.Applicative (liftA2)
import Control.Monad ((>=>), guard)
import Control.Monad.Trans.Class
import Data.Function (on)
import GHC.Generics (Generic, Generic1)
import Syntax.Algebra
import Syntax.Functor
import Syntax.Module
import Syntax.Var

newtype Scope a f b = Scope (f (Var a (f b)))
  deriving (Foldable, Functor, Generic, Generic1, Traversable)

unScope :: Scope a f b -> f (Var a (f b))
unScope (Scope s) = s

instance HFunctor (Scope a) where
  hmap f = Scope . f . fmap (fmap f) . unScope

instance (Eq   a, Eq   b, forall a . Eq   a => Eq   (f a), Monad f) => Eq   (Scope a f b) where
  (==) = (==) `on` fromScope

instance (Ord  a, Ord  b, forall a . Eq   a => Eq   (f a)
                        , forall a . Ord  a => Ord  (f a), Monad f) => Ord  (Scope a f b) where
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


fromScope :: Monad f => Scope a f b -> f (Var a b)
fromScope = unScope >=> sequenceA

toScope :: Algebra sig f => f (Var a b) -> Scope a f b
toScope = abstractEither (unVar Left Right)


-- | Bind occurrences of a variable in a term, producing a term in which the variable is bound.
abstract1 :: (Algebra sig f, Eq a) => a -> f a -> Scope () f a
abstract1 n = abstract (guard . (== n))

abstract :: Algebra sig f => (b -> Maybe a) -> f b -> Scope a f b
abstract f = abstractEither (matchMaybe f)

abstractEither :: Algebra sig f => (b -> Either a c) -> f b -> Scope a f c
abstractEither f = Scope . fmap (matchEither f) -- FIXME: succ as little of the expression as possible, cf https://twitter.com/ollfredo/status/1145776391826358273


-- | Substitute a term for the free variable in a given term, producing a closed term.
instantiate1 :: Monad f => f b -> Scope a f b -> f b
instantiate1 t = instantiate (const t)

instantiate :: Monad f => (a -> f b) -> Scope a f b -> f b
instantiate f = instantiateEither (either f pure)

instantiateEither :: Monad f => (Either a b -> f c) -> Scope a f b -> f c
instantiateEither f = unScope >=> unVar (f . Left) (>>= f . Right)
