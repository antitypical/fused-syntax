{-# LANGUAGE DeriveGeneric, DeriveTraversable, LambdaCase, QuantifiedConstraints, StandaloneDeriving #-}
module Data.Syntax.Trans.Scope
( -- * Scope transformers
  ScopeT(..)
, unScopeT
, fromScopeT
, toScopeT
, abstract1T
, abstractT
, abstractTEither
, instantiate1T
, instantiateT
, instantiateTEither
) where

import Control.Applicative (liftA2)
import Control.Monad.Module
import Control.Monad (guard)
import Control.Monad.Trans.Class
import Data.Function (on)
import Data.Syntax.Algebra
import Data.Syntax.Functor
import Data.Syntax.Var
import GHC.Generics (Generic, Generic1)

-- | Like 'Scope', but allows the inner functor to vary. Useful for syntax like declaration scopes, case alternatives, etc., which can bind variables, but cannot (directly) consist solely of them.
newtype ScopeT a t f b = ScopeT (t f (Var a (f b)))
  deriving (Foldable, Functor, Generic, Generic1, Traversable)

unScopeT :: ScopeT a t f b -> t f (Var a (f b))
unScopeT (ScopeT s) = s

instance (RightModule t, Monad f, Eq  a, Eq  b, forall a . Eq  a => Eq  (t f a)) => Eq  (ScopeT a t f b) where
  (==) = (==) `on` fromScopeT

instance (RightModule t, Monad f, Ord a, Ord b, forall a . Eq  a => Eq  (t f a)
                                              , forall a . Ord a => Ord (t f a)) => Ord (ScopeT a t f b) where
  compare = compare `on` fromScopeT

deriving instance (Show a, Show b, forall a . Show a => Show (t f a)
                                 , forall a . Show a => Show (f a)) => Show (ScopeT a t f b)

instance (Applicative (t f), Applicative f) => Applicative (ScopeT a t f) where
  pure = ScopeT . pure . F . pure
  ScopeT f <*> ScopeT a = ScopeT (liftA2 (liftA2 (<*>)) f a)

instance (Monad (t f), MonadTrans t, Monad f) => Monad (ScopeT a t f) where
  ScopeT e >>= f = ScopeT (e >>= unVar (pure . B) ((>>= unScopeT . f) . lift))

instance (HFunctor t, forall g . Functor g => Functor (t g)) => RightModule (ScopeT b t) where
  ScopeT s >>=* k = ScopeT (fmap (>>= k) <$> s)

instance MonadTrans f => MonadTrans (ScopeT a f) where
  lift = ScopeT . lift . pure . F

instance (HFunctor t, forall g . Functor g => Functor (t g)) => HFunctor (ScopeT a t) where
  hmap f = ScopeT . hmap f . fmap (fmap f) . unScopeT


fromScopeT :: (RightModule t, Monad f) => ScopeT a t f b -> t f (Var a b)
fromScopeT = unScopeT >=>* sequenceA

toScopeT :: (Functor (t f), Applicative f) => t f (Var a b) -> ScopeT a t f b
toScopeT = ScopeT . fmap (fmap pure)


-- | Bind occurrences of a variable in a term, producing a term in which the variable is bound.
abstract1T :: (Functor (t f), Algebra sig f, Eq a) => a -> t f a -> ScopeT () t f a
abstract1T n = abstractT (guard . (== n))

abstractT :: (Functor (t f), Algebra sig f) => (b -> Maybe a) -> t f b -> ScopeT a t f b
abstractT f = abstractTEither (matchMaybe f)

abstractTEither :: (Functor (t f), Algebra sig f) => (b -> Either a c) -> t f b -> ScopeT a t f c
abstractTEither f = ScopeT . fmap (matchEither f) -- FIXME: succ as little of the expression as possible, cf https://twitter.com/ollfredo/status/1145776391826358273


-- | Substitute a term for the free variable in a given term, producing a closed term.
instantiate1T :: (RightModule t, Monad f) => f b -> ScopeT a t f b -> t f b
instantiate1T t = instantiateT (const t)

instantiateT :: (RightModule t, Monad f) => (a -> f b) -> ScopeT a t f b -> t f b
instantiateT f = instantiateTEither (either f pure)

instantiateTEither :: (RightModule t, Monad f) => (Either a b -> f c) -> ScopeT a t f b -> t f c
instantiateTEither f = unScopeT >=>* unVar (f . Left) (>>= f . Right)
