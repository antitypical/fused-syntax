{-# LANGUAGE DeriveGeneric, DeriveTraversable, LambdaCase, QuantifiedConstraints, StandaloneDeriving #-}
module Data.Syntax.Scope
( -- * Variables
  Var(..)
, unVar
, matchEither
, matchMaybe
, closed
  -- * Scopes
, Scope(..)
, unScope
, fromScope
, toScope
, abstract1
, abstract
, abstractEither
, instantiate1
, instantiate
, instantiateEither
  -- * Scope transformers
, ScopeT(..)
, unScopeT
, fromScopeT
, toScopeT
  -- * Prefixes
, unprefix
, unprefixEither
) where

import Control.Applicative (liftA2)
import Control.Monad.Module
import Control.Monad ((>=>), guard)
import Control.Monad.Trans.Class
import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable
import Data.Function (on)
import Data.Syntax.Functor
import Data.Syntax.Stack
import GHC.Generics (Generic, Generic1)

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

matchEither :: Applicative f => (b -> Either a c) -> b -> Var a (f c)
matchEither f x = either B (F . pure) (f x)

matchMaybe :: (b -> Maybe a) -> (b -> Either a b)
matchMaybe f a = maybe (Right a) Left (f a)


closed :: Traversable f => f a -> Maybe (f b)
closed = traverse (const Nothing)


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

toScope :: Applicative f => f (Var a b) -> Scope a f b
toScope = Scope . fmap (fmap pure)


-- | Bind occurrences of a variable in a term, producing a term in which the variable is bound.
abstract1 :: (Applicative f, Eq a) => a -> f a -> Scope () f a
abstract1 n = abstract (guard . (== n))

abstract :: Applicative f => (b -> Maybe a) -> f b -> Scope a f b
abstract f = abstractEither (matchMaybe f)

abstractEither :: Applicative f => (b -> Either a c) -> f b -> Scope a f c
abstractEither f = Scope . fmap (matchEither f) -- FIXME: succ as little of the expression as possible, cf https://twitter.com/ollfredo/status/1145776391826358273


-- | Substitute a term for the free variable in a given term, producing a closed term.
instantiate1 :: Monad f => f b -> Scope a f b -> f b
instantiate1 t = instantiate (const t)

instantiate :: Monad f => (a -> f b) -> Scope a f b -> f b
instantiate f = instantiateEither (either f pure)

instantiateEither :: Monad f => (Either a b -> f c) -> Scope a f b -> f c
instantiateEither f = unScope >=> unVar (f . Left) (>>= f . Right)


-- | Like 'Scope', but allows the inner functor to vary. Useful for syntax like declaration scopes, case alternatives, etc., which can bind variables, but cannot (directly) consist solely of them.
newtype ScopeT a t f b = ScopeT (t f (Var a (f b)))
  deriving (Foldable, Functor, Generic, Generic1, Traversable)

unScopeT :: ScopeT a t f b -> t f (Var a (f b))
unScopeT (ScopeT s) = s

instance (Applicative (t f), Applicative f) => Applicative (ScopeT a t f) where
  pure = ScopeT . pure . F . pure
  ScopeT f <*> ScopeT a = ScopeT (liftA2 (liftA2 (<*>)) f a)

instance (Monad (t f), MonadTrans t, Monad f) => Monad (ScopeT a t f) where
  ScopeT e >>= f = ScopeT (e >>= unVar (pure . B) ((>>= unScopeT . f) . lift))

instance (forall g . Functor g => Functor (t g)) => RightModule (ScopeT b t) where
  ScopeT s >>=* k = ScopeT (fmap (>>= k) <$> s)

instance MonadTrans f => MonadTrans (ScopeT a f) where
  lift = ScopeT . lift . pure . F


fromScopeT :: (RightModule t, Monad f) => ScopeT a t f b -> t f (Var a b)
fromScopeT = unScopeT >=>* sequenceA

toScopeT :: (Functor (t f), Applicative f) => t f (Var a b) -> ScopeT a t f b
toScopeT = ScopeT . fmap (fmap pure)


-- | Unwrap a (possibly-empty) prefix of @a@s wrapping a @t@ using a helper function.
--
--   This allows us to peel a prefix of syntax, typically binders, off of a term, returning a stack of prefixing values (e.g. variables) and the outermost subterm rejected by the function.
unprefix
  :: (Int -> t -> Maybe (a, t)) -- ^ A function taking the 0-based index into the prefix & the current term, and optionally returning a pair of the prefixing value and the inner subterm.
  -> t                          -- ^ The initial term.
  -> (Stack a, t)               -- ^ A stack of prefixing values & the final subterm.
unprefix from = unprefixEither (matchMaybe . from)

-- | Unwrap a (possibly-empty) prefix of @a@s wrapping a @b@ within a @t@ using a helper function.
--
--   Compared to 'unprefix', this allows the helper function to extract inner terms of a different type, for example when @t@ is a right @b@-module.
unprefixEither
  :: (Int -> t -> Either (a, t) b) -- ^ A function taking the 0-based index into the prefix & the current term, and returning either a pair of the prefixing value and the next inner subterm of type @t@, or the final inner subterm of type @b@.
  -> t                             -- ^ The initial term.
  -> (Stack a, b)                  -- ^ A stack of prefixing values & the final subterm.
unprefixEither from = go (0 :: Int) Nil
  where go i bs t = case from i t of
          Left (b, t) -> go (succ i) (bs :> b) t
          Right b     -> (bs, b)
