{-# LANGUAGE DeriveGeneric, DeriveTraversable, LambdaCase, QuantifiedConstraints, StandaloneDeriving #-}
module Data.Syntax.Scope
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
  -- * Prefixes
, unprefix
, unprefixEither
) where

import Control.Applicative (liftA2)
import Control.Monad.Module
import Control.Monad ((>=>), guard)
import Control.Monad.Trans.Class
import Data.Function (on)
import Data.Syntax.Functor
import Data.Syntax.Stack
import Data.Syntax.Var
import GHC.Generics (Generic, Generic1)

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
