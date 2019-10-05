module Syntax.Module
( -- * Right modules
  RightModule(..)
, (>=>*)
, (<=<*)
, joinr
  -- * Left modules
, LeftModule(..)
, (*>=>)
, (*<=<)
, joinl
) where

import Syntax.Functor

-- | Modules over monads allow lifting of a monad’s product (i.e. 'Control.Monad.join') into another structure composed with the monad. A right-module @f m@ over a monad @m@ therefore allows one to extend @m@’s '>>=' operation to values of @f m@ using the '>>=*' operator.
--
--   In practical terms, this means that we can describe syntax which cannot itself bind or be substituted for variables, but which can be substituted inside when containing a substitutable expression monad. For example, we might not want to allow variables in a declaration context, but might still want to be able to substitute for e.g. globally-bound variables inside declarations; a 'RightModule' instance expresses this relationship nicely.
--
--   Note that we are calling this a right-module following Maciej Piróg, Nicolas Wu, & Jeremy Gibbons in _Modules Over Monads and their Algebras_; confusingly, other sources refer to this as a left-module.
--
--   Laws:
--
--   Right-identity:
--
-- @
-- m >>=* return = m
-- @
--
--   Associativity:
--
-- @
-- m >>=* (k >=> h) = (m >>=* k) >>=* h
-- @
class HFunctor f => RightModule f where
  (>>=*) :: Monad m => f m a -> (a -> m b) -> f m b
  infixl 1 >>=*


(>=>*) :: (RightModule f, Monad m) => (a -> f m b) -> (b -> m c) -> (a -> f m c)
f >=>* g = \x -> f x >>=* g

infixr 1 >=>*

(<=<*) :: (RightModule f, Monad m) => (b -> m c) -> (a -> f m b) -> (a -> f m c)
g <=<* f = \x -> f x >>=* g

infixr 1 <=<*


joinr :: (RightModule f, Monad m) => f m (m a) -> f m a
joinr = (>>=* id)


class HFunctor f => LeftModule f where
  (*>>=) :: Monad m => m a -> (a -> f m b) -> f m b
  infixl 1 *>>=


(*>=>) :: (LeftModule f, Monad m) => (a -> m b) -> (b -> f m c) -> (a -> f m c)
f *>=> g = \x -> f x *>>= g

infixr 1 *>=>

(*<=<) :: (LeftModule f, Monad m) => (b -> f m c) -> (a -> m b) -> (a -> f m c)
g *<=< f = \x -> f x *>>= g

infixr 1 *<=<


joinl :: (LeftModule f, Monad m) => m (f m a) -> f m a
joinl = (*>>= id)
