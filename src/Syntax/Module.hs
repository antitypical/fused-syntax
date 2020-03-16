{-# LANGUAGE DefaultSignatures, EmptyCase, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, QuantifiedConstraints, StandaloneDeriving, TypeOperators, UndecidableInstances #-}
module Syntax.Module
( -- * Right modules
  RightModule(..)
, (>>*)
, (=<<*)
, (>=>*)
, (<=<*)
, joinr
  -- * Left modules
, LeftModule(..)
, (*>>)
, (*=<<)
, (*>=>)
, (*<=<)
, joinl
  -- * Generic derivation of 'RightModule'
, GRightModule(..)
) where

import Control.Monad.Trans.Class
import GHC.Generics
import Syntax.Functor
import qualified Syntax.Sum as Sum

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
class (HFunctor f, forall g . Functor g => Functor (f g)) => RightModule f where
  (>>=*) :: Monad m => f m a -> (a -> m b) -> f m b
  default (>>=*) :: (Generic1 (f m), GRightModule m (Rep1 (f m))) => f m a -> (a -> m b) -> f m b
  f >>=* k = to1 (gbindR (from1 f) k)

  infixl 1 >>=*

(>>*) :: (Monad m, RightModule f) => f m a -> m b -> f m b
l >>* r = l >>=* const r

infixl 1 >>*

(=<<*) :: (Monad m, RightModule f) => (a -> m b) -> f m a -> f m b
(=<<*) = flip (>>=*)

infixr 1 =<<*


(>=>*) :: (RightModule f, Monad m) => (a -> f m b) -> (b -> m c) -> (a -> f m c)
f >=>* g = \x -> f x >>=* g

infixr 1 >=>*

(<=<*) :: (RightModule f, Monad m) => (b -> m c) -> (a -> f m b) -> (a -> f m c)
g <=<* f = \x -> f x >>=* g

infixr 1 <=<*


joinr :: (RightModule f, Monad m) => f m (m a) -> f m a
joinr = (>>=* id)


instance (RightModule f, RightModule g) => RightModule (f Sum.:+: g) where
  s >>=* f = case s of
    Sum.L l -> Sum.L (l >>=* f)
    Sum.R r -> Sum.R (r >>=* f)


class (HFunctor f, forall g . Functor g => Functor (f g)) => LeftModule f where
  (*>>=) :: Monad m => m a -> (a -> f m b) -> f m b
  default (*>>=) :: (Monad m, MonadTrans f, Monad (f m)) => m a -> (a -> f m b) -> f m b
  m *>>= f = lift m >>= f

  infixl 1 *>>=

(*>>) :: (Monad m, LeftModule f) => m a -> f m b -> f m b
l *>> r = l *>>= const r

infixl 1 *>>

(*=<<) :: (Monad m, LeftModule f) => (a -> f m b) -> m a -> f m b
(*=<<) = flip (*>>=)

infixr 1 *=<<


(*>=>) :: (LeftModule f, Monad m) => (a -> m b) -> (b -> f m c) -> (a -> f m c)
f *>=> g = \x -> f x *>>= g

infixr 1 *>=>

(*<=<) :: (LeftModule f, Monad m) => (b -> f m c) -> (a -> m b) -> (a -> f m c)
g *<=< f = \x -> f x *>>= g

infixr 1 *<=<


joinl :: (LeftModule f, Monad m) => m (f m a) -> f m a
joinl = (*>>= id)


class GRightModule m f where
  gbindR :: f a -> (a -> m b) -> f b

deriving instance GRightModule m f => GRightModule m (M1 i c f)

instance (GRightModule m l, GRightModule m r) => GRightModule m (l :*: r) where
  gbindR (l :*: r) k = gbindR l k :*: gbindR r k

instance (GRightModule m l, GRightModule m r) => GRightModule m (l :+: r) where
  gbindR (L1 l) k = L1 (gbindR l k)
  gbindR (R1 r) k = R1 (gbindR r k)

instance (Functor l, GRightModule m r) => GRightModule m (l :.: r) where
  gbindR (Comp1 b) k = Comp1 ((`gbindR` k) <$> b)

instance (RightModule f, Monad m) => GRightModule m (Rec1 (f m)) where
  gbindR r k = Rec1 (unRec1 r >>=* k)

instance Monad m => GRightModule m (Rec1 m) where
  gbindR r k = Rec1 (unRec1 r >>= k)

instance GRightModule m (K1 R k) where
  gbindR (K1 c) _ = (K1 c)

instance GRightModule m U1 where
  gbindR U1 _ = U1

instance GRightModule m V1 where
  gbindR v _ = case v of {}
