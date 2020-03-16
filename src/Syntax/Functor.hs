{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
module Syntax.Functor
( HFunctor(..)
, GHFunctor
) where

import qualified Control.Effect.Sum as Sum
import           GHC.Generics

class (forall f . Functor f => Functor (h f)) => HFunctor h where
  hmap :: Functor f => (forall x . f x -> g x) -> (h f a -> h g a)
  default hmap
    :: (Functor f, Generic1 (h f), Generic1 (h g), GHFunctor f g (Rep1 (h f)) (Rep1 (h g)))
    => (forall a . f a -> g a)
    -> (h f a -> h g a)
  hmap f = to1 . ghmap f . from1

instance (HFunctor l, HFunctor r) => HFunctor (l Sum.:+: r) where
  hmap f = \case
    Sum.L l -> Sum.L (hmap f l)
    Sum.R r -> Sum.R (hmap f r)
  {-# INLINE hmap #-}


class GHFunctor g g' rep rep' where
  ghmap :: Functor g => (forall x . g x -> g' x) -> rep a -> rep' a

instance GHFunctor g g' V1 V1 where
  ghmap _ = \case {}

instance GHFunctor g g' U1 U1 where
  ghmap _ = id

instance GHFunctor g g' (K1 R r) (K1 R r) where
  ghmap _ = id

instance GHFunctor g g' Par1 Par1 where
  ghmap _ = id

instance (GHFunctor g g' l l', GHFunctor g g' r r') => GHFunctor g g' (l :*: r) (l' :*: r') where
  ghmap f (l :*: r) = ghmap f l :*: ghmap f r

instance (Traversable f, GHFunctor g g' sig sig') => GHFunctor g g' (f :.: sig) (f :.: sig') where
  ghmap f = Comp1 . fmap (ghmap f) . unComp1

instance (GHFunctor g g' l l', GHFunctor g g' r r') => GHFunctor g g' (l :+: r) (l' :+: r') where
  ghmap f = \case
    L1 l -> L1 $ ghmap f l
    R1 r -> R1 $ ghmap f r

instance GHFunctor g g' f f' => GHFunctor g g' (M1 i c f) (M1 i c f') where
  ghmap f = M1 . ghmap f . unM1

instance GHFunctor g g' (Rec1 g) (Rec1 g') where
  ghmap f = Rec1 . f . unRec1

instance HFunctor sig => GHFunctor g g' (Rec1 (sig g)) (Rec1 (sig g')) where
  ghmap f = Rec1 . hmap f . unRec1
