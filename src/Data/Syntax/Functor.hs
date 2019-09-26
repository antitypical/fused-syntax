{-# LANGUAGE DefaultSignatures, EmptyCase, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, RankNTypes, TypeOperators #-}
module Data.Syntax.Functor
( HFunctor(..)
-- * Generic deriving of 'HFunctor' instances.
, GHFunctor(..)
) where

import Data.Coerce
import GHC.Generics

-- | Higher-order functors of kind @(* -> *) -> (* -> *)@ map functors to functors.
class HFunctor h where
  -- | Higher-order functor map of a natural transformation over higher-order positions within the syntax.
  --
  -- A definition for 'hmap' over first-order syntax can be derived automatically provided a 'Generic1' instance is available.
  hmap :: Functor m => (forall x . m x -> n x) -> (h m a -> h n a)
  default hmap
    :: ( Functor m
       , Generic1 (h m)
       , Generic1 (h n)
       , GHFunctor m n (Rep1 (h m)) (Rep1 (h n))
       )
    => (forall x . m x -> n x)
    -> (h m a -> h n a)
  hmap f = to1 . ghmap f . from1
  {-# INLINE hmap #-}


-- | Generic implementation of 'HFunctor'.
class GHFunctor m m' rep rep' where
  -- | Generic implementation of 'hmap'.
  ghmap :: Functor m => (forall x . m x -> m' x) -> (rep a -> rep' a)

instance GHFunctor m m' rep rep' => GHFunctor m m' (M1 i c rep) (M1 i c rep') where
  ghmap f = M1 . ghmap f . unM1
  {-# INLINE ghmap #-}

instance (GHFunctor m m' l l', GHFunctor m m' r r') => GHFunctor m m' (l :+: r) (l' :+: r') where
  ghmap f (L1 l) = L1 (ghmap f l)
  ghmap f (R1 r) = R1 (ghmap f r)
  {-# INLINE ghmap #-}

instance (GHFunctor m m' l l', GHFunctor m m' r r') => GHFunctor m m' (l :*: r) (l' :*: r') where
  ghmap f (l :*: r) = ghmap f l :*: ghmap f r
  {-# INLINE ghmap #-}

instance GHFunctor m m' V1 V1 where
  ghmap _ v = case v of {}
  {-# INLINE ghmap #-}

instance GHFunctor m m' U1 U1 where
  ghmap _ = id
  {-# INLINE ghmap #-}

instance GHFunctor m m' (K1 R c) (K1 R c) where
  ghmap _ = coerce
  {-# INLINE ghmap #-}

instance GHFunctor m m' Par1 Par1 where
  ghmap _ = coerce
  {-# INLINE ghmap #-}

instance (Functor f, GHFunctor m m' g g') => GHFunctor m m' (f :.: g) (f :.: g') where
  ghmap f = Comp1 . fmap (ghmap f) . unComp1
  {-# INLINE ghmap #-}

instance GHFunctor m m' (Rec1 m) (Rec1 m') where
  ghmap f = Rec1 . f . unRec1
  {-# INLINE ghmap #-}

instance HFunctor f => GHFunctor m m' (Rec1 (f m)) (Rec1 (f m')) where
  ghmap f = Rec1 . hmap f . unRec1
  {-# INLINE ghmap #-}
