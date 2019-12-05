{-# LANGUAGE DefaultSignatures, EmptyCase, FlexibleContexts, FlexibleInstances, LambdaCase, MultiParamTypeClasses, RankNTypes, QuantifiedConstraints, TypeOperators #-}
module Syntax.Traversable
( HTraversable(..)
, GHTraversable(..)
) where

import GHC.Generics
import Syntax.Foldable
import Syntax.Functor
import qualified Syntax.Sum as Sum

class ( HFoldable sig
      , HFunctor sig
      , forall g . Functor  g    => Functor     (sig g)
      , forall g . Traversable g => Traversable (sig g)
      )
   => HTraversable sig where
  htraverse
    :: (Monad f, Traversable g)
    => (forall a . g a -> f (h a))
    -> (sig g a -> f (sig h a))
  default htraverse
    :: (Monad f, Traversable g, Generic1 (sig g), Generic1 (sig h), GHTraversable g h (Rep1 (sig g)) (Rep1 (sig h)))
    => (forall a . g a -> f (h a))
    -> (sig g a -> f (sig h a))
  htraverse f = fmap to1 . ghtraverse f . from1

instance (HTraversable l, HTraversable r) => HTraversable (l Sum.:+: r)


class GHTraversable g g' rep rep' where
  ghtraverse :: (Monad f, Traversable g) => (forall x . g x -> f (g' x)) -> rep a -> f (rep' a)

instance GHTraversable g g' V1 V1 where
  ghtraverse _ = \case {}

instance GHTraversable g g' U1 U1 where
  ghtraverse _ = pure

instance GHTraversable g g' (K1 R r) (K1 R r) where
  ghtraverse _ = pure

instance GHTraversable g g' Par1 Par1 where
  ghtraverse _ = pure

instance (GHTraversable g g' l l', GHTraversable g g' r r') => GHTraversable g g' (l :*: r) (l' :*: r') where
  ghtraverse f (l :*: r) = (:*:) <$> ghtraverse f l <*> ghtraverse f r

instance (Traversable f, GHTraversable g g' sig sig') => GHTraversable g g' (f :.: sig) (f :.: sig') where
  ghtraverse f = fmap Comp1 . traverse (ghtraverse f) . unComp1

instance (GHTraversable g g' l l', GHTraversable g g' r r') => GHTraversable g g' (l :+: r) (l' :+: r') where
  ghtraverse f = \case
    L1 l -> L1 <$> ghtraverse f l
    R1 r -> R1 <$> ghtraverse f r

instance GHTraversable g g' f f' => GHTraversable g g' (M1 i c f) (M1 i c f') where
  ghtraverse f = fmap M1 . ghtraverse f . unM1

instance GHTraversable g g' (Rec1 g) (Rec1 g') where
  ghtraverse f = fmap Rec1 . f . unRec1

instance HTraversable sig => GHTraversable g g' (Rec1 (sig g)) (Rec1 (sig g')) where
  ghtraverse f = fmap Rec1 . htraverse f . unRec1
