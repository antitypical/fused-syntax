{-# LANGUAGE ConstraintKinds, FunctionalDependencies, QuantifiedConstraints #-}
module Syntax.Algebra
( Algebra(..)
, Elem
, term
) where

import Syntax.Functor
import Syntax.Sum

class (forall f . Functor f => Functor (sig f), HFunctor sig, Functor f) => Algebra sig f | f -> sig where
  var ::       a -> f a
  alg :: sig f a -> f a


type Elem syn sig t = (Inject syn sig, Algebra sig t)

term :: Elem syn sig t => syn t a -> t a
term = alg . inj
