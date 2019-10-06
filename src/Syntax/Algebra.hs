{-# LANGUAGE ConstraintKinds, FunctionalDependencies #-}
module Syntax.Algebra
( Algebra(..)
, Has
, term
) where

import Syntax.Functor
import Syntax.Sum

class (HFunctor sig, Functor f) => Algebra sig f | f -> sig where
  var ::       a -> f a
  alg :: sig f a -> f a


type Has syn sig t = (Inject syn sig, Algebra sig t)

term :: Has syn sig t => syn t a -> t a
term = alg . inj
