{-# LANGUAGE ConstraintKinds, FunctionalDependencies #-}
module Syntax.Algebra
( Algebra(..)
, Has
, term
) where

import Syntax.Functor
import Syntax.Sum

class (HFunctor syntax, Functor f) => Algebra syntax f | f -> syntax where
  gen :: a -> f a
  alg :: syntax f a -> f a


type Has syn sig t = (Inject syn sig, Algebra sig t)

term :: Has syn sig t => syn t a -> t a
term = alg . inj
