{-# LANGUAGE ConstraintKinds, FunctionalDependencies #-}
module Syntax.Algebra
( Algebra(..)
, Has
, term
) where

import Syntax.Functor
import Syntax.Sum

class (HFunctor syntax, Functor carrier) => Algebra syntax carrier | carrier -> syntax where
  gen :: a -> carrier a
  alg :: syntax carrier a -> carrier a


type Has syn sig t = (Inject syn sig, Algebra sig t)

term :: Has syn sig t => syn t a -> t a
term = alg . inj
