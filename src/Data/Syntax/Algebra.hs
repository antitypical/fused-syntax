{-# LANGUAGE ConstraintKinds, FunctionalDependencies #-}
module Data.Syntax.Algebra
( Algebra(..)
, Has
, term
) where

import Data.Syntax.Functor
import Data.Syntax.Sum

class (HFunctor syntax, Applicative carrier) => Algebra syntax carrier | carrier -> syntax where
  gen :: a -> carrier a
  alg :: syntax carrier a -> carrier a


type Has syn sig t = (Inject syn sig, Algebra sig t)

term :: Has syn sig t => syn t a -> t a
term = alg . inj
