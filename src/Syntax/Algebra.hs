{-# LANGUAGE FunctionalDependencies #-}
module Syntax.Algebra
( Algebra(..)
) where

import Syntax.Functor

class (HFunctor sig, Applicative t) => Algebra sig t | t -> sig where
  alg :: sig t a -> t a
