{-# LANGUAGE FunctionalDependencies #-}
module Data.Syntax.Algebra
( Algebra(..)
) where

import Data.Syntax.Functor

class HFunctor syntax => Algebra syntax carrier | carrier -> syntax where
  gen :: a -> carrier a
  alg :: syntax carrier a -> carrier a
