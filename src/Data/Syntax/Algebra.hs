{-# LANGUAGE FunctionalDependencies #-}
module Data.Syntax.Algebra
( Algebra(..)
) where

class Algebra syntax carrier | carrier -> syntax where
  gen :: a -> carrier a
  alg :: syntax carrier a -> carrier a
