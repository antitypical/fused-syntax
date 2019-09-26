{-# LANGUAGE FunctionalDependencies #-}
module Data.Syntax.Algebra
( Algebra(..)
) where

class Algebra syntax carrier | carrier -> syntax where
  gen :: a -> carrier a
