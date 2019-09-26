{-# LANGUAGE RankNTypes #-}
module Data.Syntax.Functor
( HFunctor(..)
) where

-- | Higher-order functors of kind @(* -> *) -> (* -> *)@ map functors to functors.
class HFunctor h where
  -- | Higher-order functor map of a natural transformation over higher-order positions within the syntax.
  hmap :: Functor m => (forall x . m x -> n x) -> (h m a -> h n a)
