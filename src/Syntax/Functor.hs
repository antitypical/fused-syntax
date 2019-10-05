{-# LANGUAGE RankNTypes #-}
module Syntax.Functor
( HFunctor(..)
, Syntax(..)
-- * Generic deriving of 'HFunctor' instances.
, GHFunctor(..)
) where

import Control.Effect.Carrier (HFunctor(..), GHFunctor(..))

class HFunctor sig => Syntax sig where
  weave :: (Traversable f, Monad m, Monad n) => f () -> (forall x . f (m x) -> n (f x)) -> sig m a -> sig n (f a)
