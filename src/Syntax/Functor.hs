{-# LANGUAGE QuantifiedConstraints, RankNTypes #-}
module Syntax.Functor
( HFunctor(..)
, Syntax
-- * Generic deriving of 'HFunctor' instances.
, GHFunctor(..)
) where

import Control.Effect.Carrier (HFunctor(..), GHFunctor(..))

class (HFunctor sig, forall f . Functor f => Functor (sig f)) => Syntax sig
