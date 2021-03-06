{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
module Syntax.Algebra
( Algebra(..)
, Has
, send
) where

import Syntax.Functor
import Syntax.Sum

class (HFunctor sig, Applicative t) => Algebra sig t | t -> sig where
  alg :: sig t a -> t a


-- | @m@ is a carrier for @sig@ containing @eff@.
--
-- Note that if @eff@ is a sum, it will be decomposed into multiple 'Member' constraints. While this technically allows one to combine multiple unrelated effects into a single 'Has' constraint, doing so has two significant drawbacks:
--
-- 1. Due to [a problem with recursive type families](https://gitlab.haskell.org/ghc/ghc/issues/8095), this can lead to significantly slower compiles.
--
-- 2. It defeats @ghc@’s warnings for redundant constraints, and thus can lead to a proliferation of redundant constraints as code is changed.
type Has eff sig m = (Members eff sig, Algebra sig m)

-- | Construct a request for an effect to be interpreted by some handler later on.
send :: (Member eff sig, Algebra sig m) => eff m a -> m a
send = alg . inj
{-# INLINE send #-}
