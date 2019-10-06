{-# LANGUAGE FlexibleContexts, FlexibleInstances, FunctionalDependencies, GeneralizedNewtypeDeriving, KindSignatures, QuantifiedConstraints, RankNTypes, ScopedTypeVariables, TypeApplications, TypeOperators, UndecidableInstances #-}
module Syntax.Interpret
( runInterpret
, runInterpretState
, InterpretC(..)
, Reifies
, Alg
  -- * Re-exports
, Carrier
, run
) where

import Control.Effect.State.Strict
import Data.Coerce
import Syntax.Algebra
import Syntax.Functor
import Unsafe.Coerce (unsafeCoerce)


-- | An @Alg@ is a function that interprets syntax described by @sig@ into the carrier type @m@.
data Alg sig m = Alg
  { runVar :: forall s x .                          x -> InterpretC s sig m x
  , runAlg :: forall s x . sig (InterpretC s sig m) x -> InterpretC s sig m x }


newtype Tagged a b = Tagged { unTag :: b }


class Reifies s a | s -> a where
  reflect :: Tagged s a


data Skolem


-- | @Magic@ captures the GHC implementation detail of how single method type classes are implemented.
newtype Magic a r = Magic (Reifies Skolem a => Tagged Skolem r)


-- For more information on this technique, see the @reflection@ library. We use the formulation described in https://github.com/ekmett/reflection/issues/31 for better inlining.
--
-- Essentially we can view @k@ as internally a function of type @Reifies s a -> Tagged s r@, whch we can again view as just @a -> Tagged s r@ through @unsafeCoerce@. After this coercion, we just apply the function to @a@.
reify :: forall a r . a -> (forall s . Reifies s a => Tagged s r) -> r
reify a k = unsafeCoerce (Magic @a k) a


-- | Interpret an effect using a higher-order function.
--
-- Note that due to the higher-rank type, you have to use either '$' or explicit application when applying this interpreter. That is, you will need to write @runInterpret a f (runInterpret b g myPrgram)@ or @runInterpret a f $ runInterpret b g $ myProgram@. If you try and write @runInterpret a f . runInterpret b g@, you will unfortunately get a rather scary type error!
runInterpret
  :: (HFunctor sig, Functor m)
  => (forall x .       x -> m x)
  -> (forall x . sig m x -> m x)
  -> (forall s . Reifies s (Alg sig m) => InterpretC s sig m a)
  -> m a
runInterpret var alg m = reify (Alg (InterpretC . var) (InterpretC . alg . hmap coerce)) (go m) where
  go :: InterpretC s sig m x -> Tagged s (m x)
  go m = Tagged (runInterpretC m)


-- | Interpret an effect using a higher-order function with some state variable.
runInterpretState
  :: (HFunctor sig, Functor m)
  => (forall x . s ->                  x -> m (s, x))
  -> (forall x . s -> sig (StateC s m) x -> m (s, x))
  -> s
  -> (forall t. Reifies t (Alg sig (StateC s m)) => InterpretC t sig (StateC s m) a)
  -> m (s, a)
runInterpretState var alg state m = runState state
  $ runInterpret (\ x -> StateC (\ s -> var s x)) (\ x -> StateC (\ s -> alg s x)) m


newtype InterpretC s (sig :: (* -> *) -> * -> *) m a = InterpretC { runInterpretC :: m a }
  deriving (Applicative, Functor, Monad)

instance (HFunctor sig, Reifies s (Alg sig m), forall f . Functor f => Functor (sig f), Functor m) => Algebra sig (InterpretC s sig m) where
  var = runVar (unTag (reflect @s))

  alg = runAlg (unTag (reflect @s))
