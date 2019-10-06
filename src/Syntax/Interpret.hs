{-# LANGUAGE FlexibleContexts, FlexibleInstances, FunctionalDependencies, GeneralizedNewtypeDeriving, KindSignatures, QuantifiedConstraints, RankNTypes, ScopedTypeVariables, TypeApplications, TypeOperators, UndecidableInstances #-}
module Syntax.Interpret
( runInterpret
, runInterpretState
, InterpretC(..)
, Reifies
, Handler
  -- * Re-exports
, Carrier
, run
) where

import Control.Applicative (Alternative(..))
import Control.Effect.State.Strict
import Control.Monad (MonadPlus(..))
import qualified Control.Monad.Fail as Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Coerce
import Syntax.Algebra
import Syntax.Functor
import Syntax.Sum
import Unsafe.Coerce (unsafeCoerce)


-- | A @Handler@ is a function that interprets effects described by @sig@ into the carrier monad @m@.
newtype Handler sig m =
  Handler { runHandler :: forall s x . sig (InterpretC s sig m) x -> InterpretC s sig m x }


newtype Tagged a b =
  Tagged { unTag :: b }


class Reifies s a | s -> a where
  reflect :: Tagged s a


data Skolem


-- | @Magic@ captures the GHC implementation detail of how single method type classes are implemented.
newtype Magic a r =
  Magic (Reifies Skolem a => Tagged Skolem r)


-- For more information on this technique, see the @reflection@ library. We use the formulation described in https://github.com/ekmett/reflection/issues/31 for better inlining.
--
-- Essentially we can view @k@ as internally a function of type @Reifies s a -> Tagged s r@, whch we can again view as just @a -> Tagged s r@ through @unsafeCoerce@. After this coercion, we just apply the function to @a@.
reify :: forall a r . a -> (forall s . Reifies s a => Tagged s r) -> r
reify a k =
  unsafeCoerce (Magic @a k) a


-- | Interpret an effect using a higher-order function.
--
-- Note that due to the higher-rank type, you have to use either '$' or explicit application when applying this interpreter. That is, you will need to write @runInterpret f (runInterpret g myPrgram)@ or @runInterpret f $ runInterpret g $ myProgram@. If you try and write @runInterpret f . runInterpret g@, you will unfortunately get a rather scary type error!
runInterpret
  :: forall eff m a.
     (HFunctor eff, Monad m)
  => (forall x . eff m x -> m x)
  -> (forall s . Reifies s (Handler eff m) => InterpretC s eff m a)
  -> m a
runInterpret f m =
  reify (Handler handler) (go m)

  where

    handler :: forall s x . eff (InterpretC s eff m) x -> InterpretC s eff m x
    handler e = InterpretC (f (hmap coerce e))

    go :: forall x s . InterpretC s eff m x -> Tagged s (m x)
    go m = Tagged (runInterpretC m)


-- | Interpret an effect using a higher-order function with some state variable.
runInterpretState
  :: (HFunctor eff, Monad m)
  => (forall x . s -> eff (StateC s m) x -> m (s, x))
  -> s
  -> (forall t. Reifies t (Handler eff (StateC s m)) => InterpretC t eff (StateC s m) a)
  -> m (s, a)
runInterpretState handler state m =
  runState state $
  runInterpret
    (\e -> StateC (\s -> handler s e))
    m


newtype InterpretC s (sig :: (* -> *) -> * -> *) m a = InterpretC { runInterpretC :: m a }
  deriving (Alternative, Applicative, Functor, Monad, Fail.MonadFail, MonadFix, MonadIO, MonadPlus)

instance MonadTrans (InterpretC s sig) where
  lift = InterpretC

instance (HFunctor syn, HFunctor sig, Reifies s (Handler syn m), Monad m, Algebra sig m, forall f . Functor f => Functor (syn f)) => Algebra (syn :+: sig) (InterpretC s syn m) where
  var = InterpretC . var

  alg (L syn)   = runHandler (unTag (reflect @s)) syn
  alg (R other) = InterpretC (alg (hmap coerce other))
