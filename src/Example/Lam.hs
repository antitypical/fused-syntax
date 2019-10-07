{-# LANGUAGE DeriveGeneric, DeriveTraversable, FlexibleContexts, LambdaCase, MultiParamTypeClasses, QuantifiedConstraints, StandaloneDeriving, TypeApplications, UndecidableInstances #-}
module Example.Lam
( Lam(..)
, lam
, unlam
, ($$)
, runPretty
, PrettyC(..)
) where

import Control.Effect.Pure
import Control.Effect.Reader
import GHC.Generics (Generic1)
import Syntax.Algebra
import Syntax.Functor
import Syntax.Module
import Syntax.Pretty
import Syntax.Scope
import Syntax.Sum
import Syntax.Term

data Lam t a
  = Abs (Scope () t a)
  | t a :$ t a
  deriving (Foldable, Functor, Generic1, Traversable)

infixl 9 :$

deriving instance (Eq   a, forall a . Eq   a => Eq   (f a), ScopeAlgebra sig f) => Eq   (Lam f a)
deriving instance (Ord  a, forall a . Eq   a => Eq   (f a)
                         , forall a . Ord  a => Ord  (f a), ScopeAlgebra sig f) => Ord  (Lam f a)
deriving instance (Show a, forall a . Show a => Show (f a))                     => Show (Lam f a)

instance HFunctor Lam

instance RightModule Lam where
  Abs b  >>=* f = Abs (b >>=* f)
  g :$ a >>=* f = (g >>= f) :$ (a >>= f)


lam :: (Eq a, Has Lam sig t) => a -> t a -> t a
lam v b = term (Abs (abstract1 v b))

unlam :: (Project Lam sig, RightModule sig) => a -> Term sig a -> Maybe (a, Term sig a)
unlam n t | Just (Abs b) <- prjTerm t = Just (n, instantiate1 (var n) b)
unlam _ _                             = Nothing


($$) :: Has Lam sig t => t a -> t a -> t a
f $$ a = term (f :$ a)

infixl 9 $$


runPretty :: PrettyC a -> String
runPretty = unPrec . run . runReader [] . runPrettyC

newtype PrettyC a = PrettyC { runPrettyC :: ReaderC [String] PureC (Prec String) }
  deriving (Functor)

instance Algebra Lam PrettyC where
  var _ = PrettyC (asks (atom . head))

  alg (Abs b) = PrettyC $ do
    v  <- asks @[String] (prettyVar . length)
    b' <- local (v:) (runPrettyC (unScope b))
    pure . prec 0 $ "λ " <> v <> " . " <> withPrec 0 b'
  alg (f :$ a) = PrettyC $ do
    f' <- runPrettyC f
    a' <- runPrettyC a
    pure . prec 10 $ withPrec 10 f' <> " " <> withPrec 11 a'
