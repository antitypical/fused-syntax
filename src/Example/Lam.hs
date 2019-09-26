{-# LANGUAGE FlexibleContexts #-}
module Example.Lam
( Lam(..)
, ($$)
) where

import Data.Syntax.Algebra
import Data.Syntax.Scope

data Lam t a
  = Abs (Scope () t a)
  | App (t a) (t a)

($$) :: Has Lam sig t => t a -> t a -> t a
f $$ a = term (App f a)

infixl 9 $$
