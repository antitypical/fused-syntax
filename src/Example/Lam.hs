{-# LANGUAGE FlexibleContexts #-}
module Example.Lam
( Lam(..)
, lam
, ($$)
) where

import Data.Syntax.Algebra
import Data.Syntax.Scope

data Lam t a
  = Abs (Scope () t a)
  | t a :$ t a

infixl 9 :$


lam :: (Eq a, Has Lam sig t) => a -> t a -> t a
lam v b = term (Abs (abstract1 v b))

($$) :: Has Lam sig t => t a -> t a -> t a
f $$ a = term (f :$ a)

infixl 9 $$
