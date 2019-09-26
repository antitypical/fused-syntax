module Example.Lam
( Lam(..)
) where

import Data.Syntax.Scope

data Lam t a
  = Abs (Scope () t a)
  | App (t a) (t a)
