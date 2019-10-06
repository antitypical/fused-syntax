module Syntax.Pretty
( -- * Precedence-sensitive pretty-printing
  Prec(..)
, atom
, prec
  -- * Conveniences
, prettyParens
) where

data Prec = Prec
  { precedence :: Maybe Int
  , unPrec     :: String
  }
  deriving (Eq, Ord, Show)

atom :: String -> Prec
atom = Prec Nothing

prec :: Int -> String -> Prec
prec = Prec . Just


prettyParens :: Bool -> String -> String
prettyParens True  s = "(" <> s <> ")"
prettyParens False s = s
