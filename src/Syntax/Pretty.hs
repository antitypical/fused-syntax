module Syntax.Pretty
( -- * Precedence-sensitive pretty-printing
  Prec(..)
, atom
, prec
) where

data Prec = Prec
  { precedence :: Maybe Int
  , doc        :: String
  }
  deriving (Eq, Ord, Show)

atom :: String -> Prec
atom = Prec Nothing

prec :: Int -> String -> Prec
prec = Prec . Just
