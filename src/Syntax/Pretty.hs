module Syntax.Pretty
( -- * Precedence-sensitive pretty-printing
  Prec(..)
, atom
) where

data Prec = Prec
  { precedence :: Maybe Int
  , doc        :: String
  }
  deriving (Eq, Ord, Show)

atom :: String -> Prec
atom = Prec Nothing
