module Syntax.Pretty
( -- * Precedence-sensitive pretty-printing
  Prec(..)
) where

data Prec = Prec
  { precedence :: Maybe Int
  , doc        :: String
  }
  deriving (Eq, Ord, Show)
