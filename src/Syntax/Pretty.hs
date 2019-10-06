module Syntax.Pretty
( -- * Precedence-sensitive pretty-printing
  Prec(..)
, atom
, prec
, withPrec
  -- * Conveniences
, prettyParens
, prettyVar
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

withPrec :: Int -> Prec -> String
withPrec d (Prec d' a) = prettyParens (maybe False (d >) d') a


prettyParens :: Bool -> String -> String
prettyParens True  s = "(" <> s <> ")"
prettyParens False s = s

prettyVar :: Int -> String
prettyVar i = alphabet !! r : if q > 0 then show q else "" where
  (q, r) = i `divMod` 26
  alphabet = ['a'..'z']
