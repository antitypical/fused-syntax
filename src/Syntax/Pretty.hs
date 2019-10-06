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

import Data.String

data Prec a = Prec
  { precedence :: Maybe Int
  , unPrec     :: a
  }
  deriving (Eq, Ord, Show)

atom :: a -> Prec a
atom = Prec Nothing

prec :: Int -> a -> Prec a
prec = Prec . Just

withPrec :: Int -> Prec String -> String
withPrec d (Prec d' a) = prettyParens (maybe False (d >) d') a


prettyParens :: (IsString a, Semigroup a) => Bool -> a -> a
prettyParens True  s = fromString "(" <> s <> fromString ")"
prettyParens False s = s

prettyVar :: Int -> String
prettyVar i = alphabet !! r : if q > 0 then show q else "" where
  (q, r) = i `divMod` 26
  alphabet = ['a'..'z']
