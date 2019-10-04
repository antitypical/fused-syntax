{-# LANGUAGE DeriveTraversable #-}
module Data.Syntax.Stack
( -- * Stacks
  Stack(..)
  -- * Prefixes
, unprefix
, unprefixEither
) where

import Data.Syntax.Var (matchMaybe)

data Stack a
  = Nil
  | Stack a :> a
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

infixl 4 :>

instance Semigroup (Stack a) where
  xs <> Nil       = xs
  xs <> (ys :> y) = (xs <> ys) :> y

instance Monoid (Stack a) where
  mempty = Nil


-- | Unwrap a (possibly-empty) prefix of @a@s wrapping a @t@ using a helper function.
--
--   This allows us to peel a prefix of syntax, typically binders, off of a term, returning a stack of prefixing values (e.g. variables) and the outermost subterm rejected by the function.
unprefix
  :: (Int -> t -> Maybe (a, t)) -- ^ A function taking the 0-based index into the prefix & the current term, and optionally returning a pair of the prefixing value and the inner subterm.
  -> t                          -- ^ The initial term.
  -> (Stack a, t)               -- ^ A stack of prefixing values & the final subterm.
unprefix from = unprefixEither (matchMaybe . from)

-- | Unwrap a (possibly-empty) prefix of @a@s wrapping a @b@ within a @t@ using a helper function.
--
--   Compared to 'unprefix', this allows the helper function to extract inner terms of a different type, for example when @t@ is a right @b@-module.
unprefixEither
  :: (Int -> t -> Either (a, t) b) -- ^ A function taking the 0-based index into the prefix & the current term, and returning either a pair of the prefixing value and the next inner subterm of type @t@, or the final inner subterm of type @b@.
  -> t                             -- ^ The initial term.
  -> (Stack a, b)                  -- ^ A stack of prefixing values & the final subterm.
unprefixEither from = go (0 :: Int) Nil
  where go i bs t = case from i t of
          Left (b, t) -> go (succ i) (bs :> b) t
          Right b     -> (bs, b)
