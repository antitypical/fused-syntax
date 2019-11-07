module Syntax.Fix
( Fix(..)
) where

newtype Fix sig a = Fix { unFix :: sig (Fix sig) a }
