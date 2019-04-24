{-# Language DeriveFunctor #-}
{-# Language PatternSynonyms #-}

module Jael.Grammar.Token where

import Control.Applicative.Lift

import Jael.Grammar.Located

data Token a
  = TokenDec a
  | TokenAlpha a
  | TokenEOF
  deriving (Eq, Functor, Show)

pattern IgnoreLocation :: Token a -> Located (Token a)
pattern IgnoreLocation x <- Located _ _ x

pattern EOF :: Located (Token a)
pattern EOF <- IgnoreLocation TokenEOF
