{-# Language QuasiQuotes #-}

module Jael.Grammar.Error where

import Text.Scanf ((:+)(..), fmt, scanf)

import Jael.Grammar.Located

data ParseError = UnicodeDecodeError String
                | LexicalError
                | ErrorMessage String
                deriving (Eq, Show)

parseAlexError :: String -> Located ParseError
parseAlexError s
  | Just (r :+ c :+ ()) <- scanf [fmt|lexical error at line %d, column %d|] s
  = Located Nothing (Just (r,c)) LexicalError
  | otherwise = Located Nothing Nothing (ErrorMessage s)
