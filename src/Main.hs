{-# Language OverloadedStrings #-}

module Main where

import qualified Data.ByteString               as BS
import           Text.Show.Pretty               ( ppShow )

import           Jael.Grammar.AST               ( JaelExpr )
import           Jael.Grammar.Monad             ( runParseMonad )
import           Jael.Grammar.Parser            ( pProg )

parse :: BS.ByteString -> JaelExpr
parse s = case runParseMonad s pProg of
  Left  x -> error $ "\"" <> show x <> "\""
  Right x -> x

main :: IO ()
main = putStrLn . ppShow . parse $ "1 + 2 - 3 * 4"
