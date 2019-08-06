{-# Language OverloadedStrings #-}

module Main where

import Data.Either (rights)
import Data.List (intersperse)

import qualified Data.ByteString.Lazy as BS
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T

import Jael.Grammar.Lexer
import Jael.Grammar.Parser
import Jael.Grammar.Monad

parse :: BS.ByteString -> [T.Text]
parse s = case runParseMonad s pProg of
            Left x -> error $ show x
            Right x -> rights $ fmap T.decodeUtf8' x

parseAndPrint :: BS.ByteString -> IO ()
parseAndPrint = putStrLn . T.unpack . T.unwords . parse

main :: IO ()
main = do
  parseAndPrint "x y z \xffff"
  parseAndPrint "1 2 3"
  parseAndPrint "x 1 y 2"
