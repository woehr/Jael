{-# Language OverloadedStrings #-}

module Main where

import Data.Either (rights)
import Data.Int (Int64)
import Data.List (intercalate, intersperse)

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T

import Jael.Grammar.Lexer
import Jael.Grammar.Parser
import Jael.Grammar.Monad
import Jael.Grammar.Token

parse :: BSL.ByteString -> [PlainToken] -- [T.Text]
parse s = case runParseMonad s pProg of
            Left x -> error $ "\"" <> show x <> "\""
            Right x -> x -- rights $ fmap T.decodeUtf8' x

--parseAndPrint :: BS.ByteString -> IO ()
--parseAndPrint = putStrLn . T.unpack . T.unwords . parse

-- Checks that a lazy bytestring is valid UTF8. Returns the indices, if any, of
-- invalid bytes.
invalidUtf8Indices :: BSL.ByteString -> [Int64]
invalidUtf8Indices xs = go xs 0 [] where
  go :: BSL.ByteString -> Int64 -> [Int64] -> [Int64]
  go bs ix acc =
    case UTF8.decode bs of
      Just (c, n) ->
        let bs' = BSL.drop n bs
            ix' = ix + n
        in if c == UTF8.replacement_char
             then go bs' ix' (ix:acc)
             else go bs' ix' acc
      Nothing -> acc

-- Converts an ordered list to a list of ranges
-- [8,7,3,2,1] -> [(1,3), (7,2)]
-- Assumes ints in xs are sorted from largest to smallest (which is what
-- checkUtf8 outputs)
intsToRanges :: [Int64] -> [(Int64, Int64)]
intsToRanges xs = go xs [] where
  go :: [Int64] -> [(Int64, Int64)] -> [(Int64, Int64)]
  go [] acc = acc
  go (y:ys) [] = go ys [(y,1)]
  go (y:ys) acc@((a,b):cs) =
    if y + 1 == a
      then go ys ((a-1,b+1):cs)
      else go ys ((y,1):acc)

-- Outputs, in ascending order, the indices and number of bytes of invalid
-- UTF8 encodings
checkUtf8 :: BSL.ByteString -> [(Int64, Int64)]
checkUtf8 = intsToRanges . invalidUtf8Indices

main :: IO ()
main = do
  --utf8TestFile <- BSL.readFile "UTF-8-test.txt"
  --putStrLn $ "Testing invalid utf8:\n" <> show (checkUtf8 utf8TestFile)
  print (parse (T.encodeUtf8 . T.pack $ ("a 1 朴 滐 z 9"::String)))
  --parseAndPrint "x y z \xffff"
  --parseAndPrint "朴滐"
  --parseAndPrint "x 1 y 2"
