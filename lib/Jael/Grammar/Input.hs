{-# Language PatternSynonyms #-}

module Jael.Grammar.Input where

import           Data.ByteString.Internal       ( w2c )
import           Data.Word                      ( Word8 )

import qualified Data.ByteString               as BS

import           Jael.Grammar.Token

-- byte in file; "address" -- line; 0x0A is a newline -- column
type AlexPosn = (Int, Int, Int)

-- position -- previous -- input
type AlexInput = (AlexPosn, Word8, BS.ByteString)

pattern Newline :: Word8
pattern Newline = 0x0A

alexInitialPosition :: AlexPosn
alexInitialPosition = (0, 1, 1)

advance :: AlexPosn -> Word8 -> AlexPosn
advance (a, l, _) Newline = (a + 1, l + 1, 1)
advance (a, l, c) _       = (a + 1, l, c + 1)

posnDiff :: AlexPosn -> AlexPosn -> Int
posnDiff (x1, _, _) (x2, _, _) = fromIntegral (x2 - x1)

posnSkip :: AlexPosn -> Int -> AlexPosn
posnSkip (a, l, c) n = (a + n, l, c + 1)

alexInitialInput :: BS.ByteString -> AlexInput
alexInitialInput x = (alexInitialPosition, Newline, x)

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (_, c, _) = w2c c

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte (pos, _, i) = case BS.uncons i of
  Just (b, rest) -> Just (b, (advance pos b, b, rest))
  Nothing        -> Nothing

decorate :: PlainToken -> AlexPosn -> DecoratedToken
decorate t (a, l, c) =
  DecoratedToken { tokenOffset = a, tokenPosn = (l, c), tokenPlain = t }
