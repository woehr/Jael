{-# Language PatternSynonyms #-}

module Jael.Grammar.Input where

import Data.Bifunctor (first)
import Data.ByteString.Internal (w2c)
import Data.ByteString.Lazy.UTF8 (decode, replacement_char)
import Data.Int (Int64)
import Data.Word (Word8)

import qualified Data.ByteString.Lazy as BSL

import Jael.Grammar.Token

data DecodedBytes = ValidUTF8 [Word8]
                  | InvalidUTF8 Int
                  deriving (Eq, Show)

type AlexPosn = ( Int             -- byte in file; "address"
                , Int             -- line; 0x0A is a newline
                , Int             -- column
                )

type AlexInput = ( AlexPosn       -- position
                 , Word8          -- previous
                 , DecodedBytes   -- future bytes
                 , BSL.ByteString -- input
                 )

pattern Newline :: Word8
pattern Newline = 0x0A

pattern Replacement :: Char
pattern Replacement = '\xFFFD'

decodeBS :: BSL.ByteString -> DecodedBytes
decodeBS i = case decode i of
  Just (Replacement, numBytes) -> InvalidUTF8 (fromIntegral numBytes)
  Just (_,           numBytes) -> ValidUTF8 (BSL.unpack (BSL.take numBytes i))
  Nothing                      -> ValidUTF8 []

alexInitialPosition :: AlexPosn
alexInitialPosition = (0, 1, 1)

advance :: AlexPosn -> Word8 -> AlexPosn
advance (a, l, c) Newline = (a+1, l+1, 1  )
advance (a, l, c) _       = (a+1, l,   c+1)

posnDiff :: AlexInput -> AlexInput -> Int64
posnDiff ((x1, _, _), _, _, _) ((x2, _, _), _, _, _) = fromIntegral (x2 - x1)

posnSkip :: AlexPosn -> Int -> AlexPosn
posnSkip (a, l, c) n = (a+n, l, c+1)

alexInitialInput :: BSL.ByteString -> AlexInput
alexInitialInput x = (alexInitialPosition, Newline, decodeBS x, x)

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (_, c, _, _) = w2c c

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte (pos, p, ValidUTF8 (b:[]), i) =
  let pos' = advance pos b
      i'   = BSL.drop 1 i
      bs   = decodeBS i'
  in  pos' `seq` i' `seq` bs `seq` Just (b, (pos', b, bs, i'))
alexGetByte (pos, p, ValidUTF8 (b:bs), i) =
  let pos' = advance pos b
      i'   = BSL.drop 1 i
  in  pos' `seq` i' `seq` Just (b, (pos', b, ValidUTF8 bs, i'))
alexGetByte (_, _, ValidUTF8 [], _) = Nothing
alexGetByte (_, _, InvalidUTF8 _, _) = Nothing

decorate :: PlainToken a -> AlexPosn -> DecoratedToken a
decorate t (a, l, c) = DecoratedToken
  { tokenOffset = a
  , tokenPosn   = (l, c)
  , tokenPlain  = t
  }

mkToken :: (BSL.ByteString -> PlainToken S)
        -> BSL.ByteString
        -> AlexPosn
        -> DecoratedToken S
mkToken f i p = decorate (f i) p