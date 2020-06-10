{-# Language DataKinds #-}
{-# Language DeriveFunctor #-}
{-# Language MultiWayIf #-}
{-# Language PatternSynonyms #-}
{-# Language TypeFamilies #-}
{-# Language TypeOperators #-}
{-# Language UndecidableInstances #-}

module Jael.Grammar.Token where

import Data.Char (ord)
import Data.Foldable (foldlM)
import Data.Maybe (fromMaybe)

import Data.OpenADT (OpenADT)
import Data.Row (type (.==), type (.+), Row)
import Data.Text.Short.Unsafe (fromByteStringUnsafe)

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as C8
import qualified Data.Row.Internal as RI
import qualified Data.Text.Short as T

-- S for string type
type S = T.ShortText

toS :: BSL.ByteString -> S
toS = fromByteStringUnsafe . BSL.toStrict

data IntInfo = IntInfo
  { intValue :: Integer
  , intDigits :: Integer
  } deriving (Eq, Show)

data PlainToken a
  = TokenComment a
  | TokenSymbol a
  | TokenBinInt IntInfo
  | TokenOctInt IntInfo
  | TokenDecInt IntInfo
  | TokenHexInt IntInfo
  | TokenLower a
  | TokenUpper a
  | TokenInvalid Int -- Valid utf8 but not "lexable"
  | TokenBadUTF8 Int -- Invalid utf8 encoded bytes
  | TokenEOF
  deriving (Eq, Show)

data DecoratedToken a = DecoratedToken
  { tokenOffset :: Int
  , tokenPosn   :: (Int, Int)
  , tokenPlain  :: PlainToken a
  } deriving (Eq, Show)

pattern IgnoreDecorations :: PlainToken a -> DecoratedToken a
pattern IgnoreDecorations x <- DecoratedToken { tokenPlain = x }

lowerIdent, upperIdent :: BSL.ByteString -> PlainToken S
lowerIdent = TokenLower . toS
upperIdent = TokenUpper . toS

-- Total, integer version.
-- https://hackage.haskell.org/package/base-4.11.1.0/docs/src/Data.Char.html#digitToInt
digitToInt :: Char -> Maybe Integer
digitToInt c = toInteger <$> if
  | (fromIntegral dec  :: Word) <= 9 -> Just dec
  | (fromIntegral hexl :: Word) <= 5 -> Just $ hexl + 10
  | (fromIntegral hexu :: Word) <= 5 -> Just $ hexu + 10
  | otherwise                        -> Nothing
 where
  dec  = ord c - ord '0'
  hexl = ord c - ord 'a'
  hexu = ord c - ord 'A'

number :: Integer -> String -> Integer
number base xs =
  fromMaybe
      (error $ "Grammar error: base " <> show base <> " for string " <> show xs)
    $ foldlM (\x d -> (base * x +) <$> (digitToInt d)) 0 xs

parseInteger :: BSL.ByteString -> PlainToken a
parseInteger n =
  case filter (/= '_') $ C8.unpack n of
    '~':s ->
      TokenDecInt IntInfo
        { intValue  = -(number 10 s)
        , intDigits = toInteger $ length s
        }
    '0':'b':s ->
      TokenBinInt IntInfo
        { intValue  = number 2 s
        , intDigits = toInteger $ length s
        }
    '0':'o':s ->
      TokenOctInt IntInfo
        { intValue  = number 8 s
        , intDigits = toInteger $ length s
        }
    '0':'x':s ->
      TokenHexInt IntInfo
        { intValue  = number 16 s
        , intDigits = toInteger $ length s
        }
    s ->
      TokenDecInt IntInfo
        { intValue  = number 10 s
        , intDigits = toInteger $ length s
        }
