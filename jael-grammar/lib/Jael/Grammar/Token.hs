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

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as C8
import qualified Data.Row.Internal as RI
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T

--import Jael.Types.Expr ( IntFormat(..), JInt(..) )

data IntInfo = IntInfo
  { intValue :: Integer
  , intDigits :: Integer
  } deriving (Eq, Show)

data PlainToken
  = TokenBinInt IntInfo
  | TokenOctInt IntInfo
  | TokenDecInt IntInfo
  | TokenHexInt IntInfo
  | TokenLower T.Text
  | TokenUpper T.Text
  | TokenComment
  | TokenInvalid
  | TokenEOF
  deriving (Eq, Show)

data DecoratedToken a = DecoratedToken
  { tokenOffset :: Maybe Int
  , tokenPosn   :: Maybe (Int, Int)
  , tokenString :: a
  , tokenPlain  :: PlainToken
  }
  deriving (Eq, Show)

pattern IgnoreDecorations :: PlainToken -> DecoratedToken a
pattern IgnoreDecorations x <- DecoratedToken { tokenPlain = x }

lowerIdent, upperIdent :: BS.ByteString -> PlainToken
lowerIdent = TokenLower . T.decodeUtf8
upperIdent = TokenUpper . T.decodeUtf8

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

parseInteger :: BS.ByteString -> PlainToken
parseInteger n =
  case filter (/= '_') $ C8.unpack n of
    s -> TokenBinInt IntInfo
          { intValue  = number 10 s
          , intDigits = toInteger $ length s
          }
