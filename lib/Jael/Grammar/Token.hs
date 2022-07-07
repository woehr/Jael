{-# Language DataKinds #-}
{-# Language DeriveGeneric #-}
{-# Language MultiWayIf #-}
{-# Language OverloadedStrings #-}
{-# Language PatternSynonyms #-}
{-# Language TypeFamilies #-}
{-# Language UndecidableInstances #-}

module Jael.Grammar.Token where

import           Data.Char                      ( ord )
import           Data.Foldable                  ( foldlM )
import           Data.Maybe                     ( fromMaybe )

import qualified Data.ByteString               as BS
import qualified Data.ByteString.Char8         as C8
import qualified Data.Set                      as S
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T

import           Data.TreeDiff                  ( ToExpr )
import           GHC.Generics                   ( Generic )

res :: S.Set T.Text
res = S.fromList ["if", "then", "else"]

data IntInfo = IntInfo
  { intValue  :: Integer
  , intDigits :: Integer
  }
  deriving (Eq, Generic, Show)
instance ToExpr IntInfo

data PlainToken
  = TokenComment T.Text
  | TokenSymbol BS.ByteString
  | TokenReserved T.Text
  | TokenBinInt IntInfo
  | TokenOctInt IntInfo
  | TokenDecInt IntInfo
  | TokenHexInt IntInfo
  | TokenLower T.Text
  | TokenUpper T.Text
  | TokenEOF
  deriving (Eq, Show)

data DecoratedToken = DecoratedToken
  { tokenOffset :: Int
  , tokenPosn   :: (Int, Int)
  , tokenPlain  :: PlainToken
  }
  deriving (Eq, Show)

pattern IgnoreDecorations :: PlainToken -> DecoratedToken
pattern IgnoreDecorations x <- DecoratedToken { tokenPlain = x }

lowerIdent, upperIdent :: BS.ByteString -> PlainToken
lowerIdent bs =
  let s = T.decodeUtf8 bs
  in  if s `S.member` res then TokenReserved s else TokenLower s
upperIdent = TokenUpper . T.decodeUtf8

-- Total, integer version.
-- https://hackage.haskell.org/package/base-4.11.1.0/docs/src/Data.Char.html#digitToInt
digitToInt :: Char -> Maybe Integer
digitToInt c = toInteger <$> if
  | (fromIntegral dec :: Word) <= 9  -> Just dec
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
    $ foldlM (\x d -> (base * x +) <$> digitToInt d) 0 xs

parseInteger :: BS.ByteString -> PlainToken
parseInteger n = case filter (/= '_') $ C8.unpack n of
  '~' : s -> TokenDecInt IntInfo { intValue  = -(number 10 s)
                                 , intDigits = toInteger $ length s
                                 }
  '0' : 'b' : s -> TokenBinInt IntInfo { intValue  = number 2 s
                                       , intDigits = toInteger $ length s
                                       }
  '0' : 'o' : s -> TokenOctInt IntInfo { intValue  = number 8 s
                                       , intDigits = toInteger $ length s
                                       }
  '0' : 'x' : s -> TokenHexInt IntInfo { intValue  = number 16 s
                                       , intDigits = toInteger $ length s
                                       }
  s -> TokenDecInt IntInfo { intValue  = number 10 s
                           , intDigits = toInteger $ length s
                           }
