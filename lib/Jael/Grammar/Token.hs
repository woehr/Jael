{-# Language DataKinds #-}
{-# Language DeriveGeneric #-}
{-# Language MultiWayIf #-}
{-# Language OverloadedStrings #-}
{-# Language PatternSynonyms #-}
{-# Language TypeFamilies #-}
--{-# Language TypeOperators #-}
{-# Language UndecidableInstances #-}

module Jael.Grammar.Token where

import Data.Char (ord)
import Data.Foldable (foldlM)
import Data.Maybe (fromMaybe)

--import Data.OpenADT (OpenADT)
--import Data.Row (type (.==), type (.+), Row)
--import Data.Text.Short.Unsafe (fromByteStringUnsafe)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as C8
import qualified Data.Set as S

--import qualified Data.Row.Internal as RI
--import qualified Data.Text.Short as T
import GHC.Generics (Generic)
import Data.TreeDiff (ToExpr)

res :: S.Set S
res = S.fromList ["if", "then", "else"]

-- S for string type
type S = Text -- T.ShortText
--instance Generic T.ShortText
--instance ToExpr T.ShortText

toS :: BSL.ByteString -> S
toS = decodeUtf8 . BSL.toStrict
--toS = fromByteStringUnsafe . BSL.toStrict

data IntInfo = IntInfo
  { intValue :: Integer
  , intDigits :: Integer
  } deriving (Eq, Generic, Show)
instance ToExpr IntInfo

data PlainToken a
  = TokenComment a
  | TokenSymbol a
  | TokenReserved a
  | TokenBinInt IntInfo
  | TokenOctInt IntInfo
  | TokenDecInt IntInfo
  | TokenHexInt IntInfo
  | TokenLower a
  | TokenUpper a
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
lowerIdent bs =
  let s = toS bs 
  in if s `S.member` res then TokenReserved s else TokenLower s
upperIdent =  TokenUpper . toS
  

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
    $ foldlM (\x d -> (base * x +) <$> digitToInt d) 0 xs

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
