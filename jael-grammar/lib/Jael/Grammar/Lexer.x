{
{-# Language DeriveFunctor #-}
{-# Language StandaloneDeriving #-}

module Jael.Grammar.Lexer where

import Data.Int (Int64)
import Data.Functor.Identity (Identity, runIdentity)
import Data.Word (Word8)

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Internal as BS (w2c)
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import qualified Data.Text.Lazy as T

import qualified Streaming.Prelude as S

import Jael.Grammar.Token
}

$replacement = [\xfffd]

$digit = [0-9]
$binDigit = [01]
$octDigit = [0-7]
$hexDigit = [0-9a-fA-F]

$upper = [A-Z]
$lower = [a-z]
$alpha = [a-zA-Z]

$alphanum = [$alpha $digit]
$ident = [$alphanum _]

$parL = [\(]
$parR = [\)]
$braL = [\[]
$braR = [\]]
$angL = [\<]
$angR = [\>]

$neg = [\~]

$syms = [
  -- Various enclosures
  $parL $parR $braL $braR $angL $angR
  -- Operators
  $neg
  ]

$invalid = [^ $replacement $ident $syms $white]

:-
  $white+                       ;
  "//" [.]*                     { mkToken (const TokenComment) }

  -- E notation, size specifiers

  $neg? $digit+                 { mkToken parseInteger }
  "0b" $binDigit [$binDigit _]+ { mkToken parseInteger }
  "0o" $octDigit [$octDigit _]+ { mkToken parseInteger }
  "0x" $hexDigit [$hexDigit _]+ { mkToken parseInteger }
  $lower $ident*                { mkToken lowerIdent }
  $upper $ident*                { mkToken upperIdent }
  $invalid+                     { mkToken (const TokenInvalid) }
  $replacement+                 { mkToken (TokenBadUTF8 . toInteger . BS.length) }

{
type AlexBS = BS.ByteString
type MyToken = DecoratedToken AlexBS

-- A position has an address (the number of chacaters preceding the token),
-- a line number and a column number.
data AlexPosn = AlexPn !Int !Int !Int
  deriving (Eq, Show)

alexMove :: AlexPosn -> Char -> AlexPosn
alexMove (AlexPn a l _) '\n' = AlexPn (a+1) (l+1)   1
alexMove (AlexPn a l c) _    = AlexPn (a+1)  l     (c+1)

type AlexInput = ( AlexPosn -- current position
                 , Char     -- previous char
                 , S.Stream (S.Of Word8) Identity ()
                 )

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (_, c, _) = c

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte (p, _, s) =
  case runIdentity (S.next s) of
    Left () -> Nothing
    Right (b, s') ->
      let c   = BS.w2c b
          p'  = alexMove p c
      in p' `seq` Just (b, (p', c, s'))

decorate :: AlexPosn -> a -> PlainToken -> DecoratedToken a
decorate (AlexPn b r c) s t = DecoratedToken
  { tokenOffset = Just b
  , tokenPosn   = Just (r, c)
  , tokenString = s
  , tokenPlain  = t
  }

--mkToken :: (AlexBS -> PlainToken) -> AlexAction MyToken
--mkToken f (psn, _, bs, _) len =
--  let s = BS.take len bs
--  in return $ decorate psn s (f s)

mkToken :: (AlexBS -> PlainToken) -> AlexPosn -> BS.ByteString -> MyToken
mkToken f psn bs = decorate psn bs (f bs)
}