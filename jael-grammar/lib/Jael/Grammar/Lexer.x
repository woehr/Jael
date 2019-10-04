{
{-# Language DeriveFunctor #-}

module Jael.Grammar.Lexer where

import qualified Data.ByteString.Lazy as BS
import qualified Data.Text.Lazy as T

import Jael.Grammar.Token
}

%wrapper "posn-bytestring"

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

$invalid = [^ $ident $syms $white]

:-
  $white+ ;
  "//" [.]*                     { mkToken (const TokenComment) }

  -- E notation, size specifiers

  $neg? $digit+                 { mkToken parseInteger }
  "0b" $binDigit [$binDigit _]+ { mkToken parseInteger }
  "0o" $octDigit [$octDigit _]+ { mkToken parseInteger }
  "0x" $hexDigit [$hexDigit _]+ { mkToken parseInteger }

  $lower $ident*                { mkToken lowerIdent }
  $upper $ident*                { mkToken upperIdent }

  $invalid+                     { mkToken (const TokenInvalid) }

{
type AlexBS = BS.ByteString
type MyToken = DecoratedToken AlexBS

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

--alexEOF :: Alex MyToken
--alexEOF = do
--  (psn, _, _, _) <- alexGetInput
--  return $ decorate psn BS.empty TokenEOF

type AlexUserState = ()
alexInitUserState = ()
}