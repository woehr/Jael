{
{-# Language DeriveFunctor #-}

module Jael.Grammar.Lexer where

import qualified Data.ByteString.Lazy as BS
import qualified Data.Text.Lazy as T

import Jael.Grammar.Error
import Jael.Grammar.Located
import Jael.Grammar.Token
}

%wrapper "monadUserState-bytestring"

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

$syms = [$parL $parR $braL $braR $angL $angR]

$invalid = [^ $ident $syms]

:-
  $white+     ;
  "//" [.]*       { mkToken TokenComment }
  -- E notation, size specifiers
  $digit+      { mkToken (TokenInt IntDecimal) }

  "0b" $binDigit+ { mkToken (TokenInt IntBinary) }
  "0o" $octDigit+ { mkToken (TokenInt IntOctal) }
  "0x" $hexDigit+ { mkToken (TokenInt IntHexidecimal) }

  $invalid+       { mkToken TokenInvalid }

{
type AlexBS = BS.ByteString
type AlexToken = Located (Token AlexBS)

mkToken :: (AlexBS -> Token AlexBS) -> AlexAction AlexToken
mkToken tokCon (psn, _, bs, _) len = return $ alexPosnToLocated psn (tokCon (BS.take len bs))

alexPosnToLocated :: AlexPosn -> a -> Located a
alexPosnToLocated (AlexPn b r c) x = Located (Just b) (Just (r,c)) x

alexEOF :: Alex (Located (Token a))
alexEOF = do
  (psn, _, _, _) <- alexGetInput
  return $ alexPosnToLocated psn TokenEOF

type AlexUserState = ()
alexInitUserState = ()
}
