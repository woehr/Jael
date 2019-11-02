{
{-# Language DeriveFunctor #-}
{-# Language StandaloneDeriving #-}

module Jael.Grammar.Lexer where

import Jael.Grammar.Input
import Jael.Grammar.Token

import qualified Data.ByteString.Lazy as BSL

}

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
  $white+                       ;
  "//" [.]*                     { mkToken (const TokenComment) }

  -- E notation, size specifiers

  $neg? $digit+                 { mkToken parseInteger }
  "0b" $binDigit [$binDigit _]+ { mkToken parseInteger }
  "0o" $octDigit [$octDigit _]+ { mkToken parseInteger }
  "0x" $hexDigit [$hexDigit _]+ { mkToken parseInteger }
  $lower $ident*                { mkToken lowerIdent }
  $upper $ident*                { mkToken upperIdent }
  $invalid+                     { mkToken (TokenInvalid . fromIntegral . BSL.length) }