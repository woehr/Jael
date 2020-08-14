{
{-# Language DeriveFunctor #-}
{-# Language StandaloneDeriving #-}

module Jael.Grammar.Lexer where

import Jael.Grammar.Input
import Jael.Grammar.Token

import qualified Data.ByteString.Lazy as BSL
}

$digit    = [0-9]
$binDigit = [01]
$octDigit = [0-7]
$hexDigit = [0-9a-fA-F]

$upper = [A-Z]
$lower = [a-z]
$alpha = [a-zA-Z]

$alphanum = [$alpha $digit]
$ident = [$alphanum _]

$symChar = [\( \) \[ \] \< \> \{ \} \~ \| \+]

@intBin = "0b" $binDigit+ (_ $binDigit)*
@intOct = "0o" $octDigit+ (_ $octDigit)*
@intHex = "0x" $hexDigit+ (_ $hexDigit)*
@intDec = $digit+         (_ $digit)*

$whiteOrSym = [ $white $symChar ]

$invalid = [^ $white $ident $symChar ]

:-
  $white+                       ;
  "//" [.]*                     { mkToken (TokenComment . toS) }

  "~" { mkToken (TokenSymbol . toS) }
  "|" { mkToken (TokenSymbol . toS) }
  "(" { mkToken (TokenSymbol . toS) }
  ")" { mkToken (TokenSymbol . toS) }
  "[" { mkToken (TokenSymbol . toS) }
  "]" { mkToken (TokenSymbol . toS) }
  "<" { mkToken (TokenSymbol . toS) }
  ">" { mkToken (TokenSymbol . toS) }
  "{" { mkToken (TokenSymbol . toS) }
  "}" { mkToken (TokenSymbol . toS) }
  "(|" { mkToken (TokenSymbol . toS) }
  "|)" { mkToken (TokenSymbol . toS) }
  "[|" { mkToken (TokenSymbol . toS) }
  "|]" { mkToken (TokenSymbol . toS) }
  "{|" { mkToken (TokenSymbol . toS) }
  "|}" { mkToken (TokenSymbol . toS) }
  "<|" { mkToken (TokenSymbol . toS) }
  "|>" { mkToken (TokenSymbol . toS) }
  "+" { mkToken (TokenSymbol . toS) }

  -- E notation, size specifiers
  $whiteOrSym ^ ( @intBin
                | @intOct
                | @intHex
                | @intDec
                ) { mkToken parseInteger }
  $whiteOrSym ^ $lower $ident*                 { mkToken lowerIdent }
  $whiteOrSym ^ $upper $ident*                 { mkToken upperIdent }

  $invalid+                     { mkToken (TokenInvalid . fromIntegral . BSL.length) }
