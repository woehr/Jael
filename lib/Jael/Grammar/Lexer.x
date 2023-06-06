{
{-# Language DeriveFunctor #-}
{-# Language StandaloneDeriving #-}

module Jael.Grammar.Lexer where

import Jael.Grammar.Input
import Jael.Grammar.Token
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

:-
  $white+         ;
  "//" .*         ;

  -- Enclosures
  "(" { tok TokenParenL }
  ")" { tok TokenParenR }
  "[" { tok TokenBracketL }
  "]" { tok TokenBracketR }
  "<" { tok TokenAngleL }
  ">" { tok TokenAngleR }
  "{" { tok TokenBraceL }
  "}" { tok TokenBraceR }
  -- (|   |)

  -- Symbols
  "=" { tok TokenAssign }
  ";" { tok TokenSemi }

  -- Arithmetic
  "~" { tok TokenTilde }
  "+" { tok TokenPlus }
  "-" { tok TokenMinus }
  "*" { tok TokenStar }
  "/" { tok TokenSlash }
  "%" { tok TokenPercent }

  -- Comparison
  "==" { tok TokenEq }
  "!=" { tok TokenNe }
  ">"  { tok TokenGt }
  ">=" { tok TokenGe }
  "<"  { tok TokenLt }
  "<=" { tok TokenLe }

  -- Logic
  "!"   { tok TokenNot}
  "&&"  { tok TokenAnd }
  "||"  { tok TokenOr }
  "==>" { tok TokenImp }
  "<=>" { tok TokenIff }

  -- Integer literals
  -- "0b" $binDigit+ { tok1 TBinInt }
  -- "0o" $octDigit+ { tok1 TOctInt }
  -- "0x" $hexDigit+ { tok1 THexInt }
   $digit+        { tok1 TokenDecInt }

  -- Identifiers
  $lower $ident*  { tok1 TokenLower }
  $upper $ident*  { tok1 TokenUpper }

{

}
