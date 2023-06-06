{
{-# Language OverloadedStrings #-}
{-# Language PatternSynonyms #-}

module Jael.Grammar.Parser
  ( pProg
  )
where

import qualified Data.Text as T
import qualified Data.ByteString as BS

import Jael.Grammar.Monad
import Jael.Grammar.Token
import Jael.Grammar.AST
}

%name pProg expr
%tokentype { DecoratedToken }
%error { parserError }
%errorhandlertype explist

%monad { ParseMonad }
%lexer { lexer } { IgnoreDecorations TokenEOF }

%token

  --intBin  { IgnoreDecorations (TokenBinInt $$) }
  --intOct  { IgnoreDecorations (TokenOctInt $$) }
  --intHex  { IgnoreDecorations (TokenHexInt $$) }
  intDec  { IgnoreDecorations (TokenDecInt $$) }

  -- Reserved words
  'if'   { IgnoreDecorations (TokenLower "if") }
  'then' { IgnoreDecorations (TokenLower "then") }
  'else' { IgnoreDecorations (TokenLower "else") }

  lident  { IgnoreDecorations (TokenLower $$) }
  uident  { IgnoreDecorations (TokenUpper $$) }

  -- Enclosures
  '{' { IgnoreDecorations TokenBraceL }
  '}' { IgnoreDecorations TokenBraceR }

  -- Other symbols
  '=' { IgnoreDecorations TokenAssign }
  ';' { IgnoreDecorations TokenSemi }

  -- Operators
  '==>'   { IgnoreDecorations TokenImp }
  '<=>'   { IgnoreDecorations TokenIff }
  '&&'    { IgnoreDecorations TokenAnd }
  '||'    { IgnoreDecorations TokenOr }
  '*'     { IgnoreDecorations TokenStar }
  '/'     { IgnoreDecorations TokenSlash }
  '%'     { IgnoreDecorations TokenPercent }
  '+'     { IgnoreDecorations TokenPlus }
  '-'     { IgnoreDecorations TokenMinus }

%right '<=>'
%right '==>'

%right '||'
%right '&&'

%nonassoc '==' '!=' '>' '>=' '<' '<='

%left '+' '-'
%left '*' '/' '%'

%%

int
  : intDec { $1 }
  -- | intBin { $1 }
  -- | intOct { $1 }
  -- | intHex { $1 }

letBind : lident '=' expr ';' { ($1, $3) }
letBinds
  : letBind          { [$1] }
  | letBinds letBind { $2 : $1 }

expr
  : 'if' expr 'then' expr 'else' expr { JEIf $2 $4 $6 }
  | '{' letBinds expr '}' { JELet (reverse $2) $3 }

  | expr '+' expr { JEAdd $1 $3 }
  | expr '-' expr { JESub $1 $3 }

  | expr '*' expr { JEMul $1 $3 }
  | expr '/' expr { JEDiv $1 $3 }
  | expr '%' expr { JEMod $1 $3 }

  | expr '&&' expr { JEAnd $1 $3 }
  | expr '||' expr { JEOr $1 $3 }

  | expr '==>' expr { JEImp $1 $3 }
  | expr '<=>' expr { JEIff $1 $3 }

  | int { JEIntLit $1 }

  | lident { JELIdent $1 }
  | uident { JEUIdent $1 }

{

--pattern Reserved :: T.Text -> DecoratedToken
--pattern Reserved x <- IgnoreDecorations (TokenReserved x)
--
--pattern Symbol :: BS.ByteString -> DecoratedToken
--pattern Symbol x <- IgnoreDecorations (TokenSymbol x)

}