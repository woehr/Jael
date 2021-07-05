{
{-# Language OverloadedStrings #-}
{-# Language PatternSynonyms #-}

module Jael.Grammar.Parser
  ( pProg
  )
where

import Jael.Grammar.Monad
import Jael.Grammar.Token
import Jael.Grammar.AST
}

%name pProg expr
%tokentype { MyToken }
%error { parserError }
%errorhandlertype explist

%monad { ParseMonad }
%lexer { lexer } { IgnoreDecorations TokenEOF }

%token
  lident  { IgnoreDecorations (TokenLower $$) }
  uident  { IgnoreDecorations (TokenUpper $$) }

  intBin  { IgnoreDecorations (TokenBinInt $$) }
  intOct  { IgnoreDecorations (TokenOctInt $$) }
  intHex  { IgnoreDecorations (TokenHexInt $$) }
  intDec  { IgnoreDecorations (TokenDecInt $$) }

  -- Reserved words
  'if'   { Reserved "if" }
  'then' { Reserved "then" }
  'else' { Reserved "else" }

  -- Enclosures
  '{' { Symbol "{" }
  '}' { Symbol "}" }

  -- Other symbols
  '=' { Symbol "=" }
  ';' { Symbol ";" }

  -- Operators
  '==>'   { Symbol "==>" }
  '<=>'   { Symbol "<=>" }

  '&&'    { Symbol "&&" }
  '||'    { Symbol "||" }

  '*'     { Symbol "*" }
  '/'     { Symbol "/" }
  '%'     { Symbol "%" }

  '+'     { Symbol "+" }
  '-'     { Symbol "-" }

%right '<=>'
%right '==>'

%right '||'
%right '&&'

%nonassoc '==' '!=' '>' '>=' '<' '<='

%left '+' '-'
%left '*' '/' '%'

%%

int
  : intBin { $1 }
  | intOct { $1 }
  | intHex { $1 }
  | intDec { $1 }

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

  | int { JEInt $1 }

  | lident { JELIdent $1 }
  | uident { JEUIdent $1 }

{

pattern Reserved :: S -> DecoratedToken S
pattern Reserved x <- IgnoreDecorations (TokenReserved x)

pattern Symbol :: S -> DecoratedToken S
pattern Symbol x <- IgnoreDecorations (TokenSymbol x)

}