{
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
  intBin  { IgnoreDecorations (TokenBinInt $$) }
  intOct  { IgnoreDecorations (TokenOctInt $$) }
  intHex  { IgnoreDecorations (TokenHexInt $$) }
  intDec  { IgnoreDecorations (TokenDecInt $$) }

  '+'     { IgnoreDecorations (TokenSymbol $$) }

  invalid { IgnoreDecorations (TokenInvalid $$) }

%%

int
  : intBin { $1 }
  | intOct { $1 }
  | intHex { $1 }
  | intDec { $1 }

expr
  : expr '+' expr { JEAdd $1 $3 }
  | int { JEInt $1 }
