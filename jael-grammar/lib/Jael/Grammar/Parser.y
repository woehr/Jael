{
module Jael.Grammar.Parser
  ( pProg
  , pDigit
  )
where

import Jael.Grammar.Error
import Jael.Grammar.Lexer
import Jael.Grammar.Monad
import Jael.Grammar.Token
}

%name pProg foo
%tokentype { AlexToken }
%error { parserError }
%errorhandlertype explist

%monad { ParseMonad }
%lexer { lexer } { EOF }

%token
  alpha   { IgnoreLocation (TokenAlpha $$) }
  decimal { IgnoreLocation (TokenDec $$) }

%%

foo
  : foo decimal { $2 : $1 }
  | decimal { [$1] }

{
--alexGetPosition :: Alex (AlexPosn)
--alexGetPosition = Alex $ \s@AlexState{alex_pos=pos} -> Right (s, pos)
--
--alexShowError :: (Show t, Show t1) => (t, t1, Maybe String) -> Alex a
--alexShowError (line, column, e) = alexError $ "show-error: " ++ (show (line, column, e))
--
-- parseError :: (LocToken, [String]) -> ParseMonad a
-- parseError (t, exps) = do
--   (AlexPn _ line col) <- parserGetPosition
--   error $ show t ++ "\n" ++ show exps
}
