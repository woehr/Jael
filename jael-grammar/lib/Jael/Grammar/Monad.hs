module Jael.Grammar.Monad where

import Data.Bifunctor (first, second)
import Control.Monad (ap)
import Control.Monad.Except (ExceptT, lift, runExceptT, throwError)
import Text.Scanf ((:+)(..), fmt, scanf)

import qualified Data.ByteString.Lazy as BS

import Jael.Grammar.Error
import Jael.Grammar.Lexer
import Jael.Grammar.Located

type ParseMonad = ExceptT (Located ParseError) Alex

tokenScan :: ParseMonad AlexToken
tokenScan = lift alexMonadScan

lexer :: (AlexToken -> ParseMonad a) -> ParseMonad a
lexer = (tokenScan >>=)

runParseMonad :: BS.ByteString -> ParseMonad a -> Either (Located ParseError) a
runParseMonad bs m = either (Left . parseAlexError) id (runAlex bs (runExceptT m))

alexPosition :: Alex AlexPosn
alexPosition = Alex $ \s@AlexState{alex_pos=pos} -> Right (s, pos)

parserPosition :: ParseMonad AlexPosn
parserPosition = lift alexPosition

parserError :: (AlexToken, [String]) -> ParseMonad a
parserError x = throwError $ Located Nothing Nothing (ErrorMessage (show x))
