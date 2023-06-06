module Jael.Grammar.Monad where

--import Data.Word (Word8)
import           Control.Monad.Except           ( Except
                                                , runExcept
                                                , throwError
                                                )
--import Control.Monad.Identity (Identity, runIdentity)
import           Control.Monad.State            ( StateT
                                                , evalStateT
                                                , get
                                                , put
                                                )

import qualified Data.ByteString               as BS

import           Jael.Grammar.Input
import           Jael.Grammar.Lexer
import           Jael.Grammar.Token

data ParseError = LexicalError AlexPosn
                | ParserMessage DecoratedToken [String]
                deriving (Eq, Show)

type ParseMonad a = StateT AlexInput (Except ParseError) a

scan :: AlexInput -> Either ParseError (AlexInput, PlainToken)
scan inp@(pos, _, i) = case alexScan inp 0 of
  AlexSkip inp' _        -> scan inp'
  AlexEOF                -> Right (inp, TokenEOF)
  AlexError (pos', _, _) -> Left (LexicalError pos')
  AlexToken inp'@(pos', _, _) _ act ->
    let n = posnDiff pos pos' in Right (inp', act (BS.take n i))

getToken :: ParseMonad DecoratedToken
getToken = do
  inp <- get
  case scan inp of
    Left  err                     -> throwError err
    Right (inp'@(pos, _, _), t) -> put inp' >> return (decorate t pos)

lexer :: (DecoratedToken -> ParseMonad a) -> ParseMonad a
lexer = (getToken >>=)

runParseMonad :: BS.ByteString -> ParseMonad a -> Either ParseError a
runParseMonad bs m = runExcept (evalStateT m (alexInitialInput bs))

parserError :: (DecoratedToken, [String]) -> ParseMonad a
parserError (t, es) = throwError $ ParserMessage t es
