{-# Language QuasiQuotes #-}

module Jael.Grammar.Monad where

import Data.Bifunctor (first, second)
import Control.Monad (ap)
--import Control.Monad.Except (ExceptT, lift, runExceptT, throwError)
import Control.Monad.Except (Except, lift, runExceptT, throwError)
import Text.Scanf ((:+)(..), fmt, scanf)

import qualified Data.ByteString.Lazy as BS

import Jael.Grammar.Lexer
import Jael.Grammar.Token

data ParseError = UnicodeDecodeError String
                | LexicalError (Int, Int)
                | ErrorMessage String
                deriving (Eq, Show)

parseAlexError :: String -> ParseError
parseAlexError s
  | Just (r :+ c :+ ()) <- scanf [fmt|lexical error at line %d, column %d|] s
  = LexicalError (r, c)
  | otherwise = ErrorMessage s

--type ParseMonad = ExceptT ParseError Alex
type ParseMonad = Except ParseError

-- alexMonadScan = do
--   inp__@(_,_,_,n) <- alexGetInput
--   sc <- alexGetStartCode
--   case alexScan inp__ sc of
--     AlexEOF -> alexEOF
--     AlexError ((AlexPn _ line column),_,_,_) -> alexError $ "lexical error at line " ++ (show line) ++ ", column " ++ (show column)
--     AlexSkip  inp__' _len -> do
--         alexSetInput inp__'
--         alexMonadScan
--     AlexToken inp__'@(_,_,_,n') _ action -> do
--         alexSetInput inp__'
--         action (ignorePendingBytes inp__) len
--       where
--         len = n'-n

--tokenScan :: ParseMonad AlexToken
--tokenScan = lift alexMonadScan

--lexer :: (AlexToken -> ParseMonad a) -> ParseMonad a
--lexer = (tokenScan >>=)

--runParseMonad :: BS.ByteString -> ParseMonad a -> Either ParseError a
--runParseMonad bs m = either (Left . parseAlexError) id (runAlex bs (runExceptT m))

--alexPosition :: Alex AlexPosn
--alexPosition = Alex $ \s@AlexState{alex_pos=pos} -> Right (s, pos)

--parserPosition :: ParseMonad AlexPosn
--parserPosition = lift alexPosition

--parserError :: (AlexToken, [String]) -> ParseMonad a
--parserError x = throwError $ ErrorMessage (show x)

-- posn-bytestring lexer

lexer :: (MyToken -> ParseMonad a) -> ParseMonad a
lexer = (scan >>=)

--alexScanTokens :: ByteString.ByteString -> [token]
-- alexScanTokens str0 = go (alexStartPos,'\n',str0,0)
--   where go inp__@(pos,_,str,n) =
--           case alexScan inp__ 0 of
--                 AlexEOF -> []
--                 AlexError ((AlexPn _ line column),_,_,_) -> error $ "lexical error at line " ++ (show line) ++ ", column " ++ (show column)
--                 AlexSkip  inp__' _len       -> go inp__'
--                 AlexToken inp__'@(_,_,_,n') _ act ->
--                   act pos (ByteString.take (n'-n) str) : go inp__'

scan :: BS.ByteString -> [MyToken]
scan str0 = go (alexStartPos,'\n',str0,0)
  where go inp__@(pos,_,str,n) =
          case alexScan inp__ 0 of
                AlexEOF -> []
                AlexError ((AlexPn _ line column),_,_,_) -> error $ "lexical error at line " ++ (show line) ++ ", column " ++ (show column)
                AlexSkip  inp__' _len       -> go inp__'
                AlexToken inp__'@(_,_,_,n') _ act ->
                  act pos (BS.take (n'-n) str) : go inp__'