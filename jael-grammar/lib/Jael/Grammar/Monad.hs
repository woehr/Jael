module Jael.Grammar.Monad where

import Control.Monad.Except (Except, runExcept, throwError)
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.State (StateT, evalStateT, gets, put)
import Data.Word (Word8)

import qualified Data.ByteString.Lazy as BSL
import qualified Streaming.Prelude as S

import Jael.Grammar.Input
import Jael.Grammar.Lexer
import Jael.Grammar.Token

data ParseError = UnicodeDecodeError String
                | LexicalError AlexPosn
                | ErrorMessage String
                deriving (Eq, Show)

type TokenStream = S.Stream (S.Of (DecoratedToken S)) Identity (Maybe ParseError)
type ParseMonad = StateT TokenStream (Except ParseError)
type MyToken = DecoratedToken S

scan :: AlexInput -> TokenStream
scan inp@(pos, prev, dec, i) =
  case alexScan inp 0 of
    AlexSkip inp' _ -> scan inp'
    AlexEOF ->
      case dec of
        ValidUTF8 _ -> decorate TokenEOF pos `S.cons` return Nothing
        InvalidUTF8 n ->
          let i' = BSL.drop (fromIntegral n) i
          in  decorate (TokenBadUTF8 n) pos `S.cons` scan (posnSkip pos n, prev, decodeBS i', i')
    AlexError (pos', _, _, _) -> return $ Just (LexicalError pos')
    AlexToken inp' _ act ->
      let tok = BSL.take (posnDiff inp inp') i
      in  act tok pos `S.cons` scan inp'

getToken :: ParseMonad MyToken
getToken = do
  x <- gets (runIdentity . S.next)
  case x of
    Left (Just r) -> throwError r
    Right (t, s') -> put s' >> return t

lexer :: (MyToken -> ParseMonad a) -> ParseMonad a
lexer = (getToken >>=)

runParseMonad :: BSL.ByteString -> ParseMonad a -> Either ParseError a
runParseMonad bs m = runExcept (evalStateT m (scan (alexInitialInput bs)))

--alexPosition :: Alex AlexPosn
--alexPosition = Alex $ \s@AlexState{alex_pos=pos} -> Right (s, pos)

--parserPosition :: ParseMonad AlexPosn
--parserPosition = lift alexPosition

parserError :: (MyToken, [String]) -> ParseMonad a
parserError (t,es) = throwError $ ErrorMessage (show t <> "\n" <> show es)