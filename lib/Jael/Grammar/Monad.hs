module Jael.Grammar.Monad where

import           Control.Monad.Except           ( Except
                                                , runExcept
                                                , throwError
                                                )
--import Control.Monad.Identity (Identity, runIdentity)
import           Control.Monad.State            ( StateT
                                                , evalStateT
                                                , gets
                                                , put
                                                )
--import Data.Word (Word8)

import qualified Data.ByteString.Lazy          as BSL
import qualified Streaming                     as S
import qualified Streaming.Prelude             as S

import           Jael.Grammar.Input
import           Jael.Grammar.Lexer
import           Jael.Grammar.Token

data ParseError = UnicodeDecodeError AlexPosn
                | LexicalError AlexPosn
                | ErrorMessage MyToken [String]
                deriving (Eq, Show)

type MyToken = DecoratedToken S
type TokenStream = S.Stream (S.Of MyToken) (Except ParseError) AlexPosn
type ParseMonad = StateT TokenStream (Except ParseError)

scan :: AlexInput -> TokenStream
scan inp@(pos, _, dec, i) = case alexScan inp 0 of
  AlexSkip inp' _ -> scan inp'
  AlexEOF         -> case dec of
    ValidUTF8   _ -> return pos
    InvalidUTF8 _ -> S.effect $ throwError (UnicodeDecodeError pos)
  AlexError (pos', _, _, _) -> S.effect $ throwError (LexicalError pos')
  AlexToken inp' _ act ->
    let tok = BSL.take (posnDiff inp inp') i in act tok pos `S.cons` scan inp'

getToken :: ParseMonad MyToken
getToken = do
  x <- gets (runExcept . S.next)
  case x of
    Left  r               -> throwError r
    Right (Left  pos    ) -> return $ decorate TokenEOF pos
    Right (Right (t, s')) -> put s' >> return t

lexer :: (MyToken -> ParseMonad a) -> ParseMonad a
lexer = (getToken >>=)

runParseMonad :: BSL.ByteString -> ParseMonad a -> Either ParseError a
runParseMonad bs m = runExcept (evalStateT m (scan (alexInitialInput bs)))

parserError :: (MyToken, [String]) -> ParseMonad a
parserError (t, es) = throwError $ ErrorMessage t es
