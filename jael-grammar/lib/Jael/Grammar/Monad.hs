module Jael.Grammar.Monad where

import Control.Monad (ap)
import Control.Monad.Except (Except, lift, runExceptT, throwError)

import Data.Bifunctor (first, second)
import Data.Functor.Identity (Identity, runIdentity)
import Data.Word (Word8)

import qualified Streaming as S
import qualified Streaming.Prelude as S

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Streaming as BSS

import Jael.Grammar.Lexer
import Jael.Grammar.Token


data ParseError = UnicodeDecodeError String
                | LexicalError (Int, Int)
                | ErrorMessage String
                deriving (Eq, Show)

type ParseMonad = Except ParseError

alexEOF :: AlexPosn -> MyToken
alexEOF p = decorate p BSL.empty TokenEOF

alexStartPos :: AlexPosn
alexStartPos = AlexPn 0 1 1

alexInitialInput :: S.Stream (S.Of Word8) Identity () -> AlexInput
alexInitialInput x = (alexStartPos, '\n', x)

-- alexScan :: AlexInput -> StartCode -> AlexReturn a

-- T.decodeUtf8With :: OnDecodeError -> ByteString -> Text
-- type OnDecodeError = OnError Word8 Char
-- type OnError a b = String -> Maybe a -> Maybe b

decodeUTF8Bytestring :: BSS.ByteString        Identity ()
                     -> S.Stream (S.Of Word8) Identity ()
decodeUTF8Bytestring = undefined

--let inp = alexInitialInput (decodeUTF8Bytestring bs)

packWord8Stream :: S.Stream (S.Of Word8) Identity () -> BSL.ByteString
packWord8Stream s =
  let bs S.:> () = runIdentity (BSS.toLazy (BSS.pack s))
  in  bs

scan :: AlexInput -> S.Stream (S.Of MyToken) Identity (Maybe ParseError)
scan inp@(pos, _, s) =
  case alexScan inp 0 of
    AlexSkip inp' _ -> scan inp'
    AlexEOF         -> S.yields (alexEOF pos S.:> Nothing)
    AlexError ((AlexPn _ l c), _, _) -> pure $ Just (LexicalError (l, c))
    AlexToken inp' l act ->
      act pos (packWord8Stream (S.take l s)) `S.cons` scan inp'

--  go ::  AlexInput -> S.ByteString Identity ParseError
--  go x@(pos, _, str) =
--    case alexScan x 0 of
--      AlexEOF                             -> Free $ Compose (alexEOF pos, Nothing)
--      AlexError ((AlexPn _ l c), _, _) -> Pure (LexicalError (l, c))
--      AlexSkip  x' _                      -> go x'
--      AlexToken x'@(_,_,_) _ act ->
--        Free $ Compose (act pos (BS.take (n'-n) str), Just (go x'))

getToken :: ParseMonad MyToken
getToken = undefined

lexer :: (MyToken -> ParseMonad a) -> ParseMonad a
lexer = (getToken >>=)

runParseMonad :: BSL.ByteString -> ParseMonad a -> Either ParseError a
runParseMonad bs m = undefined -- either (Left . parseAlexError) id (runAlex bs (runExceptT m))

--alexPosition :: Alex AlexPosn
--alexPosition = Alex $ \s@AlexState{alex_pos=pos} -> Right (s, pos)

--parserPosition :: ParseMonad AlexPosn
--parserPosition = lift alexPosition

parserError :: (MyToken, [String]) -> ParseMonad a
parserError (t,es) = throwError $ ErrorMessage (show t <> "\n" <> show es)