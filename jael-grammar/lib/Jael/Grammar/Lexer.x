{
{-# Language DeriveFunctor #-}

module Jael.Grammar.Lexer where

import qualified Data.ByteString.Lazy as BS
import qualified Data.Text.Lazy as T

import Jael.Grammar.Error
import Jael.Grammar.Located
import Jael.Grammar.Token
}

%wrapper "monadUserState-bytestring"

$binDigit = [01]
$octDigit = [0-7]
$decDigit = [0-9]
$hexDigit = [0-9a-fA-F]

$alpha = [a-zA-Z]

:-
  $white+     ;
  "//" [.]*   ;
  $decDigit+    { mkToken TokenDec }
  $alpha+       { mkToken TokenAlpha }

{
type AlexBS = BS.ByteString
type AlexToken = Located (Token AlexBS)

mkToken :: (AlexBS -> Token AlexBS) -> AlexAction AlexToken
mkToken tokCon (psn, _, bs, _) len = return $ alexPosnToLocated psn (tokCon (BS.take len bs))

alexPosnToLocated :: AlexPosn -> a -> Located a
alexPosnToLocated (AlexPn b r c) x = Located (Just b) (Just (r,c)) x

alexEOF :: Alex (Located (Token a))
alexEOF = do
  (psn, _, _, _) <- alexGetInput
  return $ alexPosnToLocated psn TokenEOF

type AlexUserState = ()
alexInitUserState = ()
}
