{-# Language OverloadedStrings #-}

module LexerSpec where

import           Test.Hspec

import           Control.Monad.Except           ( runExcept )
--import Data.Bifunctor (second)

import qualified Data.ByteString.Lazy          as BSL
import qualified Streaming.Prelude             as S

import           Jael.Grammar.Monad
import           Jael.Grammar.Input
import           Jael.Grammar.Token

parseToken :: BSL.ByteString -> Either ParseError (DecoratedToken S)
parseToken s = case runExcept . S.next . scan . alexInitialInput $ s of
  Left  err             -> Left err
  Right (Left  _      ) -> error "Parsed EOF instead of a token."
  Right (Right (t, ts)) -> case runExcept . S.next $ ts of
    Left  err      -> Left err
    Right (Left _) -> Right t -- Token followed by EOF
    Right (Right (t', _)) ->
      error
        $  "Parsed:\n\t"
        ++ show t
        ++ "\n\nfollowed by another token:\n\t"
        ++ show t'
        ++ "\n"

isSingleTokenLexerError :: ParseError -> Bool
isSingleTokenLexerError (LexicalError _) = True
isSingleTokenLexerError _                = False

isInvalidToken :: Int -> PlainToken S -> Bool
isInvalidToken x (TokenInvalid y) | x == y = True
isInvalidToken _ _                         = False

singleToken :: BSL.ByteString -> PlainToken S
singleToken s =
  let x =
          either (error . ("should parse but error:\n" ++) . show) id
            . parseToken
            $ s
  in  case x of
        IgnoreDecorations y -> y
        _                   -> error "Expected IgnoreDecorations"

shouldNotParse :: BSL.ByteString -> Expectation
shouldNotParse =
  either (`shouldSatisfy` isSingleTokenLexerError)
         (error . ("should not parse:\n" ++) . show)
    . parseToken

spec :: Spec
spec = do
  describe "tokens" $ do
    context "symbols" $ do
      it "invalid symbol" $ do
        singleToken "`" `shouldSatisfy` isInvalidToken 1
      it "parens" $ do
        singleToken "(" `shouldBe` TokenSymbol "("
        singleToken ")" `shouldBe` TokenSymbol ")"
      it "brackets" $ do
        singleToken "[" `shouldBe` TokenSymbol "["
        singleToken "]" `shouldBe` TokenSymbol "]"
      it "angles" $ do
        singleToken "<" `shouldBe` TokenSymbol "<"
        singleToken ">" `shouldBe` TokenSymbol ">"
      it "braces" $ do
        singleToken "{" `shouldBe` TokenSymbol "{"
        singleToken "}" `shouldBe` TokenSymbol "}"
      it "bananas" $ do
        singleToken "(|" `shouldBe` TokenSymbol "(|"
        singleToken "|)" `shouldBe` TokenSymbol "|)"
      it "tilde" $ do
        singleToken "~" `shouldBe` TokenSymbol "~"
    context "identifiers" $ do
      context "invalid" $ do
        it "underscore-upper" $ do
          shouldNotParse "_A"
      context "valid" $ do
        it "lowercase" $ do
          singleToken "a" `shouldBe` TokenLower "a"
        it "lowercase-underscore" $ do
          singleToken "a_" `shouldBe` TokenLower "a_"
        it "uppercase" $ do
          singleToken "A" `shouldBe` TokenUpper "A"
    context "integers" $ do
      context "invalid" $ do
        it "incomplete-binary" $ do
          shouldNotParse "0b"
        it "incomplete-octal" $ do
          shouldNotParse "0o"
        it "incomplete-hexadecimal" $ do
          shouldNotParse "0x"
      context "valid" $ do
        it "binary" $ do
          singleToken "0b0"
            `shouldBe` TokenBinInt (IntInfo { intValue = 0, intDigits = 1 })
        it "octal" $ do
          singleToken "0o0"
            `shouldBe` TokenOctInt (IntInfo { intValue = 0, intDigits = 1 })
        it "hexadecimal" $ do
          singleToken "0x0"
            `shouldBe` TokenHexInt (IntInfo { intValue = 0, intDigits = 1 })
        it "zero" $ do
          singleToken "0"
            `shouldBe` TokenDecInt (IntInfo { intValue = 0, intDigits = 1 })
        it "positive" $ do
          singleToken "1"
            `shouldBe` TokenDecInt (IntInfo { intValue = 1, intDigits = 1 })
