{-# Language OverloadedStrings #-}

module LexerSpec where

import           Test.Hspec

import qualified Data.ByteString               as BS

import           Jael.Grammar.Input
import           Jael.Grammar.Monad
import           Jael.Grammar.Token

parseToken :: BS.ByteString -> Either ParseError DecoratedToken
parseToken s = case scan . alexInitialInput $ s of
  Left err -> Left err
  Right ((pos, _, rest), t) | BS.null rest -> Right $ decorate t pos
  _        -> error "remaining input"

isSingleTokenLexerError :: ParseError -> Bool
isSingleTokenLexerError (LexicalError _) = True
isSingleTokenLexerError _                = False

singleToken :: BS.ByteString -> PlainToken
singleToken s =
  let x =
        either (error . ("should parse but error:\n" ++) . show) id
          . parseToken
          $ s
  in  case x of
        IgnoreDecorations y -> y
        _                   -> error "Expected IgnoreDecorations"

shouldNotParse :: BS.ByteString -> Expectation
shouldNotParse =
  either (`shouldSatisfy` isSingleTokenLexerError)
         (error . ("should not parse:\n" ++) . show)
    . parseToken

spec :: Spec
spec = do
  describe "tokens" $ do
    context "symbols" $ do
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
      it "add" $ do
        singleToken "+" `shouldBe` TokenSymbol "+"
      it "sub" $ do
        singleToken "-" `shouldBe` TokenSymbol "-"
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
