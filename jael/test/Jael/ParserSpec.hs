{-# Language NoImplicitPrelude
, OverloadedStrings #-}

module Jael.ParserSpec (spec) where

import BasePrelude
import Test.Hspec

import qualified Data.Text as T
import qualified Data.Functor.Foldable as F
import qualified Jael.Grammar as G
import Jael.Expr
import Jael.Parser
import Jael.Util

parseExpr :: T.Text -> G.Expr
parseExpr t = case runParser G.pExpr t of
                Left err -> error (T.unpack err)
                Right expr -> expr

annotateUntyped :: Expr -> MaybeTypedExpr
annotateUntyped = F.cata alg
  where 
    alg (EAppF e a)     = AnnExpr (Ann Nothing $ EAppF e a)
    alg (EAbsF n e)     = AnnExpr (Ann Nothing $ EAbsF n e)
    alg (ELetF n e1 e2) = AnnExpr (Ann Nothing $ ELetF n e1 e2)
    alg (EIteF b t e)   = AnnExpr (Ann Nothing $ EIteF b t e)
    alg (EVarF n)       = AnnExpr (Ann Nothing $ EVarF n)
    alg (EConF c)       = AnnExpr (Ann Nothing $ EConF c)

spec :: Spec
spec = do
  describe "jaelify" $ do
    describe "should return a Token for identifiers" $ do
      it "is implemented for G.LIdent" $ do
        jaelify (G.LIdent ((4,5), "ident")) `shouldBe` Token "ident" (4,5)

    describe "should return a Token for integer constants" $ do
      it "is implemented for G.DecInt" $ do
        jaelify (G.DecInt ((4,5), "10")) `shouldBe` Token 10 (4,5)

  describe "expressions" $ do
    describe "associativity" $ do
      it "should bind * and / more strongly than + and -" $ do
        parseExpr "1*2*3+1/2-~1" `shouldBe`
          (G.EMinus (G.EPlus (G.ETimes (G.ETimes (G.EInt (G.DecInt ((1,1), "1")))
                                                 (G.EInt (G.DecInt ((1,3), "2")))
                                       )
                                       (G.EInt (G.DecInt ((1,5), "3")))
                             )
                             (G.EDiv   (G.EInt (G.DecInt ((1,7), "1")))
                                       (G.EInt (G.DecInt ((1,9), "2")))
                             )
                    )
                    (G.EInt (G.DecInt ((1,11), "~1")))
          )

  describe "integer parsers" $ do
    it "should parse a decimal" $ do
      parseDecInt "5" `shouldBe` 5
    it "should parse a negative decimal" $ do
      parseDecInt "~5" `shouldBe` (-5)
    it "should parse hexidecimal in any case" $ do
      parseHexInt "0xFf" `shouldBe` 255
    it "should parse octal" $ do
      parseOctInt "0o377" `shouldBe` 255
    it "should parse binary" $ do
      parseBinInt "0b11111111" `shouldBe` 255
    describe "cases which should fail because the grammar definition won't tokenize as such" $ do
      it "hex ints must be prefixed with '0x'" $ do
        evaluate (parseHexInt "01ff") `shouldThrow` errorCall "impossible"

  describe "jaelify transforms the grammar AST to a Jael Expr" $ do
    it "transforms variables and constants" $ do
      (jaelify . parseExpr) "5" `shouldBe` annotateUntyped (ECon $ CInt (Token 5 (1,1)))

