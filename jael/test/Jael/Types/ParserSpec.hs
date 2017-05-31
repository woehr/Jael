{-# Language OverloadedStrings #-}

module Jael.Types.ParserSpec (spec) where

import Test.Hspec

import qualified Data.Text as T
import qualified Jael.Grammar as G
import qualified Language.Fixpoint.Types as F

import Jael.Types
import Jael.Util

parseThrow :: ParseFun a -> T.Text -> a
parseThrow p t = case runParser p t of
                   Left err -> error (T.unpack err)
                   Right x -> x

parseExpr :: T.Text -> G.Expr
parseExpr = parseThrow G.pExpr

parseType :: T.Text -> G.Type
parseType = parseThrow G.pType

--parseProg :: T.Text -> G.Prog
--parseProg = parseThrow G.pProg

annotateUntyped :: Expr -> MaybeTypedExpr
annotateUntyped = cata (\e -> [] :< e)

--applyArgs :: Expr -> [Expr] -> Expr
--applyArgs x as = foldl' (\a v -> Fix $ EAppF v a) x as

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
          (G.EMinus (G.EPlus (G.ETimes (G.ETimes (G.EInt (G.AnyIntDecInt $ G.DecInt ((1,1), "1")))
                                                 (G.EInt (G.AnyIntDecInt $ G.DecInt ((1,3), "2")))
                                       )
                                       (G.EInt (G.AnyIntDecInt $ G.DecInt ((1,5), "3")))
                             )
                             (G.EDiv   (G.EInt (G.AnyIntDecInt $ G.DecInt ((1,7), "1")))
                                       (G.EInt (G.AnyIntDecInt $ G.DecInt ((1,9), "2")))
                             )
                    )
                    (G.EInt (G.AnyIntDecInt $ G.DecInt ((1,11), "~1")))
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

  describe "jaelify transforms the grammar to something useful" $ do
    it "transforms variables and constants" $ do
      (jaelify . parseExpr) "5" `shouldBe` annotateUntyped (Fix $ EConF $ CInt (Token 5 (1,1)))
    it "transforms exprs" $ do
      (jaelify . parseExpr) "x+y" `shouldBe` annotateUntyped
        (Fix $ EAppF (Fix $ EAppF (Fix $ EConF $ CAdd)
                                  (Fix $ EVarF $ Token "x" (1,1)))
                     (Fix $ EVarF $ Token "y" (1,3))
        )
    it "transforms types" $ do
      (jaelify . parseType) "A({x:Int|x>0})" `shouldBe`
        (Nothing :<
           TConF (Token "A" (1,1))
                   [Just (F.reft (F.symbol ("x"::T.Text)) (F.PAtom F.Gt (F.EVar "x") (F.ECon (F.I 0))))
                    :< TIntF
                   ]
        )

