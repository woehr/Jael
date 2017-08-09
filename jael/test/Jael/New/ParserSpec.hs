{-# Language OverloadedStrings #-}

module Jael.New.ParserSpec (spec) where

import Test.Hspec

import Text.Trifecta
import Text.Trifecta.Delta

import Jael.New.Expr
import Jael.New.Types
import Jael.New.Parser

parseThrow :: Parser a -> String -> a
parseThrow p t = case parseString (p <* eof) (Directed "" 0 0 0 0) t of
                   Failure e -> error . show . _errDoc $ e
                   Success x -> x

parseScheme :: String -> Type
parseScheme = parseThrow pScheme

parseType :: String -> Type
parseType = parseThrow pType0

parseData :: String -> DataDecl
parseData = parseThrow pData

parseEnum :: String -> EnumDecl
parseEnum = parseThrow pEnum

parseStruct :: String -> StructDecl
parseStruct = parseThrow pStruct

parsePattern :: String -> Pattern
parsePattern = parseThrow pPattern

parseExpr :: String -> E
parseExpr = parseThrow pExpr0

spec :: Spec
spec = do
  describe "..." $ do
    it "should type (var)" $ do
      parseType "a" `shouldBe` (TVar "a")
    it "should type (record)" $ do
      parseType "{foo: a, bar: Int}" `shouldBe` TRec [("foo", TVar "a"), ("bar", TCon (Just "Int") [])]
    it "should scheme" $ do
      parseScheme "forall a. a" `shouldBe` (TAll ["a"] $ TVar "a")
    it "should pattern" $ do
      parsePattern "con(a, b)" `shouldBe` PPat "con" [PPat "a" [], PPat "b" []]
    it "should pattern (tuple)" $ do
      parsePattern "(a, b)" `shouldBe` PPat "" [PPat "a" [], PPat "b" []]
    it "should pattern (rec)" $ do
      parsePattern "{x=1, y=_, z=z}" `shouldBe`
        PRec [("x", PConst (CInt 1)), ("y", PWild), ("z", PPat "z" [])]
    it "should pattern (arr)" $ do
      parsePattern "[1, x, ...]" `shouldBe` PArr [PConst (CInt 1), PPat "x" [], PMultiWild]
    it "should expr (abs)" $ do
      parseExpr "\\(x) -> true" `shouldBe` EAbs "x" Nothing (EVar "true")
    it "should expr (rec)" $ do
      parseExpr "{x=5, y=x}" `shouldBe` ERec [("x", EConst $ CInt 5), ("y", EVar "x")]
      parseExpr "{}" `shouldBe` ERec []
    it "should expr (let with patterns)" $ do
      parseExpr "{x=1;x}" `shouldBe` ELet [(PPat "x" [], EConst (CInt 1))] (EVar "x")
      parseExpr "{1=x;x}" `shouldBe` ELet [(PConst (CInt 1), EVar "x")] (EVar "x")
      parseExpr "{{y=1, z=z} = x; z}" `shouldBe`
        ELet [( PRec [ ("y", PConst $ CInt 1)
                     , ("z", PPat "z" [])]
             ,  EVar "x")
             ]
             (EVar "z")
    it "should expr (case with patterns)" $ do
      parseExpr "case x of {p -> a; p(p) -> b; (x,y) -> c; _ -> d}"
        `shouldBe` ECase (EVar "x")
                         -- Whether the first case is a PPat or a PBind depends
                         -- what data constructors are defined and must be
                         -- resolved in a separate step
                         [ (PPat "p" []
                           , EVar "a")
                         , (PPat "p" [ PPat "p" []]
                           , EVar "b")
                         , (PPat ""  [ PPat "x" []
                                     , PPat "y" []
                                        ]
                           , EVar "c")
                         , (PWild
                           , EVar "d")
                         ]

    -- Sums of products
    -- Introduces the type Foo(a) and the functions foo1:Int->a->Foo, foo2:Int->Foo,
    -- and the data constructors foo1, foo2 to the environment
    it "should data" $ do
      parseData "data Foo(a) = foo1(Int, a)\
                \            | foo2(Int)" `shouldBe` DataDecl []

    -- Sum of Ints
    -- Introduces Foo as an alias to { v:Int | v==4 || v=5 } to the environment
    it "should enum" $ do
      parseEnum   "enum   Foo = { bar = 0b100, quux = 5 }" `shouldBe` EnumDecl []

    -- Records with layout
    -- Introduces Foo as an alias to { bar:X, quux:Int } to the environment
    it "should struct" $ do
      parseStruct "struct Foo = { bar : X, quux : Int }" `shouldBe` StructDecl []
