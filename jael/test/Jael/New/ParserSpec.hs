{-# Language LambdaCase #-}
{-# Language OverloadedStrings #-}
{-# Language RankNTypes #-}

module Jael.New.ParserSpec (spec) where

import Test.Hspec

import qualified Data.Map as M

import Jael.Test.Util
import Jael.New.Expr
import Jael.New.DataDecl
import Jael.New.Type
import Jael.New.QType
import Jael.New.Parser
import Jael.New.Misc

parseBitRep :: String -> BitRep Pattern
parseBitRep = removePatternParseInfo . parseThrow pBitRep

removePatternParseInfo :: BitRep P -> BitRep Pattern
removePatternParseInfo (BitRep x y z alts) = BitRep x y z (map f alts) where
  f (BitCase p cs) = BitCase (removeAnn p) cs

parsePattern' :: String -> Pattern
parsePattern' = removeAnn . parsePattern

spec :: Spec
spec = do
  describe "..." $ do
    it "should type (var)" $ do
      parseType "a" `shouldBe` (TVar "a")
    it "should type (record)" $ do
      parseType "{foo: a, bar: Int}" `shouldBe` TRec [("foo", TVar "a"), ("bar", TCon "Int" [])]
    it "should scheme" $ do
      parseType "forall a. a" `shouldBe` (TAll ["a"] $ TVar "a")
    it "should pattern" $ do
      parsePattern' "con(a, b)" `shouldBe` PPat "con" [PPat "a" [], PPat "b" []]
    it "should pattern (tuple)" $ do
      parsePattern' "(a, b)" `shouldBe` PTup [PPat "a" [], PPat "b" []]
    it "should pattern (rec)" $ do
      parsePattern' "{x=1, y=_, z=z}" `shouldBe`
        PRec [("x", PConst (CInt $ JInt DecInt 1 1)), ("y", PWild), ("z", PPat "z" [])]
    it "should pattern (arr)" $ do
      parsePattern' "[1, x, ...]" `shouldBe` PArr [PConst (CInt $ JInt DecInt 1 1), PPat "x" [], PMultiWild]
    it "should pattern (or)" $ do
      parsePattern' "con(1|x|_)" `shouldBe` PPat "con" [POr [PConst $ (CInt $ JInt DecInt 1 1), PPat "x" [], PWild]]
    it "should expr (abs)" $ do
      parseExpr "\\(x) -> true" `shouldBe` EAbs [PPat "x" []] [] (EVar "true")
    it "should expr (lamcase)" $ do
      parseExpr "\\case { c -> x; c(1) -> x }" `shouldBe`
        ELamCase [ (PPat "c" [], EVar "x")
                 , (PPat "c" [PConst $ CInt $ JInt DecInt 1 1], EVar "x")
                 ]
    it "should expr (rec)" $ do
      parseExpr "{}" `shouldBe` ERec []
      parseExpr "{x=5, y=x}" `shouldBe` ERec [("x", EConst $ CInt $ JInt DecInt 5 1), ("y", EVar "x")]
    it "should expr (rec ops)" $ do
      parseExpr "x.1.a.b" `shouldBe` ERecSel (ERecSel (ERecSel (EVar "x") "1") "a") "b"
      parseExpr "{x=1,y=z|r}" `shouldBe` ERecUp [("x", EConst $ CInt $ JInt DecInt 1 1), ("y", EVar "z")] (EVar "r")
      parseExpr "{}--1--a" `shouldBe` ERecRes (ERecRes (ERec []) "1") "a"
    it "should expr (let with patterns)" $ do
      parseExpr "{x|_=1;x}"   `shouldBe`
        ELet [ ( POr [ PPat "x" []
                     , PWild
                     ]
                , EConst $ CInt $ JInt DecInt 1 1)
             ]
             (EVar "x")
      parseExpr "{0b1|02=x;x}" `shouldBe`
        ELet [ ( POr [ PConst (CInt $ JInt BinInt 1 1)
                     , PConst (CInt $ JInt DecInt 2 2)
                     ]
               , EVar "x")
             ]
             (EVar "x")
      parseExpr "{{y=1, z=z} = x; z}" `shouldBe`
        ELet [ ( PRec [ ("y", PConst $ CInt $ JInt DecInt 1 1)
                      , ("z", PPat "z" [])
                      ]
               , EVar "x")
             ]
             (EVar "z")
    it "should expr (if, multiIf)" $ do
      parseExpr "if x then y else z" `shouldBe` EIf (EVar "x") (EVar "y") (EVar "z")
      parseExpr "if | x then y | else x" `shouldBe` EMultiIf [Guarded (EVar "x") (EVar "y")]
                                                             (Just $ EVar "x")
    it "should expr (case with patterns)" $ do
      parseExpr "case x of {p -> a; p(p) -> b; (x,y) -> c; _ -> d}"
        `shouldBe` ECase (EVar "x")
                         -- Whether the first case is a PPat or a PBind depends
                         -- what data constructors are defined and must be
                         -- resolved in a separate step
                         [ (PPat "p" [], EVar "a")
                         , (PPat "p" [ PPat "p" []], EVar "b")
                         , (PTup [PPat "x" [], PPat "y" []], EVar "c")
                         , (PWild, EVar "d")
                         ]

    -- Sums of products
    -- Introduces the type Foo(a) and the values foo1:Foo, foo2:a->Int->{x:a,y:Int}->Foo,
    -- and the data constructors foo1, foo2 to the environment
    it "should data" $ do
      parseData "data Foo(a) { foo1 \
                \            ; foo2(a, Int, {x:a, y:Int}) \
                \            }" `shouldBe` (DataDecl "Foo" ["a"] $ M.fromList
                                             [ ("foo1", [])
                                             , ("foo2",
                                                 [ UQVar "a"
                                                 , UQCon "Int" []
                                                 , UQRec [ ("x", UQVar "a")
                                                         , ("y", UQCon "Int" [])
                                                         ]
                                                 ]
                                               )
                                             ]
                                           )

    it "should bitrep" $ do
      parseBitRep "bitrep Foo(a) <5b> { foo1 = 0b011#_; foo2(x, {l1=y}) = 1<3b>#x#y<3B> }"
        `shouldBe` BitRep "Foo"
                          ["a"]
                          (Just $ BitSize 5)
                          [ BitCase (PPat "foo1" [])
                                    [ (BitConInt $ JInt BinInt 3 3, Nothing)
                                    , (BitConWild, Nothing)]
                          , BitCase (PPat "foo2" [PPat "x" [], PRec [("l1", PPat "y" [])]])
                            [ (BitConInt $ JInt DecInt 1 1, Just $ BitSize 3)
                                    , (BitConIdent "x", Nothing)
                                    , (BitConIdent "y", Just $ ByteSize 3)
                                    ]
                          ]

{-
    bitrep Foo(a) <4B+sizeof(a)> = { foo1(x, y) = 1#x#_#y, foo3({x, y}) = y#x#0 }

    data Tup3(a, b, c) = tup3(a, b, c)
    bitrep Tup3(a, b, c) <sizeof(a)+sizeof(b)+sizeof(c)> = { (a, b, c) = a#b#c }

    data X(t0, t1, ...) = c({l0:t0, l1:t1, ...})
    bitrep X(t0, t1, ...)  <sizeof(t0)+sizeof(t1)+...> = { c({l0=b0, l1=b1, ...}) = b0#b1#... }
-}
