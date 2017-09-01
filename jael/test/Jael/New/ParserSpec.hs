{-# Language OverloadedStrings #-}

module Jael.New.ParserSpec (spec) where

import Test.Hspec

import Text.Trifecta
import Text.Trifecta.Delta

import Jael.New.Expr
import Jael.New.Types
import Jael.New.QType
import Jael.New.Parser

iterCofree :: Functor f => (a -> f b -> b) -> Cofree f a -> b
iterCofree f cfa = uncofree f cfa
  where uncofree fn x = fn (extract x) $ fmap (uncofree fn) $ unwrap x

removeAnn :: Functor f => Cofree f a -> Fix f
removeAnn = iterCofree (\_ f -> Fix f)

parseThrow :: Parser a -> String -> a
parseThrow p t = case parseString (p <* eof) (Directed "" 0 0 0 0) t of
                   Failure e -> error . show . _errDoc $ e
                   Success x -> x

parseScheme :: String -> Type
parseScheme = removeAnn . parseThrow pType0

parseType :: String -> Type
parseType = removeAnn . parseThrow pType1

parseQType :: String -> QType Expr
parseQType = (fmap (fmap (fmap removeAnn))) . parseThrow pType1

parseData :: String -> DataDecl Expr
parseData = fmap removeAnn . parseThrow pData

parseBitRep :: String -> BitRep
parseBitRep = parseThrow pBitRep

parsePattern :: String -> Pattern
parsePattern = parseThrow pPattern0

parseExpr :: String -> Expr
parseExpr = removeAnn . parseThrow pExpr0

spec :: Spec
spec = do
  describe "..." $ do
    it "should type (var)" $ do
      parseType "a" `shouldBe` (TVar "a")
    it "should type (record)" $ do
      parseType "{foo: a, bar: Int}" `shouldBe` TRec [("foo", TVar "a"), ("bar", TCon "Int" [])]
    it "should scheme" $ do
      parseScheme "forall a. a" `shouldBe` (TAll ["a"] $ TVar "a")
    it "should pattern" $ do
      parsePattern "con(a, b)" `shouldBe` PPat "con" [PPat "a" [], PPat "b" []]
    it "should pattern (tuple)" $ do
      parsePattern "(a, b)" `shouldBe` PTup [PPat "a" [], PPat "b" []]
    it "should pattern (rec)" $ do
      parsePattern "{x=1, y=_, z=z}" `shouldBe`
        PRec [("x", PConst (CInt $ JInt DecInt 1 1)), ("y", PWild), ("z", PPat "z" [])]
    it "should pattern (arr)" $ do
      parsePattern "[1, x, ...]" `shouldBe` PArr [PConst (CInt $ JInt DecInt 1 1), PPat "x" [], PMultiWild]
    it "should pattern (or)" $ do
      parsePattern "con(1|x|_)" `shouldBe` PPat "con" [POr [PConst $ (CInt $ JInt DecInt 1 1), PPat "x" [], PWild]]
    it "should expr (abs)" $ do
      parseExpr "\\(x) -> true" `shouldBe` EAbs [AbsBind (PPat "x" []) Nothing] (EVar "true")
    it "should expr (lamcase)" $ do
      parseExpr "\\case { c -> x; c(1) -> x }" `shouldBe`
        ELamCase [ CaseAlt (PPat "c" []) Nothing (EVar "x")
                 , CaseAlt (PPat "c" [PConst $ CInt $ JInt DecInt 1 1]) Nothing (EVar "x")
                 ]
    it "should expr (rec)" $ do
      parseExpr "{}" `shouldBe` ERec []
      parseExpr "{x=5, y=x}" `shouldBe` ERec [("x", EConst $ CInt $ JInt DecInt 5 1), ("y", EVar "x")]
    it "should expr (rec ops)" $ do
      parseExpr "x.1.a.b" `shouldBe` ERecSel (ERecSel (ERecSel (EVar "x") "1") "a") "b"
      parseExpr "{x=1,y=z|r}" `shouldBe` ERecUp [("x", EConst $ CInt $ JInt DecInt 1 1), ("y", EVar "z")] (EVar "r")
      parseExpr "{}--1--a" `shouldBe` ERecRes (ERecRes (ERec []) "1") "a"
    it "should expr (let with patterns)" $ do
      parseExpr "{x|_=1;x}"   `shouldBe` ELet [ LetBind (POr [ PPat "x" []
                                                             , PWild
                                                             ])
                                                        Nothing
                                                        (EConst $ CInt $ JInt DecInt 1 1)
                                               ] (EVar "x")
      parseExpr "{0b1|02=x;x}" `shouldBe` ELet [ LetBind (POr [ PConst (CInt $ JInt BinInt 1 1)
                                                              , PConst (CInt $ JInt DecInt 2 2)
                                                              ])
                                                         Nothing
                                                         (EVar "x")] (EVar "x")
      parseExpr "{{y=1, z=z} = x; z}" `shouldBe`
        ELet [ LetBind (PRec [ ("y", PConst $ CInt $ JInt DecInt 1 1)
                             , ("z", PPat "z" [])])
                       Nothing (EVar "x")
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
                         [ CaseAlt (PPat "p" []) Nothing (EVar "a")
                         , CaseAlt (PPat "p" [ PPat "p" []]) Nothing (EVar "b")
                         , CaseAlt (PTup [PPat "x" [], PPat "y" []]) Nothing (EVar "c")
                         , CaseAlt PWild Nothing (EVar "d")
                         ]

    -- Sums of products
    -- Introduces the type Foo(a) and the values foo1:Foo, foo2:a->Int->{x:a,y:Int}->Foo,
    -- and the data constructors foo1, foo2 to the environment
    it "should data" $ do
      parseData "data Foo(a) { foo1 \
                \            ; foo2(a, Int, {x:a, y:Int}) \
                \            }" `shouldBe` DataDecl "Foo" ["a"]
                                             [ DataCon "foo1" []
                                             , DataCon "foo2"
                                                 [ UQVar "a"
                                                 , UQCon "Int" []
                                                 , UQRec [ ("x", UQVar "a")
                                                         , ("y", UQCon "Int" [])
                                                         ]
                                                 ]
                                             ]

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
