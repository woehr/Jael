{-# Language NoImplicitPrelude, QuasiQuotes #-}

module Test.Jael.Grammar
( grammarTests
) where

import ClassyPrelude
import Test.Framework as T
import Test.Framework.Providers.HUnit
import Test.HUnit

import Test.Jael.Util
import Jael.Grammar
import Jael.Parser

grammarTests :: [T.Test]
grammarTests = [ testCase "parse plus expr" (shouldParse pGExpr exprPlus)
               , testCase "parse lambda expr" (shouldParse pGExpr exprAbs)
               , testCase "parse func app expr" (shouldParse pGExpr exprApp)
               , testCase "test func app tree" (checkParsedTree pGExpr exprApp gstApp)
               , testCase "parse lambda expr with application" (shouldParse pGExpr exprAbsWithApp)
               , testCase "parse the kitchen sink" (shouldParse pGExpr exprKitchenSink)
               , testCase "test kitchen sink tree" (checkParsedTree pGExpr exprKitchenSink gstKitchenSink)
               ]

exprPlus :: Text
exprPlus = pack [raw|
  2+2
|]

exprAbs :: Text
exprAbs = pack [raw|
  \ a b c -> {
    a + b + c
  }
|]

exprApp :: Text
exprApp = pack [raw|
  a b c
|]

-- Tests that function application is parsed left-associatively
gstApp :: GExpr
gstApp = (GEApp (GEApp (GEVar (LIdent "a"))
                       (GEVar (LIdent "b"))
                )
                (GEVar (LIdent "c"))
         )

exprAbsWithApp :: Text
exprAbsWithApp = pack [raw|
  \ a b c -> {
    a + b + c
  } x y z
|]

-- an expression that tests many components of the defined grammar
exprKitchenSink :: Text
exprKitchenSink = pack [raw|
  if x z{
    y=1+2;
    f = \ a b c -> {
      a=a+b*c;
      (a+b)*c
    } i j k ;
    !(f 1 ~2 y + 3)
  }else {~2 1 a ~2 False}
|]

-- The expected "grammar syntax tree" of ifExpr1
gstKitchenSink :: GExpr
gstKitchenSink = GEIf (GEApp (GEVar (LIdent "x")) (GEVar (LIdent "z"))) (
  GELetExpr [ GELetIdent (LIdent "y") (GEPlus (GEInt (IntTok "1")) (GEInt (IntTok "2")))
            , GELetIdent (LIdent "f") (GEApp (GEApp (GEApp (GEAbs [ GEAbsArg (LIdent "a")
                                                    , GEAbsArg (LIdent "b")
                                                    , GEAbsArg (LIdent "c")
                                                    ]
                                                    ( GELetExpr [ GELetIdent (LIdent "a") (GEPlus (GEVar (LIdent "a")) (GETimes (GEVar (LIdent "b")) (GEVar (LIdent "c"))))
                                                                ]
                                                                (GETimes (GEPlus (GEVar (LIdent "a")) (GEVar (LIdent "b"))) (GEVar (LIdent "c")))
                                                    )
                                             )
                                             (GEVar (LIdent "i"))
                                      )      (GEVar (LIdent "j"))
                                      )      (GEVar (LIdent "k"))
                                      )
            ]
            (GELogNot (GEApp (GEApp (GEApp (GEVar (LIdent "f")) (GEInt (IntTok "1")))
                                                  (GEInt (IntTok "~2")))
                                                  (GEPlus (GEVar (LIdent "y")) (GEInt (IntTok "3"))))
                      )
  ) (
    GELetExpr [] (
      GEApp (GEApp (GEApp (GEApp (GEInt (IntTok "~2")) (GEInt (IntTok "1")))
                          (GEVar (LIdent "a")))
                          (GEInt (IntTok "~2")))
                          GEFalse)
    )

shouldParse :: ParseFun a -> Text -> Assertion
shouldParse p t = either (assertFailure . unpack) (\_ -> return ()) (runParser p t)

checkParsedTree :: (Eq a, Show a) => ParseFun a -> Text -> a -> Assertion
checkParsedTree p tx tr = case runParser p tx of
                               Left e -> assertFailure . unpack $ e
                               Right parsed_tr -> tr @=? parsed_tr

