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
grammarTests = [ testCase "plus function parsing" (shouldParse pGExpr expr1)
               , testCase "kitchen sink parsing" (shouldParse pGExpr kitchenSinkExpr1)
               , testCase "kitchen sink tree" (checkParsedTree pGExpr kitchenSinkExpr1 kitchenSinkGST)
               ]

expr1 :: Text
expr1 = pack [raw|
  2+2
|]

-- an expression that tests many components of the defined grammar
-- function application binds highest ()
-- ~ is not a prefix operator but part of a negative integer literal
--   thus ~2(...) should be an application to ~2 not ~ application to 2.
kitchenSinkExpr1 :: Text
kitchenSinkExpr1 = pack [raw|
  if x{
    y=1+2;
    f = \ a, b, c -> {
      a=a+b*c;
      (a+b)*c
    }(i,j , k ) ;
    !f(1, ~2, y+3)
  }else {~2(1,a,~2,False)}
|]

-- The expected "grammar syntax tree" of ifExpr1
kitchenSinkGST :: GExpr
kitchenSinkGST = GEIf (GEVar (LIdent "x")) (
  GELetExpr [ GELetIdent (LIdent "y") (GEPlus (GEInt 1) (GEInt 2))
            , GELetIdent (LIdent "f") (GEApp (GEAbs [ GEAbsArg (LIdent "a")
                                                    , GEAbsArg (LIdent "b")
                                                    , GEAbsArg (LIdent "c")
                                                    ]
                                                    ( GELetExpr [ GELetIdent (LIdent "a") (GEPlus (GEVar (LIdent "a")) (GETimes (GEVar (LIdent "b")) (GEVar (LIdent "c"))))
                                                                ]
                                                                (GETimes (GEPlus (GEVar (LIdent "a")) (GEVar (LIdent "b"))) (GEVar (LIdent "c")))
                                                    )
                                             )
                                             [ GEAppArg (GEVar (LIdent "i"))
                                             , GEAppArg (GEVar (LIdent "j"))
                                             , GEAppArg (GEVar (LIdent "k"))
                                             ]
                                      )
            ]
            (GELogNot (GEApp (GEVar (LIdent "f")) [ GEAppArg (GEInt 1)
                                                  , GEAppArg (GEIntNeg 2)
                                                  , GEAppArg (GEPlus (GEVar (LIdent "y")) (GEInt 3))
                                                  ]
                      )
            )
  ) (
    GELetExpr [] (
      GEApp (GEIntNeg 2) [ GEAppArg (GEInt 1)
                         , GEAppArg (GEVar (LIdent "a"))
                         , GEAppArg (GEIntNeg 2)
                         , GEAppArg GEFalse
                         ]
    )
  )

shouldParse :: ParseFun a -> Text -> Assertion
shouldParse p t = either (assertFailure . unpack) (\_ -> return ()) (runParser p t)

checkParsedTree :: (Eq a, Show a) => ParseFun a -> Text -> a -> Assertion
checkParsedTree p tx tr = case runParser p tx of
                               Left e -> assertFailure . unpack $ e
                               Right parsed_tr -> tr @=? parsed_tr

