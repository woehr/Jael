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
grammarTests = [ testCase "plus expr" (checkParsedTree pGExpr exprPlus gstPlus)
               , testCase "plus expr" (checkParsedTree pGExpr exprTimes gstTimes)
               , testCase "operator precedence" (checkParsedTree pGExpr exprOpPrec gstOpPrec)
               , testCase "lambda expr" (checkParsedTree pGExpr exprAbs gstAbs)
               , testCase "double application" (checkParsedTree pGExpr exprApp gstApp)
               , testCase "application with operator" (checkParsedTree pGExpr exprAppWithOp gstAppWithOp)
               , testCase "application precedence (1)" (checkParsedTree pGExpr exprAppPrec1 gstAppPrec1)
               , testCase "application precedence (2)" (checkParsedTree pGExpr exprAppPrec2 gstAppPrec2)
               , testCase "lambda expr with application" (checkParsedTree pGExpr exprAbsWithApp gstAbsWithApp)
               , testCase "if expr with application" (checkParsedTree pGExpr exprIfWithApp gstIfWithApp)
               , testCase "if expr with application w/o paren" (shouldNotParse pGExpr exprIfWithAppFail)
               ]

shouldNotParse :: ParseFun a -> Text -> Assertion
shouldNotParse p t = either (\_ -> return ()) (\_ -> assertFailure "Expression parsed successful") (runParser p t)

checkParsedTree :: (Eq a, Show a) => ParseFun a -> Text -> a -> Assertion
checkParsedTree p tx tr = either (assertFailure . unpack) ((@=?) tr) (runParser p tx)

-- 1
exprPlus :: Text
exprPlus = pack [raw|
  a+b+c
|]

gstPlus :: GExpr
gstPlus = (GEPlus (GEPlus (GEVar (LIdent "a"))
                          (GEVar (LIdent "b"))
                  )
                  (GEVar (LIdent "c"))
          )

-- 2
exprTimes :: Text
exprTimes = pack [raw|
  a*b*c
|]

gstTimes :: GExpr
gstTimes = (GETimes (GETimes (GEVar (LIdent "a"))
                             (GEVar (LIdent "b"))
                    )
                    (GEVar (LIdent "c"))
           )

-- 3
exprOpPrec :: Text
exprOpPrec = pack [raw|
  !a+!b*!c // (!a)+((!b)*(!c))
|]

gstOpPrec :: GExpr
gstOpPrec = (GEPlus (GELogNot (GEVar (LIdent "a")))
                    (GETimes (GELogNot (GEVar (LIdent "b")))
                             (GELogNot (GEVar (LIdent "c")))
                    )
            )

-- 4
exprAbs :: Text
exprAbs = pack [raw|
  \ a b c -> {
    a
  }
|]

gstAbs :: GExpr
gstAbs = (GEAbs [ GEAbsArg (LIdent "a")
                , GEAbsArg (LIdent "b")
                , GEAbsArg (LIdent "c")
                ]
                (GELetExpr [] (GEVar (LIdent "a"))
                )
         )

-- 5
-- Apply b to a, then apply c to the result, application binds stronger than !
exprApp :: Text
exprApp = pack [raw|
  !a(b)(c)
|]

gstApp :: GExpr
gstApp = (GELogNot (GEApp (GEApp (GEVar (LIdent "a"))
                                 [GEAppArg (GEVar (LIdent "b"))]
                          )
                          [GEAppArg (GEVar (LIdent "c"))]
                   )
         )

-- 6
exprAppWithOp :: Text
exprAppWithOp = pack [raw|
  f (1 + 2)
|]

gstAppWithOp :: GExpr
gstAppWithOp = (GEApp (GEVar (LIdent "f"))
                      [GEAppArg (GEPlus (GEInt (IntTok "1"))
                                        (GEInt (IntTok "2"))
                                )
                      ]
                )

-- 7
exprAppPrec1 :: Text
exprAppPrec1 = pack [raw|
  a+f(b)
|]

gstAppPrec1 :: GExpr
gstAppPrec1 = (GEPlus (GEVar (LIdent "a"))
                      (GEApp (GEVar (LIdent "f"))
                             [GEAppArg (GEVar (LIdent "b"))]
                      )
              )

-- 8
exprAppPrec2 :: Text
exprAppPrec2 = pack [raw|
  f(b)*a
|]

gstAppPrec2 :: GExpr
gstAppPrec2 = (GETimes (GEApp (GEVar (LIdent "f"))
                              [GEAppArg (GEVar (LIdent "b"))]
                       )
                       (GEVar (LIdent "a"))
              )

-- 9
exprAbsWithApp :: Text
exprAbsWithApp = pack [raw|
  \ a b c -> {
    a
  }(x, y, z)
|]

gstAbsWithApp :: GExpr
gstAbsWithApp = (GEApp (GEAbs [ GEAbsArg (LIdent "a")
                              , GEAbsArg (LIdent "b")
                              , GEAbsArg (LIdent "c")
                              ]
                              (GELetExpr [] (GEVar (LIdent "a"))
                              )
                       )
                       [ GEAppArg (GEVar (LIdent "x"))
                       , GEAppArg (GEVar (LIdent "y"))
                       , GEAppArg (GEVar (LIdent "z"))
                       ]
                )

-- 10
exprIfWithApp :: Text
exprIfWithApp = pack [raw|
  (if True {a} else {b})(c)
|]

gstIfWithApp :: GExpr
gstIfWithApp = (GEApp (GEIf (GETrue)
                            (GELetExpr [] (GEVar (LIdent "a")))
                            (GELetExpr [] (GEVar (LIdent "b")))
                      )
                      [GEAppArg (GEVar (LIdent "c"))]
               )

-- 11
-- This looks confusing so the grammar is defined such that parenthesis are
-- required
exprIfWithAppFail :: Text
exprIfWithAppFail = pack [raw|
  if True {a} else {b}(c)
|]

