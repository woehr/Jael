{-# Language NoImplicitPrelude, QuasiQuotes #-}

module Test.Jael.Grammar.Expr
( gExprTests
) where

import ClassyPrelude
import qualified Test.Framework as T
import Test.Framework.Providers.HUnit

import Test.Jael.Util
import Jael.Grammar

gExprTests :: [T.Test]
gExprTests = [ testCase "int zero" (checkParsedTree pGExpr intZero)
             , testCase "int neg" (checkParsedTree pGExpr intNeg)
             , testCase "int pos" (checkParsedTree pGExpr intPos)
             , testCase "int too many zero" (shouldNotParse pGExpr intTooManyZero)
             , testCase "int leading zeros pos" (shouldNotParse pGExpr intLeadingZeroPos)
             , testCase "int leading zeros neg" (shouldNotParse pGExpr intLeadingZeroNeg)
             , testCase "int neg zero" (shouldNotParse pGExpr intNegZero)
             , testCase "plus expr" (checkParsedTree pGExpr plus)
             , testCase "plus expr" (checkParsedTree pGExpr times)
             , testCase "operator precedence" (checkParsedTree pGExpr opPrec)
             , testCase "lambda expr" (checkParsedTree pGExpr abstr)
             , testCase "double application" (checkParsedTree pGExpr app)
             , testCase "application with operator" (checkParsedTree pGExpr appWithOp)
             , testCase "application precedence (1)" (checkParsedTree pGExpr appPrec1)
             , testCase "application precedence (2)" (checkParsedTree pGExpr appPrec2)
             , testCase "lambda expr with application" (checkParsedTree pGExpr absWithApp)
             , testCase "if expr with application" (checkParsedTree pGExpr ifWithApp)
             , testCase "if expr with application w/o paren" (shouldNotParse pGExpr ifWithAppFail)
             , testCase "tuple expr" (checkParsedTree pGExpr tup)
             , testCase "tuple in abs" (checkParsedTree pGExpr tupInAbs)
             , testCase "index accessor" (checkParsedTree pGExpr accIndex)
             , testCase "label accessor" (checkParsedTree pGExpr accLabel)
             , testCase "multi accessors" (checkParsedTree pGExpr accMulti)
             , testCase "accessor, invalid ident" (shouldNotParse pGExpr accInvalidIdent)
             ]

intZero :: (Text, GExpr)
intZero = (pack [raw|
  0
|], GEInt (IntTok "0")
  )

intNeg :: (Text, GExpr)
intNeg = (pack [raw|
  ~123
|], GEInt (IntTok "~123")
  )

intPos :: (Text, GExpr)
intPos = (pack [raw|
  123
|], GEInt (IntTok "123")
  )

intTooManyZero :: Text
intTooManyZero = pack [raw|
  000
|]

intLeadingZeroPos :: Text
intLeadingZeroPos = pack [raw|
  0123
|]

intLeadingZeroNeg  :: Text
intLeadingZeroNeg = pack [raw|
  ~0123
|]

intNegZero :: Text
intNegZero = pack [raw|
  ~0
|]

plus :: (Text, GExpr)
plus = (pack [raw|
  a+b+c
|], GEPlus (GEPlus (GEVar (LIdent "a"))
                    (GEVar (LIdent "b"))
            )
            (GEVar (LIdent "c"))
  )

times :: (Text, GExpr)
times = (pack [raw|
  a*b*c
|], GETimes (GETimes (GEVar (LIdent "a"))
                      (GEVar (LIdent "b"))
             )
             (GEVar (LIdent "c"))
  )

opPrec :: (Text, GExpr)
opPrec = (pack [raw|
  !a+!b*!c // (!a)+((!b)*(!c))
|], GEPlus (GELogNot (GEVar (LIdent "a")))
            (GETimes (GELogNot (GEVar (LIdent "b")))
                     (GELogNot (GEVar (LIdent "c")))
            )
  )

abstr :: (Text, GExpr)
abstr = (pack [raw|
  \ a b c -> {
    a
  }
|], GEAbs [ GEAbsArg (LIdent "a")
           , GEAbsArg (LIdent "b")
           , GEAbsArg (LIdent "c")
           ]
           (GELetExpr [] (GEVar (LIdent "a")))
  )

-- Apply b to a, then apply c to the result, application binds stronger than !
app :: (Text, GExpr)
app = (pack [raw|
  !a(b)(c)
|], GELogNot (GEApp (GEApp (GEVar (LIdent "a"))
                            [GEAppArg (GEVar (LIdent "b"))]
                     )
                     [GEAppArg (GEVar (LIdent "c"))]
              )
  )

appWithOp :: (Text, GExpr)
appWithOp = (pack [raw|
  f (1 + 2)
|], GEApp (GEVar (LIdent "f"))
          [GEAppArg (GEPlus (GEInt (IntTok "1"))
                            (GEInt (IntTok "2"))
                    )
          ]
  )

appPrec1 :: (Text, GExpr)
appPrec1 = (pack [raw|
  a+f(b)
|], GEPlus (GEVar (LIdent "a"))
           (GEApp (GEVar (LIdent "f"))
                  [GEAppArg (GEVar (LIdent "b"))]
           )
  )

appPrec2 :: (Text, GExpr)
appPrec2 = (pack [raw|
  f(b)*a
|], GETimes (GEApp (GEVar (LIdent "f"))
                   [GEAppArg (GEVar (LIdent "b"))]
            )
            (GEVar (LIdent "a"))
  )

absWithApp :: (Text, GExpr)
absWithApp = (pack [raw|
  \ a b c -> {
    a
  }(x, y, z)
|], GEApp (GEAbs [ GEAbsArg (LIdent "a")
                 , GEAbsArg (LIdent "b")
                 , GEAbsArg (LIdent "c")
                 ]
                 (GELetExpr [] (GEVar (LIdent "a")))
          )
          [ GEAppArg (GEVar (LIdent "x"))
          , GEAppArg (GEVar (LIdent "y"))
          , GEAppArg (GEVar (LIdent "z"))
          ]
  )

ifWithApp :: (Text, GExpr)
ifWithApp = (pack [raw|
  (if true {a} else {b})(c)
|], GEApp (GEIf GETrue
                (GELetExpr [] (GEVar (LIdent "a")))
                (GELetExpr [] (GEVar (LIdent "b")))
          )
          [GEAppArg (GEVar (LIdent "c"))]
  )

-- This looks confusing so the grammar is defined such that parenthesis are
-- required
ifWithAppFail :: Text
ifWithAppFail = pack [raw|
  if true {a} else {b}(c)
|]

tup :: (Text, GExpr)
tup = (pack [raw|
  { 1
  , true
  , {}
  , 42
  }
|], GETup $ map GETupArg [ GEInt (IntTok "1")
                         , GETrue
                         , GEUnit GUnit
                         , GEInt (IntTok "42")
                         ]
  )

tupInAbs :: (Text, GExpr)
tupInAbs = (pack [raw|
  \a b c d -> {
    { 1
    , 2
    , a
    , a+b
    }
  }
|], GEAbs [ GEAbsArg (LIdent "a")
          , GEAbsArg (LIdent "b")
          , GEAbsArg (LIdent "c")
          , GEAbsArg (LIdent "d")
          ]
          (GELetExpr []
                     (GETup $ map GETupArg [ GEInt (IntTok "1")
                                           , GEInt (IntTok "2")
                                           , GEVar (LIdent "a")
                                           , GEPlus (GEVar (LIdent "a"))
                                                    (GEVar (LIdent "b"))
                                           ]
                     )
          )
  )

accIndex :: (Text, GExpr)
accIndex = (pack [raw|
  x :: 0
|], GEIdx (GEVar (LIdent "x")) (GEInt (IntTok "0"))
  )

accLabel :: (Text, GExpr)
accLabel = (pack [raw|
  x :: abc
|], GEIdx (GEVar (LIdent "x")) (GEVar (LIdent "abc"))
  )

accMulti :: (Text, GExpr)
accMulti = (pack [raw|
  x::0:: xyz ::123
|], GEIdx (GEIdx (GEIdx (GEVar (LIdent "x"))
                        (GEInt (IntTok "0"))
                 )
                 (GEVar (LIdent "xyz"))
          )
          (GEInt (IntTok "123"))
  )

accInvalidIdent :: Text
accInvalidIdent = pack [raw|
  x::UPPER
|]

