module Test.Jael.Grammar.Expr
( gExprTests
) where

import           Jael.Grammar
import qualified Test.Framework as T
import           Test.Jael.Util

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
             , testCase "tuple scoped fn" (checkParsedTree pGExpr accIndex)
             , testCase "struct scoped fn" (checkParsedTree pGExpr accLabel)
             , testCase "invalid scoped" (shouldNotParse pGExpr accMulti)
             ]

intZero :: (Text, GExpr)
intZero = (pack [raw|
  0
|], GEInt (DecInt "0")
  )

intNeg :: (Text, GExpr)
intNeg = (pack [raw|
  ~123
|], GEInt (DecInt "~123")
  )

intPos :: (Text, GExpr)
intPos = (pack [raw|
  123
|], GEInt (DecInt "123")
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
          [GEAppArg (GEPlus (GEInt (DecInt "1"))
                            (GEInt (DecInt "2"))
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
  , void
  , 42
  }
|], GETup $ map GETupArg [ GEInt (DecInt "1")
                         , GETrue
                         , GEUnit
                         , GEInt (DecInt "42")
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
                     (GETup $ map GETupArg [ GEInt (DecInt "1")
                                           , GEInt (DecInt "2")
                                           , GEVar (LIdent "a")
                                           , GEPlus (GEVar (LIdent "a"))
                                                    (GEVar (LIdent "b"))
                                           ]
                     )
          )
  )

accIndex :: (Text, GExpr)
accIndex = (pack [raw|
  Tup1::0
|], GEScopedFn (UIdent "Tup1") (GEScopeIndex (DecInt "0"))
  )

accLabel :: (Text, GExpr)
accLabel = (pack [raw|
  SomeStruct::someField(x)
|], GEApp (GEScopedFn (UIdent "SomeStruct")
                      (GEScopeIdent (LIdent "someField"))
          )
          [ GEAppArg (GEVar (LIdent "x")) ]
  )

accMulti :: Text
accMulti = pack [raw|
  x::a::b x
|]

