module Test.Jael.Grammar.Type
( gTypeTests
) where

import           Jael.Grammar
import qualified Test.Framework as T
import           Test.Jael.Util

gTypeTests :: [T.Test]
gTypeTests = [ testCase "unit type" (checkParsedTree pGType unit)
             , testCase "int type" (checkParsedTree pGType int)
             , testCase "bool type" (checkParsedTree pGType bool)
             , testCase "type variable type" (checkParsedTree pGType tvar)
             , testCase "tuple type (1)" (checkParsedTree pGType tup1)
             , testCase "tuple type (2)" (checkParsedTree pGType tup2)
             , testCase "named type, no vars" (checkParsedTree pGType named)
             , testCase "named type, type params" (checkParsedTree pGType kind)
             ]

unit :: (Text, GType)
unit = (pack [raw|
  Void
|], GTUnit)

int :: (Text, GType)
int = (pack [raw|
  Int
|], GTInt)

bool :: (Text, GType)
bool = (pack [raw|
  Bool
|], GTBool)

tvar :: (Text, GType)
tvar = (pack [raw|
  a
|], GTTVar (LIdent "a"))

tup1 :: (Text, GType)
tup1 = (pack [raw|
  { Int }
|], GTTup $ map GTTupArg [ GTInt
                         ]
  )

tup2 :: (Text, GType)
tup2 = (pack [raw|
  { I, a }
|], GTTup $ map GTTupArg [ GTNamed (UIdent "I") GTNamedNoParam
                         , GTTVar (LIdent "a")
                         ]
  )

named :: (Text, GType)
named = (pack [raw|
  X
|], GTNamed (UIdent "X") GTNamedNoParam)

kind :: (Text, GType)
kind = (pack [raw|
  A( a, Int, Bool, B, C(a, Int, Bool))
|], GTNamed (UIdent "A") (GTNamedParams
            (map GTNamedParam [ GTTVar (LIdent "a")
                              , GTInt
                              , GTBool
                              , GTNamed (UIdent "B") GTNamedNoParam
                              , GTNamed (UIdent "C") (GTNamedParams
                                  (map GTNamedParam [ GTTVar (LIdent "a")
                                                    , GTInt
                                                    , GTBool
                                                    ]
                                  ))
                              ]
            ))
  )
