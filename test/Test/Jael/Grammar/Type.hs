{-# Language NoImplicitPrelude, QuasiQuotes #-}

module Test.Jael.Grammar.Type
( gTypeTests
) where

import ClassyPrelude
import Jael.Grammar
import Jael.Parser
import Test.Framework as T
import Test.Framework.Providers.HUnit
import Test.HUnit
import Test.Jael.Util
import Test.Jael.Grammar.Util

gTypeTests :: [T.Test]
gTypeTests = [ testCase "unit type" (checkParsedTree pGType unit)
             , testCase "int type" (checkParsedTree pGType int)
             , testCase "bool type" (checkParsedTree pGType bool)
             , testCase "type variable type" (checkParsedTree pGType tvar)
             , testCase "named type, no vars" (checkParsedTree pGType named)
             , testCase "named type, type params" (checkParsedTree pGType kind)
             ]

unit :: (Text, GType)
unit = (pack [raw|
  {}
|], GTUnit GUnit)

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
