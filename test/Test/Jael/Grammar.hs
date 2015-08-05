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
grammarTests = [ testCase "expression parsing" testExprParser ]

expr1 :: Text
expr1 = pack [raw|
  2+2
|]

shouldParse :: ParseFun a -> Text -> Assertion
shouldParse p t = either (assertFailure . unpack) (\_ -> return ()) (runParser p t)


testExprParser :: Assertion
testExprParser = shouldParse pGExpr expr1

