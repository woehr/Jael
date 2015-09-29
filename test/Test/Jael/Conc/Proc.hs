{-# Language NoImplicitPrelude, QuasiQuotes #-}

module Test.Jael.Conc.Proc
( procTests
) where

import ClassyPrelude
import Jael.Grammar
import Jael.Parser
import Test.Framework as T
import Test.Framework.Providers.HUnit
import Test.HUnit
import Test.Jael.Util

procTests :: [T.Test]
procTests =
  [ testCase "proc validation" $ checkProc allErrs
  ]

checkProc :: (Text, Text) -> Assertion
checkProc (t, expected) =
  case runParser pGProc t of
       Left err -> assertFailure (show err)
       Right gDef ->
         case {-validateProc (gToProc gDef)-} Nothing of
              Just x -> assertEqual "" x expected
              Nothing -> assertFailure "Expected proc error"

allErrs :: (Text, a)
allErrs = (pack [raw|
  new x : SomeProto;
  y = 5;
  x -> z;
  x <- y;
  //x <- True;
  x select label;
  x case
    { p1 => {}
    , p2 => ( {}
            //| SomeProc(x)
            | new a : Proto2;
              ( z <- a; {}
              | z -> b; {}
              )
            )
    , p3 => rec X(j=x, k=1)
              { j <- k;
                ( X(j, k+1)
                | {}
                )
              }
    }
|], undefined
  )

