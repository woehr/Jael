{-# Language NoImplicitPrelude, QuasiQuotes #-}

module Test.Jael.Compile
( compileTests
) where

import ClassyPrelude
import qualified Data.Set as S
import Jael.Compile
import Jael.Err
import Test.Framework as T
import Test.Framework.Providers.HUnit
import Test.HUnit
import Test.Jael.Util

compileTests :: [T.Test]
compileTests =
  [ testCase "duplicate def" $ assertCompErr dupDef
  , testCase "undefined var" $ assertCompErr undefinedVar
  , testCase "call cycle" $ assertCompErr callCycle
  , testCase "recursive type" $ assertCompErr recType
  , testCase "undefined type" $ assertCompErr undefType
  ]

assertCompErr :: (Text, CompileErr) -> Assertion
assertCompErr (t, e) =
  either (assertEqual "" e)
         (const $ assertFailure "Expected compile error but result was success")
         (compile t)

dupDef :: (Text, CompileErr)
dupDef = (pack [raw|
  f=1;
  f=2;
|], DupDef ["f"]
  )

undefinedVar :: (Text, CompileErr)
undefinedVar = (pack [raw|
  f=g;
|], UndefName $ S.fromList ["g"]
  )

callCycle :: (Text, CompileErr)
callCycle = (pack [raw|
  f=g;
  g=f;
|], DepCycle ["f", "g"]
  )

recType :: (Text, CompileErr)
recType = (pack [raw|
  struct S {
    f1 :: S
  }
|], DepCycle ["S"]
  )

undefType :: (Text, CompileErr)
undefType = (pack [raw|
  enum E { f1 T }
|], UndefName $ S.fromList ["T"]
  )

