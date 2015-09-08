{-# Language NoImplicitPrelude, QuasiQuotes #-}

module Test.Jael.Seq.Enum
( enumTests
) where

import ClassyPrelude
import Jael.Grammar
import Jael.Parser
import Jael.Seq.Types
import Jael.Seq.UserDefTy
import Test.Framework as T
import Test.Framework.Providers.HUnit
import Test.HUnit
import Test.Jael.Util

p :: ParseFun GTEnumDef
p = pGTEnumDef

validator :: GTEnumDef -> Either TDefError [(Text, PolyTy)]
validator = validateType . gToEnumer

checkEnum :: (Text, [(Text, PolyTy)]) -> Assertion
checkEnum = checkParsedTypes p validator

checkErr :: (Text, TDefError) -> Assertion
checkErr = checkTDefErr p validator

enumTests :: [T.Test]
enumTests =
  [ testCase "simple enum with tags with types" $ checkEnum enumTypedTags
  , testCase "simple enum with tags without types" $ checkEnum enumUntypedTags
  , testCase "simple enum with mixed tags" $ checkEnum enumMixedTags
  , testCase "Combination of all possible errors" $ checkErr enumAllErrs
  ]

enumTypedTags :: (Text, [(Text, PolyTy)])
enumTypedTags = (pack [raw|
  X{f0,f1}
|], [ ("X::f0", PolyTy [] $ TNamed "X" [])
    , ("X::f1", PolyTy [] $ TNamed "X" [])
    ]
  )

enumUntypedTags :: (Text, [(Text, PolyTy)])
enumUntypedTags = (pack [raw|
  X { f0 Int, f1 Bool }
|], [ ("X::f0", PolyTy [] $ TFun TInt  (TNamed "X" []))
    , ("X::f1", PolyTy [] $ TFun TBool (TNamed "X" []))
    ]
  )

enumMixedTags :: (Text, [(Text, PolyTy)])
enumMixedTags = (pack [raw|
  X a { f0, f1 a, f2 Bool }
|], [ ("X::f0", PolyTy ["a"] $ TNamed "X" [TVar "a"])
    , ("X::f1", PolyTy ["a"] $ TFun (TVar "a") (TNamed "X" [TVar "a"]))
    , ("X::f2", PolyTy ["a"] $ TFun TBool (TNamed "X" [TVar "a"]))
    ]
  )

enumAllErrs :: (Text, TDefError)
enumAllErrs = (pack [raw|
  X a a b b { f0 a, f1 a, f1 c, f2 c }
|], TDefError { dupTv = ["a", "b"]
              , dupField = ["f1"]
              , freeTv = ["c"]
              , unusedTv = ["b"]
              }
  )

