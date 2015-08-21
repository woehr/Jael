{-# Language NoImplicitPrelude, QuasiQuotes #-}

module Test.Jael.Seq.Enum
( enumTests
) where

import ClassyPrelude
import Jael.Grammar
import Jael.Parser
import Jael.Seq.AST
import Jael.Seq.Enum
import Jael.Seq.Types
import Test.Framework as T
import Test.Framework.Providers.HUnit
import Test.HUnit
import Test.Jael.Util

p :: ParseFun GTEnumDef
p = pGTEnumDef

validator :: GTEnumDef -> Either TDefError [(Text, PolyTy)]
validator = validateEnum . gToEnum

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
|], [ ("f0", PolyTy [] $ TNamed "X" [])
    , ("f1", PolyTy [] $ TNamed "X" [])
    ]
  )

enumUntypedTags :: (Text, [(Text, PolyTy)])
enumUntypedTags = (pack [raw|
  X { f0 Int, f1 Bool }
|], [ ("f0", PolyTy [] $ TFun TInt  (TNamed "X" []))
    , ("f1", PolyTy [] $ TFun TBool (TNamed "X" []))
    ]
  )

enumMixedTags :: (Text, [(Text, PolyTy)])
enumMixedTags = (pack [raw|
  X a { f0, f1 a }
|], [ ("f0", PolyTy [] $ TNamed "X" [])
    , ("f1", PolyTy [] $ TFun (TVar "a") (TNamed "X" []))
    ]
  )

enumAllErrs :: (Text, TDefError)
enumAllErrs = (pack [raw|
  X a a b b { f0 a, f1 a, f1 c, f2 c }
|], TDefError (DuplicateTyVars ["a", "b"])
              (DuplicateFields ["f1"])
              (FreeTyVars ["c"])
              (UnusedTyVars ["b"])
  )

