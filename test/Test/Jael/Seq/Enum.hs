{-# Language NoImplicitPrelude, QuasiQuotes #-}

module Test.Jael.Seq.Enum
( enumTests
) where

import ClassyPrelude
import qualified Data.Set as S
import Jael.Grammar
import Jael.Seq.Types
import Jael.Seq.UserDefTy
import Test.Framework as T
import Test.Framework.Providers.HUnit
import Test.HUnit
import Test.Jael.Util

validator :: GTypeDef -> Either TDefError [(Text, PolyTy)]
validator x =
  case gToUserDefTy x of
    (n, y@(Enumer _ _)) -> validateType (n, y)
    _ -> error "Parsed non-enum typedef"

checkEnum :: (Text, [(Text, PolyTy)]) -> Assertion
checkEnum = checkParsedTypes pGTypeDef ((either (Left . tshow) Right) . validator)

checkErr :: (Text, TDefError) -> Assertion
checkErr = checkTDefErr pGTypeDef validator

enumTests :: [T.Test]
enumTests =
  [ testCase "simple enum with tags with types" $ checkEnum enumTypedTags
  , testCase "simple enum with tags without types" $ checkEnum enumUntypedTags
  , testCase "simple enum with mixed tags" $ checkEnum enumMixedTags
  , testCase "Combination of all possible errors" $ checkErr enumAllErrs
  ]

enumTypedTags :: (Text, [(Text, PolyTy)])
enumTypedTags = (pack [raw|
  enum X{f0,f1}
|], [ ("X::f0", PolyTy [] $ TNamed "X" [])
    , ("X::f1", PolyTy [] $ TNamed "X" [])
    ]
  )

enumUntypedTags :: (Text, [(Text, PolyTy)])
enumUntypedTags = (pack [raw|
  enum X { f0 Int, f1 Bool }
|], [ ("X::f0", PolyTy [] $ TFun TInt  (TNamed "X" []))
    , ("X::f1", PolyTy [] $ TFun TBool (TNamed "X" []))
    ]
  )

enumMixedTags :: (Text, [(Text, PolyTy)])
enumMixedTags = (pack [raw|
  enum X a { f0, f1 a, f2 Bool }
|], [ ("X::f0", PolyTy ["a"] $ TNamed "X" [TVar "a"])
    , ("X::f1", PolyTy ["a"] $ TFun (TVar "a") (TNamed "X" [TVar "a"]))
    , ("X::f2", PolyTy ["a"] $ TFun TBool (TNamed "X" [TVar "a"]))
    ]
  )

enumAllErrs :: (Text, TDefError)
enumAllErrs = (pack [raw|
  enum X a a b b { f0 a, f1 a, f1 c, f2 c }
|], TDefError { dupTv = S.fromList ["a", "b"]
              , dupField = S.fromList ["f1"]
              , freeTv = S.fromList ["c"]
              , unusedTv = S.fromList ["b"]
              }
  )

