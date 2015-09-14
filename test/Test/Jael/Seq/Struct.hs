{-# Language NoImplicitPrelude, QuasiQuotes #-}

module Test.Jael.Seq.Struct
( structTests
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
       (n, y@(Struct _ _)) -> validateType (n, y)
       _ -> error "Parsed non-struct typedef"

checkStruct :: (Text, [(Text, PolyTy)]) -> Assertion
checkStruct = checkParsedTypes pGTypeDef ((either (Left . tshow) Right) . validator)

checkErr :: (Text, TDefError) -> Assertion
checkErr = checkTDefErr pGTypeDef validator

structTests :: [T.Test]
structTests =
  [ testCase "Valid struct, not polymorphic" $ checkStruct structValidSimple
  , testCase "Valid struct, polymorphic" $ checkStruct structValidPoly
  , testCase "Duplicate type variables" $ checkErr structDupTyVars
  , testCase "Duplicate fields" $ checkErr structDupFields
  , testCase "Free type variables in struct types" $ checkErr structFreeTvs
  , testCase "Unused type variable in struct" $ checkErr structUnusedTv
  , testCase "Combination of all possible errors" $ checkErr structAllErrs
  ]

structValidSimple :: (Text, [(Text, PolyTy)])
structValidSimple = (pack [raw|
  struct X { f0 :: Int ,f1::Bool}
|], [ ("x",     PolyTy [] $ TFun TInt (TFun TBool (TNamed "X" [])))
    , ("X::f0", PolyTy [] $ TFun (TNamed "X" []) TInt)
    , ("X::f1", PolyTy [] $ TFun (TNamed "X" []) TBool)
    ]
  )

structValidPoly :: (Text, [(Text, PolyTy)])
structValidPoly = (pack [raw|
  struct X a b{f0::a, f1 :: b }
|], [ ("x",     PolyTy ["a", "b"] $ TFun (TVar "a") (TFun (TVar "b") (TNamed "X" [TVar "a", TVar "b"])))
    , ("X::f0", PolyTy ["a", "b"] $ TFun (TNamed "X" [TVar "a", TVar "b"]) (TVar "a"))
    , ("X::f1", PolyTy ["a", "b"] $ TFun (TNamed "X" [TVar "a", TVar "b"]) (TVar "b"))
    ]
  )

structDupTyVars :: (Text, TDefError)
structDupTyVars = (pack [raw|
  struct X a a { f1:: a , f2 :: a }
|],    TDefError { dupTv = S.fromList ["a"]
                 , dupField = S.empty
                 , freeTv = S.empty
                 , unusedTv = S.empty
                 }
  )

structDupFields :: (Text, TDefError)
structDupFields = (pack [raw|
  struct X { same :: Int , same :: Bool }
|],    TDefError { dupTv = S.empty
                 , dupField = S.fromList ["same"]
                 , freeTv = S.empty
                 , unusedTv = S.empty
                 }
  )

structFreeTvs :: (Text, TDefError)
structFreeTvs = (pack [raw|
  struct X a { f1 :: a , f2 :: b }
|],    TDefError { dupTv = S.empty
                 , dupField = S.empty
                 , freeTv = S.fromList ["b"]
                 , unusedTv = S.empty
                 }
  )

structUnusedTv :: (Text, TDefError)
structUnusedTv = (pack [raw|
  struct X a { f1 :: Int , f2 :: Bool }
|],    TDefError { dupTv = S.empty
                 , dupField = S.empty
                 , freeTv = S.empty
                 , unusedTv = S.fromList ["a"]
                 }
  )

structAllErrs :: (Text, TDefError)
structAllErrs = (pack [raw|
  struct X a a c c { f1 :: a , f1 :: a , f2 :: b , f2 :: b }
|],    TDefError { dupTv = S.fromList ["a", "c"]
                 , dupField = S.fromList ["f1", "f2"]
                 , freeTv = S.fromList ["b"]
                 , unusedTv = S.fromList ["c"]
                 }
  )

