{-# Language NoImplicitPrelude, QuasiQuotes #-}

module Test.Jael.Seq.Struct
( structTests
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

p :: ParseFun GTStructDef
p = pGTStructDef

validator :: GTStructDef -> Either TDefError [(Text, PolyTy)]
validator = validateType . gToStruct

checkStruct :: (Text, [(Text, PolyTy)]) -> Assertion
checkStruct = checkParsedTypes p validator

checkErr :: (Text, TDefError) -> Assertion
checkErr = checkTDefErr p validator

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
  X { f0 :: Int ,f1::Bool}
|], [ ("x",     PolyTy [] $ TFun TInt (TFun TBool (TNamed "X" [])))
    , ("X::f0", PolyTy [] $ TFun (TNamed "X" []) TInt)
    , ("X::f1", PolyTy [] $ TFun (TNamed "X" []) TBool)
    ]
  )

structValidPoly :: (Text, [(Text, PolyTy)])
structValidPoly = (pack [raw|
  X a b{f0::a, f1 :: b }
|], [ ("x",     PolyTy ["a", "b"] $ TFun (TVar "a") (TFun (TVar "b") (TNamed "X" [TVar "a", TVar "b"])))
    , ("X::f0", PolyTy ["a", "b"] $ TFun (TNamed "X" [TVar "a", TVar "b"]) (TVar "a"))
    , ("X::f1", PolyTy ["a", "b"] $ TFun (TNamed "X" [TVar "a", TVar "b"]) (TVar "b"))
    ]
  )

structDupTyVars :: (Text, TDefError)
structDupTyVars = (pack [raw|
  X a a { f1:: a , f2 :: a }
|],    TDefError { dupTv = ["a"]
                 , dupField = []
                 , freeTv = []
                 , unusedTv = []
                 }
  )

structDupFields :: (Text, TDefError)
structDupFields = (pack [raw|
  X { same :: Int , same :: Bool }
|],    TDefError { dupTv = []
                 , dupField = ["same"]
                 , freeTv = []
                 , unusedTv = []
                 }
  )

structFreeTvs :: (Text, TDefError)
structFreeTvs = (pack [raw|
  X a { f1 :: a , f2 :: b }
|],    TDefError { dupTv = []
                 , dupField = []
                 , freeTv = ["b"]
                 , unusedTv = []
                 }
  )

structUnusedTv :: (Text, TDefError)
structUnusedTv = (pack [raw|
  X a { f1 :: Int , f2 :: Bool }
|],    TDefError { dupTv = []
                 , dupField = []
                 , freeTv = []
                 , unusedTv = ["a"]
                 }
  )

structAllErrs :: (Text, TDefError)
structAllErrs = (pack [raw|
  X a a c c { f1 :: a , f1 :: a , f2 :: b , f2 :: b }
|],    TDefError { dupTv = ["a", "c"]
                 , dupField = ["f1", "f2"]
                 , freeTv = ["b"]
                 , unusedTv = ["c"]
                 }
  )

