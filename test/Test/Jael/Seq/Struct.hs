{-# Language NoImplicitPrelude, QuasiQuotes #-}

module Test.Jael.Seq.Struct
( structTests
) where

import ClassyPrelude
import Jael.Grammar
import Jael.Parser
import Jael.Seq.AST
import Jael.Seq.Struct
import Jael.Seq.Types
import Test.Framework as T
import Test.Framework.Providers.HUnit
import Test.HUnit
import Test.Jael.Util

p :: ParseFun GTStructDef
p = pGTStructDef

validator :: GTStructDef -> Either TDefError [(Text, PolyTy)]
validator = validateStruct . gToStruct

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
|], TDefError (DuplicateTyVars ["a"])
                (DuplicateFields [])
                (FreeTyVars [])
                (UnusedTyVars [])
  )

structDupFields :: (Text, TDefError)
structDupFields = (pack [raw|
  X { same :: Int , same :: Bool }
|], TDefError (DuplicateTyVars [])
                (DuplicateFields ["same"])
                (FreeTyVars [])
                (UnusedTyVars [])
  )

structFreeTvs :: (Text, TDefError)
structFreeTvs = (pack [raw|
  X a { f1 :: a , f2 :: b }
|], TDefError (DuplicateTyVars [])
                (DuplicateFields [])
                (FreeTyVars ["b"])
                (UnusedTyVars [])
  )

structUnusedTv :: (Text, TDefError)
structUnusedTv = (pack [raw|
  X a { f1 :: Int , f2 :: Bool }
|], TDefError (DuplicateTyVars [])
                (DuplicateFields [])
                (FreeTyVars [])
                (UnusedTyVars ["a"])
  )

structAllErrs :: (Text, TDefError)
structAllErrs = (pack [raw|
  X a a c c { f1 :: a , f1 :: a , f2 :: b , f2 :: b }
|], TDefError (DuplicateTyVars ["a", "c"])
                (DuplicateFields ["f1", "f2"])
                (FreeTyVars ["b"])
                (UnusedTyVars ["c"])
  )

