{-# Language NoImplicitPrelude, QuasiQuotes #-}

module Test.Jael.Seq.AST
( astTests
) where

import ClassyPrelude
import Jael.Seq.Types
import Test.Framework as T
import Test.Framework.Providers.HUnit
import Test.HUnit

astTests :: [T.Test]
astTests = [ testCase "tyEquiv (1)" $ assertTyEquiv tyEq1
           , testCase "tyEquiv (2)" $ assertTyEquiv tyEq2
           , testCase "tyEquiv (3)" $ assertTyEquiv tyEq3
           , testCase "tyEquiv (4)" $ assertTyEquiv tyEq4
           , testCase "tyEquiv (5)" $ assertTyEquiv tyEq5
           , testCase "tyEquiv (6)" $ assertTyEquiv tyEq6
           ]

assertTyEquiv :: (Ty, Ty) -> Assertion
assertTyEquiv ts@(t1, t2) =
  assertBool ("Expected:\n" ++ show t1 ++
              "\n     and:\n" ++ show t2 ++
              "\nto be equivalent."
             ) $ uncurry tyEquiv ts

tyEq1 :: (Ty, Ty)
tyEq1 = ( TInt
        , TInt
        )

tyEq2 :: (Ty, Ty)
tyEq2 = ( TVar "a"
        , TVar "a"
        )

tyEq3 :: (Ty, Ty)
tyEq3 = ( (TVar "a")
        , (TVar "b")
        )

tyEq4 :: (Ty, Ty)
tyEq4 = ( (TFun (TVar "a") (TVar "b"))
        , (TFun (TVar "c") (TVar "d"))
        )

tyEq5 :: (Ty, Ty)
tyEq5 = ( (TFun (TVar "a") (TVar "b"))
        , (TFun (TVar "b") (TVar "c"))
        )

tyEq6 :: (Ty, Ty)
tyEq6 = ( (TFun (TVar "a") (TVar "a"))
        , (TFun (TVar "b") (TVar "b"))
        )

