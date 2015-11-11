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
astTests =
  [ testCase "tyEquiv (1)" $ assertTyEquiv tyEq1
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
tyEq2 = ( TyVar "a"
        , TyVar "a"
        )

tyEq3 :: (Ty, Ty)
tyEq3 = ( (TyVar "a")
        , (TyVar "b")
        )

tyEq4 :: (Ty, Ty)
tyEq4 = ( (TFun (TyVar "a") (TyVar "b"))
        , (TFun (TyVar "c") (TyVar "d"))
        )

tyEq5 :: (Ty, Ty)
tyEq5 = ( (TFun (TyVar "a") (TyVar "b"))
        , (TFun (TyVar "b") (TyVar "c"))
        )

tyEq6 :: (Ty, Ty)
tyEq6 = ( (TFun (TyVar "a") (TyVar "a"))
        , (TFun (TyVar "b") (TyVar "b"))
        )

