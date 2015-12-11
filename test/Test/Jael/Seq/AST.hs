module Test.Jael.Seq.AST
( astTests
) where

import Jael.Seq.Types
import Test.Framework as T

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
tyEq1 = ( TySimple TyInt
        , TySimple TyInt
        )

tyEq2 :: (Ty, Ty)
tyEq2 = ( TyVar "a"
        , TyVar "a"
        )

tyEq3 :: (Ty, Ty)
tyEq3 = ( TyVar "a"
        , TyVar "b"
        )

tyEq4 :: (Ty, Ty)
tyEq4 = ( TyFun (TyVar "a") (TyVar "b")
        , TyFun (TyVar "c") (TyVar "d")
        )

tyEq5 :: (Ty, Ty)
tyEq5 = ( TyFun (TyVar "a") (TyVar "b")
        , TyFun (TyVar "b") (TyVar "c")
        )

tyEq6 :: (Ty, Ty)
tyEq6 = ( TyFun (TyVar "a") (TyVar "a")
        , TyFun (TyVar "b") (TyVar "b")
        )

