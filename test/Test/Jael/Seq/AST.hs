module Test.Jael.Seq.AST
( astTests
) where

import Jael.Seq.Types
import Test.Jael.Util
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

assertTyEquiv :: (HMTy, HMTy) -> Assertion
assertTyEquiv ts@(t1, t2) =
  assertBool ("Expected:\n" ++ show t1 ++
              "\n     and:\n" ++ show t2 ++
              "\nto be equivalent."
             ) $ uncurry tyEquiv ts

tyEq1 :: (HMTy, HMTy)
tyEq1 = ( HMTyInt
        , HMTyInt
        )

tyEq2 :: (HMTy, HMTy)
tyEq2 = ( HMTyVar "a"
        , HMTyVar "a"
        )

tyEq3 :: (HMTy, HMTy)
tyEq3 = ( HMTyVar "a"
        , HMTyVar "b"
        )

tyEq4 :: (HMTy, HMTy)
tyEq4 = ( HMTyFun (HMTyVar "a") (HMTyVar "b")
        , HMTyFun (HMTyVar "c") (HMTyVar "d")
        )

tyEq5 :: (HMTy, HMTy)
tyEq5 = ( HMTyFun (HMTyVar "a") (HMTyVar "b")
        , HMTyFun (HMTyVar "b") (HMTyVar "c")
        )

tyEq6 :: (HMTy, HMTy)
tyEq6 = ( HMTyFun (HMTyVar "a") (HMTyVar "a")
        , HMTyFun (HMTyVar "b") (HMTyVar "b")
        )

