module Test.Jael.Seq.Builtin
( builtinTests
) where

import qualified Data.Map as M
import           Jael.Seq.Env
import           Jael.Seq.HM_Types
import qualified Test.Framework as T
import           Test.Jael.Util

checkBuiltins :: [(Text, PolyTy)] -> Assertion
checkBuiltins expected =
  let (TyEnv mDefault) = defaultEnv
      mExpected = M.fromList expected
   in TyEnv mExpected `envEq` TyEnv (M.intersection mDefault mExpected)

builtinTests :: [T.Test]
builtinTests =
  [ testCase "IntDivRes" $ checkBuiltins sIntDivRes
  ]

sIntDivRes :: [(Text, PolyTy)]
sIntDivRes = [ ( "intDivRes"
               , PolyTy [] $ TyFun (TySimple TyInt)
                                   (TyFun (TySimple TyInt)
                                          (TyNamed "IntDivRes" [])
                                   )
               )
             , ( "intDivRes::quot"
               , PolyTy [] $ TyFun (TyNamed "IntDivRes" []) (TySimple TyInt)
               )
             , ( "intDivRes::rem"
               , PolyTy [] $ TyFun (TyNamed "IntDivRes" []) (TySimple TyInt)
               )
             ]

