module Test.Jael.Seq.Builtin
( builtinTests
) where

import qualified Data.Map as M
import           Jael.Seq.Env
import           Jael.Seq.Types
import qualified Test.Framework as T
import           Test.Jael.Util

checkBuiltins :: [(Text, HMPolyTy)] -> Assertion
checkBuiltins expected =
  let (HMTyEnv mDefault) = defaultEnv
      mExpected = M.fromList expected
   in HMTyEnv mExpected `envEq` HMTyEnv (M.intersection mDefault mExpected)

builtinTests :: [T.Test]
builtinTests =
  [ testCase "IntDivRes" $ checkBuiltins sIntDivRes
  ]

sIntDivRes :: [(Text, HMPolyTy)]
sIntDivRes = [ ( "intDivRes"
               , HMPolyTy [] $ HMTyFun HMTyInt
                                   (HMTyFun HMTyInt
                                          (HMTyNamed "IntDivRes" [])
                                   )
               )
             , ( "intDivRes::quot"
               , HMPolyTy [] $ HMTyFun (HMTyNamed "IntDivRes" []) HMTyInt
               )
             , ( "intDivRes::rem"
               , HMPolyTy [] $ HMTyFun (HMTyNamed "IntDivRes" []) HMTyInt
               )
             ]

