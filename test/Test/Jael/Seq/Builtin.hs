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
               , PolyTy [] $ TFun TInt
                                  (TFun TInt
                                        (TNamed "IntDivRes" [])
                                  )
               )
             , ( "intDivRes::quot"
               , PolyTy [] $ TFun (TNamed "IntDivRes" []) TInt
               )
             , ( "intDivRes::rem"
               , PolyTy [] $ TFun (TNamed "IntDivRes" []) TInt
               )
             ]

