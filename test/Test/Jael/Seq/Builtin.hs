module Test.Jael.Seq.Builtin
( builtinTests
) where

import qualified Data.Map as M
import           Jael.Seq.Env
import           Jael.Seq.Types
import qualified Test.Framework as T
import           Test.Jael.Util

checkBuiltins :: [(Text, PolyTy)] -> Assertion
checkBuiltins expected =
  let (TyEnv mDefault) = defaultEnv
      mExpected = M.fromList expected
   in TyEnv mExpected `envEq` TyEnv (M.intersection mDefault mExpected)

builtinTests :: [T.Test]
builtinTests =
  [ testCase "Maybe" $ checkBuiltins eMaybe
  , testCase "Tup1" $ checkBuiltins sTup1
  , testCase "Tup2" $ checkBuiltins sTup2
  , testCase "IntDivRes" $ checkBuiltins sIntDivRes
  ]

eMaybe :: [(Text, PolyTy)]
eMaybe = [ ("Maybe::just", PolyTy ["a"] $ TFun (TyVar "a") (TNamed "Maybe" [TyVar "a"]))
         , ("Maybe::nothing", PolyTy ["a"] $ TNamed "Maybe" [TyVar "a"])
         ]

sTup1 :: [(Text, PolyTy)]
sTup1 = [ ( "tup1"
          , PolyTy ["a"] $ TFun (TyVar "a") (TNamed "Tup1" [TyVar "a"])
          )
        , ( "Tup1::0"
          , PolyTy ["a"] $ TFun (TNamed "Tup1" [TyVar "a"]) (TyVar "a")
          )
        ]

sTup2 :: [(Text, PolyTy)]
sTup2 = [ ( "tup2"
          , PolyTy ["a", "b"] $ TFun (TyVar "a")
                                     (TFun (TyVar "b")
                                           (TNamed "Tup2" [TyVar "a", TyVar "b"])
                                     )
          )
        , ( "Tup2::0"
          , PolyTy ["a", "b"] $ TFun (TNamed "Tup2" [TyVar "a", TyVar "b"]) (TyVar "a")
          )
        , ( "Tup2::1"
          , PolyTy ["a", "b"] $ TFun (TNamed "Tup2" [TyVar "a", TyVar "b"]) (TyVar "b")
          )
        ]

sIntDivRes :: [(Text, PolyTy)]
sIntDivRes = [ ( "intDivRes"
               , PolyTy [] $ TFun TInt
                                  (TFun TInt
                                        (TNamed "IntDivRes" [])
                                  )
               )
             , ( "IntDivRes::quot"
               , PolyTy [] $ TFun (TNamed "IntDivRes" []) TInt
               )
             , ( "IntDivRes::rem"
               , PolyTy [] $ TFun (TNamed "IntDivRes" []) TInt
               )
             ]

