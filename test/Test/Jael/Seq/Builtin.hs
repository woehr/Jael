{-# Language NoImplicitPrelude #-}

module Test.Jael.Seq.Builtin
( builtinTests
) where

import ClassyPrelude
import qualified Data.Map as M
import Jael.Seq.AST
import Jael.Seq.Env
import Test.Framework as T
import Test.Framework.Providers.HUnit
import Test.HUnit
import Test.Jael.Util

checkBuiltins :: [(Text, PolyTy)] -> Assertion
checkBuiltins expected =
  let mExpected = M.fromList expected
   in mExpected `envEq` M.intersection defaultEnv mExpected

builtinTests :: [T.Test]
builtinTests =
  [ testCase "Maybe" $ checkBuiltins eMaybe
  , testCase "Tup1" $ checkBuiltins sTup1
  , testCase "Tup2" $ checkBuiltins sTup2
  , testCase "IntDivRes" $ checkBuiltins sIntDivRes
  ]

eMaybe :: [(Text, PolyTy)]
eMaybe = [ ("Maybe::just", PolyTy ["a"] $ TFun (TVar "a") (TNamed "Maybe" [TVar "a"]))
         , ("Maybe::nothing", PolyTy ["a"] $ TNamed "Maybe" [TVar "a"])
         ]

sTup1 :: [(Text, PolyTy)]
sTup1 = [ ( "tup1"
          , PolyTy ["a"] $ TFun (TVar "a") (TNamed "Tup1" [TVar "a"])
          )
        , ( "Tup1::0"
          , PolyTy ["a"] $ TFun (TNamed "Tup1" [TVar "a"]) (TVar "a")
          )
        ]

sTup2 :: [(Text, PolyTy)]
sTup2 = [ ( "tup2"
          , PolyTy ["a", "b"] $ TFun (TVar "a")
                                     (TFun (TVar "b")
                                           (TNamed "Tup2" [TVar "a", TVar "b"])
                                     )
          )
        , ( "Tup2::0"
          , PolyTy ["a", "b"] $ TFun (TNamed "Tup2" [TVar "a", TVar "b"]) (TVar "a")
          )
        , ( "Tup2::1"
          , PolyTy ["a", "b"] $ TFun (TNamed "Tup2" [TVar "a", TVar "b"]) (TVar "b")
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

