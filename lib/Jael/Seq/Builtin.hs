{-# Language NoImplicitPrelude #-}

module Jael.Seq.Builtin
( builtinFuncs
, builtinStructs
) where

import ClassyPrelude
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Jael.Seq.AST
import Jael.Seq.Struct

builtinStructs :: [Struct]
builtinStructs = [ Struct "IntDivRes" [] $ NE.fromList
                                [ ("quot", TInt)
                                , ("rem", TInt)
                                ]
                 ]

builtinFuncs :: TyEnv
builtinFuncs = M.fromList
  [ ( "if" -- Bool -> a -> a -> a
    , PolyTy ["a"] (TFun TBool (TFun (TVar "a") (TFun (TVar "a") (TVar "a"))))
    )
  , ( "<$" -- (a -> b) -> a -> b
    , PolyTy ["a", "b"] (TFun (TFun (TVar "a") (TVar "b")) (TFun (TVar "a") (TVar "b")))
    )
  , ( "$>" -- a -> (a -> b) -> b
    , PolyTy ["a", "b"] (TFun (TVar "a") (TFun (TFun (TVar "a") (TVar "b")) (TVar "b")))
    )
  , ( "||"
    , PolyTy [] (TFun TBool (TFun TBool TBool))
    )
  , ( "&&"
    , PolyTy [] (TFun TBool (TFun TBool TBool))
    )
  , ( "=="
    , PolyTy ["a"] (TFun (TVar "a") (TFun (TVar "a") TBool))
    )
  , ( "!="
    , PolyTy ["a"] (TFun (TVar "a") (TFun (TVar "a") TBool))
    )
  , ( ">="
    , PolyTy ["a"] (TFun (TVar "a") (TFun (TVar "a") TBool))
    )
  , ( "<="
    , PolyTy ["a"] (TFun (TVar "a") (TFun (TVar "a") TBool))
    )
  , ( ">"
    , PolyTy ["a"] (TFun (TVar "a") (TFun (TVar "a") TBool))
    )
  , ( "<"
    , PolyTy ["a"] (TFun (TVar "a") (TFun (TVar "a") TBool))
    )
  , ( "+"
    , PolyTy ["a"] (TFun (TVar "a") (TFun (TVar "a") (TVar "a")))
    )
  , ( "-"
    , PolyTy ["a"] (TFun (TVar "a") (TFun (TVar "a") (TVar "a")))
    )
  , ( "*"
    , PolyTy ["a"] (TFun (TVar "a") (TFun (TVar "a") (TVar "a")))
    )
  , ( "/"
    , PolyTy [] (TFun TInt (TFun TInt (TNamed "IntDivRes" [])))
    )
  , ( "%"
    , PolyTy [] (TFun TInt (TFun TInt TInt))
    )
  , ( "<o" -- (b -> c) -> (a -> b) -> (a -> c)
    , PolyTy ["a", "b", "c"] (TFun (TFun (TVar "b") (TVar "c")) (TFun (TFun (TVar "a") (TVar "b")) (TFun (TVar "a") (TVar "c"))))
    )
  , ( "o>" -- (a -> b) -> (b -> c) -> (a -> c)
    , PolyTy ["a", "b", "c"] (TFun (TFun (TVar "a") (TVar "b")) (TFun (TFun (TVar "b") (TVar "c")) (TFun (TVar "a") (TVar "c"))))
    )
  , ( "!"
    , PolyTy ["a"] (TFun (TVar "a") (TVar "a"))
    )
  ]

