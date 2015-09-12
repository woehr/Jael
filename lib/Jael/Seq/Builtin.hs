{-# Language NoImplicitPrelude #-}

module Jael.Seq.Builtin
where

import ClassyPrelude
import Data.List (genericTake)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Jael.Seq.Types
import Jael.Seq.UserDefTy

maxBuiltinTupSize :: Integer
maxBuiltinTupSize = 10

buildTup :: Integer -> UserDefTy
buildTup i = let tvs = genericTake i . map (\v -> "a" ++ tshow v) $ ([0..]::[Integer])
              in Struct ("Tup" ++ tshow i) tvs $ NE.fromList
                                               $ map (tshow *** TVar)
                                               $ zip ([0..]::[Integer]) tvs

builtinTypes :: [UserDefTy]
builtinTypes = Struct "IntDivRes" [] ( NE.fromList [ ("quot", TInt)
                                                     , ("rem", TInt)
                                                     ]
                                       )
               : [ Enumer "Maybe" ["a"] $ NE.fromList [ TagWithTy "just" (TVar "a")
                                                      , Tag "nothing"
                                                      ]
                 ]
               ++ map buildTup [1..maxBuiltinTupSize]

builtinFuncs :: TyEnv
builtinFuncs = TyEnv $ M.fromList
  [ ( "<$" -- (a -> b) -> a -> b
    , PolyTy ["a", "b"] (TFun (TFun (TVar "a") (TVar "b")) (TFun (TVar "a") (TVar "b")))
    )
  , ( "$>" -- a -> (a -> b) -> b
    , PolyTy ["a", "b"] (TFun (TVar "a") (TFun (TFun (TVar "a") (TVar "b")) (TVar "b")))
    )
  , ( "<o" -- (b -> c) -> (a -> b) -> (a -> c)
    , PolyTy ["a", "b", "c"] (TFun (TFun (TVar "b") (TVar "c")) (TFun (TFun (TVar "a") (TVar "b")) (TFun (TVar "a") (TVar "c"))))
    )
  , ( "o>" -- (a -> b) -> (b -> c) -> (a -> c)
    , PolyTy ["a", "b", "c"] (TFun (TFun (TVar "a") (TVar "b")) (TFun (TFun (TVar "b") (TVar "c")) (TFun (TVar "a") (TVar "c"))))
    )
  ]

