{-# Language NoImplicitPrelude #-}

module Jael.Seq.Builtin
where

import ClassyPrelude hiding (Enum)
import Data.List (genericTake)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Jael.Seq.Enum
import Jael.Seq.Struct
import Jael.Seq.Types

maxBuiltinTupSize :: Integer
maxBuiltinTupSize = 10

buildTup :: Integer -> (Text, Struct)
buildTup i = let tvs = genericTake i . map (\v -> "a" ++ tshow v) $ ([0..]::[Integer])
              in ("Tup" ++ tshow i, Struct tvs $ NE.fromList
                                               $ map (tshow *** TVar)
                                               $ zip ([0..]::[Integer]) tvs
                 )

builtinStruct :: [(Text, Struct)]
builtinStruct = ( "IntDivRes", Struct [] ( NE.fromList [ ("quot", TInt)
                                                       , ("rem", TInt)
                                                       ]
                                        )
                ) : map buildTup [1..maxBuiltinTupSize]

builtinEnums :: [(Text, Enum)]
builtinEnums = [ ("Maybe", Enum ["a"] $ NE.fromList [ TagWithTy "just" (TVar "a")
                                                    , Tag "nothing"
                                                    ]
                 )
               ]

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

