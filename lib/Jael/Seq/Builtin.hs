module Jael.Seq.Builtin where

import Data.List (genericTake)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Jael.Seq.Enum
import Jael.Seq.Struct
import Jael.Seq.Types

maxBuiltinTupSize :: Integer
maxBuiltinTupSize = 10

buildTup :: Integer -> (Text, Struct)
buildTup i = let tvs = genericTake i . map (\v -> "a" <> (pack . show) v) $ ([0..]::[Integer])
              in ("Tup" <> (pack . show) i, Struct tvs $ NE.fromList
                                               $ map ((pack . show) *** TyVar)
                                               $ zip ([0..]::[Integer]) tvs
                 )

builtinStruct :: [(Text, Struct)]
builtinStruct = ( "IntDivRes", Struct [] ( NE.fromList [ ("quot", TInt)
                                                       , ("rem", TInt)
                                                       ]
                                        )
                ) : map buildTup [1..maxBuiltinTupSize]

builtinEnums :: [(Text, Enumer)]
builtinEnums = [ ("Maybe", Enumer ["a"] $ NE.fromList [ TagWithTy "just" (TyVar "a")
                                                    , Tag "nothing"
                                                    ]
                 )
               ]

builtinFuncs :: TyEnv
builtinFuncs = TyEnv $ M.fromList
  [ ( "<$" -- (a -> b) -> a -> b
    , PolyTy ["a", "b"] (TFun (TFun (TyVar "a") (TyVar "b")) (TFun (TyVar "a") (TyVar "b")))
    )
  , ( "$>" -- a -> (a -> b) -> b
    , PolyTy ["a", "b"] (TFun (TyVar "a") (TFun (TFun (TyVar "a") (TyVar "b")) (TyVar "b")))
    )
  , ( "<o" -- (b -> c) -> (a -> b) -> (a -> c)
    , PolyTy ["a", "b", "c"] (TFun (TFun (TyVar "b") (TyVar "c")) (TFun (TFun (TyVar "a") (TyVar "b")) (TFun (TyVar "a") (TyVar "c"))))
    )
  , ( "o>" -- (a -> b) -> (b -> c) -> (a -> c)
    , PolyTy ["a", "b", "c"] (TFun (TFun (TyVar "a") (TyVar "b")) (TFun (TFun (TyVar "b") (TyVar "c")) (TFun (TyVar "a") (TyVar "c"))))
    )
  ]

