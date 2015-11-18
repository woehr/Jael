module Jael.Seq.Builtin where

import Data.List (genericTake)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Jael.Seq.Enum
import Jael.Seq.HM_Types
import Jael.Seq.Struct

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
  [ ( "if"
    , PolyTy ["a"] $ TFun TBool $ TFun (TyVar "a") $ TFun (TyVar "a") (TyVar "a")
    )
  ]

