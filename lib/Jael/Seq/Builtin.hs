module Jael.Seq.Builtin where

import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import           Jael.Seq.Enum
import           Jael.Seq.HM_Types
import           Jael.Seq.Struct
import           Jael.Util

maxBuiltinTupSize :: Int
maxBuiltinTupSize = 10

buildTup :: Int -> (Text, PolyTy)
buildTup i | i < 2 = error "Tuples need at least 2 elements"
buildTup i = (tupFun i, PolyTy (map tshow [1..i]) $ buildTupTy 1)
  where buildTupTy x | x == i+1 = TTup $ map (TyVar . tshow) [1..i]
        buildTupTy x = TFun (TyVar $ tshow x) (buildTupTy $ x+1)

builtinStruct :: [(Text, Struct)]
builtinStruct = [( "IntDivRes", Struct ( NE.fromList [ ("quot", TInt)
                                                    , ("rem", TInt)
                                                    ]
                                      )
                )]

builtinEnums :: [(Text, Enumer)]
builtinEnums = []

builtinFuncs :: TyEnv
builtinFuncs = TyEnv $ M.fromList $
  ( "if"
  , PolyTy ["a"] $ TFun TBool $ TFun (TyVar "a") $ TFun (TyVar "a") (TyVar "a")
  )
  : map buildTup [2..maxBuiltinTupSize]

