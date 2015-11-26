module Jael.Seq.Builtin where

import qualified Data.Map as M
import           Jael.Seq.CG_Types
import           Jael.Seq.HM_Types
import           Jael.Seq.UserDefinedType
import           Jael.Util

maxBuiltinTupSize :: Int
maxBuiltinTupSize = 10

buildTup :: Int -> (Text, PolyTy)
buildTup i | i < 2 = error "Tuples need at least 2 elements"
buildTup i = (tupFun i, PolyTy (map tshow [1..i]) $ buildTupTy 1)
  where buildTupTy x | x == i+1 = TyTup $ map (TyVar . tshow) [1..i]
        buildTupTy x = TyFun (TyVar $ tshow x) (buildTupTy $ x+1)

builtinTypes :: [(Text, UserDefinedType)]
builtinTypes = [( "IntDivRes", UDTStruct [ ("quot", GramTySimple (BTInt Nothing Nothing))
                                          , ("rem", GramTySimple (BTInt Nothing Nothing))
                                          ]
                )]

builtinFuncs :: TyEnv
builtinFuncs = TyEnv $ M.fromList $
  ( "if"
  , PolyTy ["a"] $ TyFun (TySimple TyBool) $ TyFun (TyVar "a") $ TyFun (TyVar "a") (TyVar "a")
  )
  : map buildTup [2..maxBuiltinTupSize]

