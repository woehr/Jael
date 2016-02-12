module Jael.Seq.Env where

import qualified Data.Map as M
import qualified Data.Set as S
import           Jael.Seq.Types
import           Jael.Seq.Prm
import           Jael.Util

newtype HMTyEnv = HMTyEnv (M.Map Text HMPolyTy)
  deriving (Show)

instance TIOps HMTyEnv where
  ftv (HMTyEnv env) = S.unions . map ftv . M.elems $ env
  apply sub (HMTyEnv env) = HMTyEnv $ M.map (apply sub) env

defaultEnv :: HMTyEnv
defaultEnv =
  case addToEnv builtinFuncs prmFuncs of
       Left dups -> error $ unpack . intercalate "\n"
                          $ "Duplicates in environment when adding builtins:"
                            :dups
       Right env' -> env'

addToEnv :: HMTyEnv -> [(Text, HMPolyTy)] -> Either [Text] HMTyEnv
addToEnv (HMTyEnv env) = liftA HMTyEnv . insertCollectDups env

maxBuiltinTupSize :: Int
maxBuiltinTupSize = 10

buildTup :: Int -> (Text, HMPolyTy)
buildTup i | i < 2 = error "Tuples need at least 2 elements"
buildTup i = (tupFun i, HMPolyTy (map tshow [1..i]) $ buildTupTy 1)
  where buildTupTy x | x == i+1 = HMTyTup $ map (HMTyVar . tshow) [1..i]
        buildTupTy x = HMTyFun (HMTyVar $ tshow x) (buildTupTy $ x+1)

builtinFuncs :: HMTyEnv
builtinFuncs = HMTyEnv $ M.fromList $
  ( "if"
  , HMPolyTy ["a"] $ HMTyFun HMTyBool $ HMTyFun (HMTyVar "a") $ HMTyFun (HMTyVar "a") (HMTyVar "a")
  )
  : map buildTup [2..maxBuiltinTupSize]

