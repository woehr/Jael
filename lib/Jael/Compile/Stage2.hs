module Jael.Compile.Stage2 where

import qualified Data.Map as M
import           Jael.Compile.Common
import           Jael.Conc.Proc
import           Jael.Conc.TyCk.S2
import           Jael.Seq.AST
import           Jael.Seq.Env
import           Jael.Seq.TI.S2
import           Jael.Seq.Types
import           Jael.Seq.UserDefinedType

addToTyEnv :: HMTyEnv -> [(Text, HMPolyTy)] -> CompileErrM HMTyEnv
addToTyEnv env xs = either (throwError . DupDef) return (addToEnv env xs)

processSeqTypes :: HMTyEnv
                -> M.Map Text UserDefinedType
                -> CompileErrM HMTyEnv
processSeqTypes env udts =
  let udtItems = M.mapWithKey (curry seqEnvItems) udts
   in addToTyEnv env (concat . M.elems $ udtItems)

typeCheckTopExpr :: HMTyEnv
                 -> TopExpr S1Ex S1Ty
                 -> CompileErrM (TopExpr S2TyEx S2Ty)
typeCheckTopExpr env (TopGlob{..}) =
  either (throwError . TypeInfErr)
         (\x -> return TopGlob{ tgExpr = x })
         (typeInf env tgExpr)
typeCheckTopExpr env (TopFunc{..}) =
  either (throwError . TypeInfErr)
         (\(as,rt,ex) -> return TopFunc{ tfArgs  = zip (map fst tfArgs) as
                                       , tfRetTy = rt
                                       , tfExpr  = ex }
         )
         (typeCheckFunc env (tfArgs, tfRetTy, tfExpr))

typeCheckSeq :: M.Map Text (TopExpr S1Ex S1Ty)
             -> [Text]
             -> HMTyEnv
             -> CompileErrM (HMTyEnv, M.Map Text (TopExpr S2TyEx S2Ty))
typeCheckSeq exprs order env =
  foldM (\(env', acc) def ->
    case M.lookup def exprs of
         Just x -> do
           typedTopEx <- typeCheckTopExpr env' x
           env'' <- addToTyEnv env' [(def, polyTy (hmTyOf typedTopEx))]
           return (env'', M.insert def typedTopEx acc)
         Nothing -> error "Any name in the order list should be in the map of\
                         \ top level expressions."
    ) (env, M.empty) order

checkSeqInProcs :: HMTyEnv
                -> M.Map Text S1TopProc
                -> CompileErrM ( M.Map Text S2TopProc
                               , M.Map Text S2PEx
                               )
checkSeqInProcs env s1Procs = do
  let (errMap, resMap) = M.mapEither (s1ProcToS2Proc env) s1Procs
  when (not . null $ errMap) $ throwError (ProcSeqErr errMap)
  let s2Tops = M.map fst resMap
      exprs  = M.elems $
                 M.mapWithKey (\k (_, pExs) -> map (first ((k <> "$") <>)) pExs)
                              resMap
      peMap  = M.fromList (concat exprs)
  return (s2Tops, peMap)

stage2 :: Stage1
       -> CompileErrM Stage2
stage2 s1@(Stage1{..}) = do
  env <- processSeqTypes defaultEnv s1Udts

  (env', exprs) <- typeCheckSeq s1Exprs s1ExprOrder env

  -- TODO: Change name of checkSeqInProcs since it does more than just seq type
  -- checking.
  (procs, procExprs) <- checkSeqInProcs env' s1Procs

  return Stage2
           { s1Data   = s1
           , s2Exprs  = exprs
           , s2ProcExprs = procExprs
           , s2Procs  = procs
           }

