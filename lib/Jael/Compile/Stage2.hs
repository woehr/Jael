{-# Language RecordWildCards #-}

module Jael.Compile.Stage2 where

import qualified Data.Map as M
import           Jael.Compile.Common
import           Jael.Seq.Env
import           Jael.Seq.CG_AST
import           Jael.Seq.HM_AST
import           Jael.Seq.Types
import           Jael.Seq.UserDefinedType

addToTyEnv :: TyEnv -> [(Text, PolyTy)] -> CompileErrM TyEnv
addToTyEnv env xs = either (throwError . DupDef) return (addToEnv env xs)

processSeqTypes :: TyEnv
                -> M.Map Text UserDefinedType
                -> CompileErrM TyEnv
processSeqTypes env udts =
  let udtItems = M.mapWithKey (curry seqEnvItems) udts
   in addToTyEnv env (concat . M.elems $ udtItems)

typeCheckTopExpr :: TyEnv
                 -> TopExpr CGEx GramTy
                 -> CompileErrM (TopExpr TypedEx Ty)
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

typeCheckSeq :: M.Map Text (TopExpr CGEx GramTy)
             -> [Text]
             -> TyEnv
             -> CompileErrM (TyEnv, M.Map Text (TopExpr TypedEx Ty))
typeCheckSeq exprs order env =
  foldM (\(env', acc) def ->
    case M.lookup def exprs of
         Just x -> do
           typedTopEx <- typeCheckTopExpr env' x
           env'' <- addToTyEnv env' [(def, polyTy (tyOf typedTopEx))]
           return (env'', M.insert def typedTopEx acc)
         Nothing -> error "Any name in the order list should be in the map of\
                         \ top level expressions."
    ) (env, M.empty) order

stage2 :: Stage1
       -> CompileErrM Stage2
stage2 s1@(Stage1{..}) = do
  env <- processSeqTypes defaultEnv udts

  -- The TopExpr map is returned with CG types and expr replaced by HM ones
  (env', hmTopExpr) <- typeCheckSeq exprs exprOrder env

  -- Using the original expressions and the newly type annotated tree we can
  -- solve constraints on builtin types that have them.
  -- TODO

  return Stage2
           { s1Data   = s1
           , s2SeqEnv = env'
           , s2Exprs  = undefined
           }

