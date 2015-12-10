module Jael.Conc.TyCk.S2 where

import qualified Data.Functor.Foldable as F
import qualified Data.Map as M
import           Jael.Conc.Proc
import           Jael.Seq.Env
import           Jael.Seq.TI.S2
import           Jael.Seq.Types

data ProcSeqErr = PSEInferenceErr S2TypeErr
                | PSERedefinedName [Text]
                deriving (Eq, Show)

type ProcSeqErrM = Either ProcSeqErr

type SeqDefEnv = M.Map Text S1Ty

-- typeInf :: HMTyEnv -> S1Ex -> Either S2TypeErr S2TyEx

s1ProcToS2Proc :: HMTyEnv
               -> S1TopProc
               -> ProcSeqErrM (S2TopProc, [(Text, ProcExpr)])
s1ProcToS2Proc env (TopProc pArgs p) = do
  let seqArgs = foldr
                (\(n, x) acc -> case x of
                                     TorSTy t -> (n, t):acc
                                     _        -> acc
                ) [] pArgs
  hmEnv <- case addToEnv env (map (second $ polyTy . hmTyOf) seqArgs) of
                Left ns -> throwError $ PSERedefinedName ns
                Right env' -> return env'

  let alg :: S1ProcF (ProcSeqErrM (S2Proc, [(Text, ProcExpr)]))
                   -> ProcSeqErrM (S2Proc, [(Text, ProcExpr)])
      alg (S1PNilF) = return (S2PNil, [])
      alg (S1PGetChanF c1 c2 cnt) = undefined

  (s2p, pExs) <- F.cata alg p
  return undefined

{--
mkSeqEnv :: ConcTyEnv -> SessTyErrM HMTyEnv
mkSeqEnv (ConcTyEnv{cteBase=bEnv, cteSeq=sEnv}) =
  case addToEnv sEnv $ M.toList $ M.map polyTy (M.map snd bEnv) of
       Left errs -> throwError $ DuplicateSeqEnvItem errs
       Right env -> return env

seqTyInf :: ConcTyEnv -> S1Ex -> SessTyErrM HMTy
seqTyInf env expr = do
  seqEnv <- mkSeqEnv env
  case typeInf seqEnv expr of
       Left e -> throwError $ SeqErrs [e]
       Right typedEx -> return (hmTyOf typedEx)

seqTyCk :: ConcTyEnv -> S1Ex -> HMTy -> SessTyErrM ()
seqTyCk env expr ty = do
  seqEnv <- mkSeqEnv env
  case typeCheck seqEnv expr ty of
       Left e -> throwError $ SeqErrs [e]
       Right _ -> return ()
--}

