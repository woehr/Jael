module Jael.Conc.TyCk.S2 where

import qualified Data.Functor.Foldable as F
import qualified Data.Map as M
import           Jael.Conc.Proc
import           Jael.Seq.Env
import           Jael.Seq.TI.S2
import           Jael.Seq.Types

data S2ProcSeqErr = PSEInferenceErr S2TypeErr
                  | PSERedefinedName [Text]
                  deriving (Eq, Show)

type S2ProcSeqErrM = Either [S2ProcSeqErr]

data TypeProcSeqState = TypeProcSeqState
                        { tpssEnv    :: HMTyEnv
                        , tpssCount  :: Integer
                        } deriving (Show)

type TypeProcSeqM = WriterT ([S2ProcSeqErr], [(Text, S2PEx)])
                    (StateT TypeProcSeqState Identity)

type SeqDefEnv = M.Map Text S1Ty

-- typeInf :: HMTyEnv -> S1Ex -> Either S2TypeErr S2TyEx

typeAndExtractSeq :: HMTyEnv
                  -> S1Proc
                  -> S2ProcSeqErrM (S2Proc, [(Text, S2PEx)])
typeAndExtractSeq env s1 =
  let alg :: S1ProcF (TypeProcSeqM S2Proc)
                   -> TypeProcSeqM S2Proc
      alg (S1PGetChanF c1 c2 p)   = (liftM $ S2PGetChan c1 c2) p
      alg (S1PGetValF c n p)      = (liftM $ S2PGetVal c n) p
      alg (S1PGetIgnF c p)        = (liftM $ S2PGetIgn c) p
      alg (S1PPutChanF c1 c2 p)   = (liftM $ S2PPutChan c1 c2) p
      alg (S1PNewChanF n1 n2 s p) = (liftM $ S2PNewChan n1 n2 s) p
      alg (S1PParF ps)            = (liftM S2PPar) (sequence ps)
      alg (S1PCaseF c ps)         = (liftM $ S2PCase c)
                                      (liftM (zip $ map fst ps)
                                        (sequence $ map snd ps))
      alg (S1PSelF c l p)         = (liftM $ S2PSel c l) p
      alg (S1PFwdF c1 c2)         = return $ S2PFwd c1 c2
      alg (S1PNilF)               = return S2PNil

      alg (S1PPutValF c e p) = undefined
      alg (S1PNewValF n e p) = undefined
      alg (S1PCoRecF n as p) = undefined
      alg (S1PNamedF n as)   = undefined

      initState = TypeProcSeqState { tpssEnv = env
                                   , tpssCount = 0}

      (s2, (errs, pExs)) = runIdentity (evalStateT
                                         (runWriterT $ F.cata alg s1)
                                         initState)
   in if null errs
         then return (s2, pExs)
         else throwError errs

inferS2Args :: [(Text, TyOrSess S1Ty)] -> S2Proc -> M.Map Text S2PEx -> S2ProcSeqErrM S2TopProc
inferS2Args s1Args s2p mExpr = undefined

s1ProcToS2Proc :: HMTyEnv
               -> S1TopProc
               -> S2ProcSeqErrM (S2TopProc, [(Text, S2PEx)])
s1ProcToS2Proc env (TopProc pArgs p) = do
  let seqArgs = foldr
                (\(n, x) acc -> case x of
                                     TorSTy t -> (n, t):acc
                                     _        -> acc
                ) [] pArgs
  hmEnv <- case addToEnv env (map (second $ polyTy . hmTyOf) seqArgs) of
                Left ns -> throwError [PSERedefinedName ns]
                Right env' -> return env'

  (s2p, pExs) <- typeAndExtractSeq hmEnv p
  let pExMap = M.fromList pExs
  liftM (flip (,) pExs) $ inferS2Args pArgs s2p pExMap

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

