module Jael.Conc.TyCk.S2 where

import qualified Data.Functor.Foldable as F
import qualified Data.Map as M
import           Jael.Conc.Proc
import           Jael.Conc.Session
import           Jael.Seq.AST
import           Jael.Seq.Env
import           Jael.Seq.TI.S2
import           Jael.Seq.Types

data S2ProcErr = S2PEInferenceErr S2TypeErr
               | S2PERedefinedName [Text]
               | S2PEInvalidSession SessDefErr
               deriving (Eq, Show)

type S2ProcErrM = Either [S2ProcErr]

data S2ProcState = S2ProcState
                     { tpssEnv    :: HMTyEnv
                     , tpssErrs   :: [S2ProcErr]
                     , tpssS2PEx  :: [(Text, S2PEx)]
                     } deriving (Show)

type S2ProcM = State S2ProcState

type SeqDefEnv = M.Map Text S1Ty

addError :: S2ProcErr -> S2ProcM S2Proc
addError e = modify (\s@(S2ProcState{..}) -> s{tpssErrs=e:tpssErrs}) >> return S2PNil

s2ProcAddToEnv :: HMTypable a => [(Text, a)] -> S2ProcM ()
s2ProcAddToEnv xs = do
  hmEnv <- gets tpssEnv
  case addToEnv hmEnv (map (second $ polyTy . hmTyOf) xs) of
       Left dups -> addError (S2PERedefinedName dups) >> return ()
       Right hmEnv' -> modify (\s@(S2ProcState{..}) -> s{tpssEnv=hmEnv'})

checkSeq :: S1Ex -> S2ProcM S2TyEx
checkSeq e = do
  hmEnv <- gets tpssEnv
  case typeInf hmEnv e of
       -- TODO: Fix to not have an ugly error like this.
       Left err -> addError (S2PEInferenceErr err) >> error "Should not hit because an error was added"
       Right tyEx -> return tyEx

parEnv :: [a] -> (a -> S2ProcM b) -> S2ProcM [b]
parEnv as f = do
  oldEnv <- gets tpssEnv
  flip mapM as $ \a -> f a <* modify (\s@(S2ProcState{..}) -> s{tpssEnv=oldEnv})

typeAndExtractSeq :: HMTyEnv
                  -> S1Proc
                  -> S2ProcErrM (S2Proc, [(Text, S2PEx)])
typeAndExtractSeq env s1 =
  let alg :: S1ProcF (S2ProcM S2Proc)
                   -> S2ProcM S2Proc
      alg (S1PGetChanF c1 c2 p)   = liftM (S2PGetChan c1 c2) p
      alg (S1PGetIgnF c p)        = liftM (S2PGetIgn c) p
      alg (S1PPutChanF c1 c2 p)   = liftM (S2PPutChan c1 c2) p
      alg (S1PNewChanF n1 n2 s p) = liftM (S2PNewChan n1 n2 s) p
      alg (S1PSelF c l p)         = liftM (S2PSel c l) p
      alg (S1PFwdF c1 c2)         = return $ S2PFwd c1 c2
      alg (S1PNilF)               = return S2PNil

      alg (S1PGetValF c n p) = s2ProcAddToEnv [(n, HMTyVar "a")] >> liftM (S2PGetVal c n) p

      alg (S1PPutValF c e p) = do
        tyEx <- checkSeq e
        liftM (S2PPutVal c (S2PEx tyEx)) p

      alg (S1PNewValF n e p) = do
        tyEx <- checkSeq e
        s2ProcAddToEnv [(n, tyEx)]
        liftM (S2PNewVal n (S2PEx tyEx)) p

      alg (S1PParF ps)    = liftM S2PPar (parEnv ps id)
      alg (S1PCaseF c ps) = liftM (S2PCase c)
                                  (liftM (zip $ map fst ps)
                                         (parEnv (map snd ps) id))

      alg (S1PCoRecF n as p) = do
        -- Check the sequence arguments to a co-rec process. Return the variable
        -- name and type in the first element of the tuple and the (ChanEx S2PEx)
        -- in the second element.
        xs <- parEnv as $ \(v, ce) ->
              case ce of
                   Left c -> return (Nothing, (v, Left c))
                   Right e -> do
                     tyEx <- checkSeq e
                     return (Just (v, tyEx), (v, Right $ S2PEx tyEx))

        -- Collect the first elements of xs which make up the sequential names
        -- available to p.
        let newEnvItems = catMaybes $ map fst xs

        -- Replace hmEnv with the original with the sequential arguments added
        modify (\s@(S2ProcState{..})->s{tpssEnv=env})
        s2ProcAddToEnv newEnvItems

        liftM (S2PCoRec n $ map snd xs) p

      alg (S1PNamedF n as)   = liftM (S2PNamed n) $ parEnv as $
        either (return . Left) (\e -> checkSeq e >>= return . Right . S2PEx)

      initState = S2ProcState { tpssEnv = env
                              , tpssErrs = []
                              , tpssS2PEx = []
                              }

      (s2, (S2ProcState{tpssS2PEx=pEx, tpssErrs=es})) = runIdentity (runStateT (F.cata alg s1)
                                                                               initState)
   in if null es
         then return (s2, pEx)
         else throwError es

inferS2Args :: [(Text, S1TyOrSess)] -> S2Proc -> M.Map Text S2PEx -> S2ProcErrM S2TopProc
inferS2Args s1Args s2p mExpr = undefined

s1ProcToS2Proc :: HMTyEnv
               -> S1TopProc
               -> S2ProcErrM (S2TopProc, [(Text, S2PEx)])
s1ProcToS2Proc env (TopProc pArgs p) = do
  let seqArgs = foldr
                (\(n, x) acc -> case x of
                                     TorSTy t -> (n, t):acc
                                     _        -> acc
                ) [] pArgs
  hmEnv <- case addToEnv env (map (second $ polyTy . hmTyOf) seqArgs) of
                Left ns -> throwError [S2PERedefinedName ns]
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

