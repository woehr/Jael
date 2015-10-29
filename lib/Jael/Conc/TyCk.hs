{-# Language NoImplicitPrelude #-}

module Jael.Conc.TyCk where

import ClassyPrelude hiding (Chan, Foldable)
import Control.Monad.Except hiding (foldM, mapM_)
import qualified Data.List as L (head)
import qualified Data.Map as M
import qualified Data.Set as S
import Jael.Util
import Jael.Conc.Env
import Jael.Conc.Proc
import Jael.Conc.Session
import Jael.Seq.AST
import Jael.Seq.Env
import Jael.Seq.Expr (freeVars)
import Jael.Seq.TI
import Jael.Seq.Types

type SessTyErrM = Either SessTyErr

data SessTyErr = UnusedResources { unusedLin :: M.Map Chan Session
                                 , unusedSeq :: M.Map Text Ty
                                 }
               | UndefinedChan Chan
               | RedefinedName Text
               | ProtocolMismatch Chan Session
               | TypeMismatch Chan Ty
               | DuplicateSeqEnvItem [Text]
               | SeqTIErrs [Text]
               | UnknownLabel Text
               | CaseLabelMismatch (S.Set Text)
               | NonFreshChan Text
               | ChannelInterference Text (S.Set Text)
               | NonParallelUsage Text
               | InterferringProcArgs Text (S.Set Text)
               | CaseProcErrs (M.Map Label SessTyErr)
               -- TODO: Return some information about which cases are offending
               | CasesUseDifferentLinearResources
               | InsufficientProcArgs Text
               | ProcArgTypeMismatch (S.Set Text)
  deriving (Eq, Show)

missingKey :: a
missingKey = error "Assumed map would contain key but it did not."

-- This function is unsafe as it assumes that the channel name (k) is already in
-- the linear environment. This function replaces the session represented by
-- names until the session is no longer a SCoInd, SVar, or SDualVar
unfoldSession :: Text -> ConcTyEnv -> ConcTyEnv
unfoldSession k env@(ConcTyEnv{cteLin=linEnv, cteAlias=alsEnv}) =
  let kEnv@(LinEnv{leAliases=a}) = M.findWithDefault missingKey k linEnv
      kEnv' = case leSess $ M.findWithDefault missingKey k linEnv of
        (SCoInd var s) -> Just kEnv{ leSess=s
                                   , leAliases=M.insert var s alsEnv}

        (SVar var) -> Just
          kEnv{leSess=fromMaybe (M.findWithDefault missingKey var alsEnv)
                                (M.lookup var a)}

        (SDualVar var) ->
          if var `M.member` a
             then error "A recursion variable should not be dualed."
             else Just kEnv{leSess=dual $ M.findWithDefault missingKey var alsEnv}

        _ -> Nothing

   in maybe env (\e -> unfoldSession k env{cteLin=M.insert k e linEnv}) kEnv'

addOrNameErr :: Text -> a -> M.Map Text a -> SessTyErrM (M.Map Text a)
addOrNameErr k v m =
  case addIfUnique (k, v) m of
       Just m' -> return m'
       Nothing -> throwError $ RedefinedName k

addIfNotRedefinition :: EnvValue -> ConcTyEnv -> SessTyErrM ConcTyEnv
addIfNotRedefinition v env@(ConcTyEnv {cteLin=lEnv, cteBase=bEnv}) =
 case v of
      Linear n s -> addOrNameErr n (newLinEnv s Nothing) lEnv
               >>= (\lEnv' -> return $ unfoldSession n env{cteLin=lEnv'})
      DualLinear n1 n2 s -> addOrNameErr n1 (newLinEnv s (Just n2)) lEnv
                        >>= addOrNameErr n2 (newLinEnv (dual s) (Just n1))
                        >>= (\lEnv' -> return $ addInterferenceUnsafe n1 n2 env{cteLin=lEnv'})
      Base n t -> addOrNameErr n (False, t) bEnv
              >>= (\bEnv' -> return $ env{cteBase=bEnv'})

updateSession :: Chan -> Session -> ConcTyEnv -> ConcTyEnv
updateSession c v env@(ConcTyEnv {cteLin=linEnv, cteFresh=freshEnv}) =
  case M.lookup c linEnv of
       Just le -> unfoldSession c env{ cteLin=M.insert c le{leSess=v} linEnv
                                     , cteFresh=S.delete c freshEnv}
       Nothing -> error "It is expected that if a channel's session is being\
                       \ updated that it already exists in the environment."

-- Get the session associated with channel c
lookupChan :: Chan -> ConcTyEnv -> SessTyErrM Session
lookupChan c (ConcTyEnv{cteLin=linEnv}) =
  case M.lookup c linEnv of
       Just (LinEnv{leSess=s, leConcCtx=True }) -> return s
       Just (LinEnv{          leConcCtx=False}) -> throwError $ NonParallelUsage c
       _ -> throwError $ UndefinedChan c

-- Get the session associated with channel c, only if it is a fresh channel
lookupFreshChan :: Chan -> ConcTyEnv -> SessTyErrM Session
lookupFreshChan c (ConcTyEnv{cteLin=linEnv, cteFresh=freshEnv}) =
  case (M.lookup c linEnv, c `S.member` freshEnv) of
       (Just (LinEnv{leSess=s}), True) -> return s
       (Nothing, _) -> throwError $ UndefinedChan c
       (Just _, False) -> throwError $ NonFreshChan c

mkSeqEnv :: ConcTyEnv -> SessTyErrM TyEnv
mkSeqEnv (ConcTyEnv{cteBase=bEnv, cteSeq=sEnv}) =
  case addToEnv sEnv $ M.toList $ M.map polyTy (map snd bEnv) of
       Left errs -> throwError $ DuplicateSeqEnvItem errs
       Right env -> return env

-- Separate the arguments of a TopProc into linear sessions and base types
separateArgs :: [(Text, TyOrSess)] -> ([(Chan, Session)], [(Text, Ty)])
separateArgs = foldr
  (\(n, x) (ss, ts) -> case x of
                            TorSTy t   -> (ss, (n,t):ts)
                            TorSSess s -> ((n,s):ss, ts)
  ) ([],[])

envErrors :: ConcTyEnv -> SessTyErrM ConcTyEnv
envErrors env@(ConcTyEnv{cteLin=linEnv, cteBase=baseEnv}) =
  let uLin = M.filter (/= SEnd) (M.map leSess linEnv)
      uSeq = M.map snd . M.filter (not . fst) $ baseEnv
   in if (not . null) uLin || (not . null) uSeq
         then throwError UnusedResources { unusedLin = uLin, unusedSeq = uSeq}
         else return env

markSeqUsed :: S.Set Text -> ConcTyEnv -> ConcTyEnv
markSeqUsed vars env@(ConcTyEnv{cteBase=baseEnv}) =
  env{cteBase=foldr (M.adjust (\(_,t)->(True,t))) baseEnv vars}

-- Take an old env and an updated env and return the used and unused parts of
-- the updated one.
splitEnvs :: ConcTyEnv -> ConcTyEnv -> (ConcTyEnv, ConcTyEnv)
splitEnvs origEnv newEnv@(ConcTyEnv{cteLin=le, cteBase=be}) = do
  let (unused, used) =
        M.partitionWithKey (\k v -> case M.lookup k (cteLin origEnv) of
                                         Just l -> leSess l == leSess v
                                         Nothing -> False
                           ) le
  -- Linear resources have to be removed from the original
  -- env when used, where as base types don't have to be
  let (origBase, resBase) =
        M.partitionWithKey (\k _ -> k `M.member` cteBase origEnv) be
   in ( newEnv{cteLin=unused, cteBase=origBase}
      , newEnv{cteLin=used, cteBase=resBase}
      )

tyCheckTopProc :: TyEnv
               -> M.Map Text Session
               -> M.Map Text [(Text, TyOrSess)]
               -> TopProc
               -> Maybe SessTyErr
tyCheckTopProc sEnv sessNames namedProcs (TopProc as p) =
  -- Separate arguments into linear and base types
  let (ls, bs) = separateArgs as
  -- Do some processing of the top level sessions so type checking can match on
  -- what we expect the session to be. The updateSession function does the same
  -- thing if the session after unfolding is SCoInd, SVar, or SDualVar
      env = emptyEnv
              { cteLin   = M.map (\s->(newLinEnv s Nothing){leConcCtx=True}) (M.fromList ls)
              , cteBase  = M.fromList $ map (\(n,t)->(n,(False,t))) bs
              , cteSeq   = sEnv
              , cteAlias = sessNames
              , cteProcs = namedProcs
              }
      envOrErr = tyCkProc (foldr (unfoldSession . fst) env ls) p
   in case envOrErr of
           Left err -> Just err
           Right env' -> either Just (const Nothing) $ envErrors env'

-- Type checks a process. Returns either a type checking error or an updated
-- environment
tyCkProc :: ConcTyEnv -> Proc -> SessTyErrM ConcTyEnv
tyCkProc env (PGet c name p) = do
  -- Get the session the channel c is suppose to implement
  sess <- lookupChan c env
  -- Check that the session implements a "get", update the session in the
  -- environment, and introduce the new name
  env' <- case sess of
               SGetTy   v s -> do
                  env'  <- addIfNotRedefinition (Base name v) env
                  -- c has to already be in the environment so an insert replaces
                  -- the old session with the updated one
                  return $ updateSession c s env'
               SGetSess v s -> do
                  env'  <- addIfNotRedefinition (Linear name v) env
                  return $ updateSession c s env'
               _ -> throwError $ ProtocolMismatch c sess
  tyCkProc env' p

tyCkProc env@(ConcTyEnv{cteLin=lEnv}) (PPut c chanOrExpr pCont) = do
  sess <- lookupChan c env
  env' <- case (sess, chanOrExpr) of
               (SPutTy v sCont, Right putExpr) -> do
                  completeSeqEnv <- mkSeqEnv env
                  -- ' is a name guaranteed not to be used in the expression
                  case seqInfer completeSeqEnv (ELet "'" putExpr $ EVar "'") of
                       Left errs -> throwError $ SeqTIErrs errs
                       Right ty -> if ty /= v
                                      then throwError $ TypeMismatch c ty
                                      else return $ updateSession c sCont
                                                  $ markSeqUsed (freeVars putExpr) env
               (SPutSess v sCont, Left putChan) -> do
                  putSess <- lookupFreshChan putChan env
                  if putSess /= v
                     then throwError $ ProtocolMismatch c putSess
                     else
                       -- putChan must be fresh so it must also have a dual
                       -- That dual, and the continuation session of c (sCont)
                       -- must be used in parallel according to the typing rules
                       -- to prevent interference. Specifically, a process in
                       -- parallel could receive the sent channel and use the
                       -- other end of c
                       return $ updateSession c sCont env
                         { cteLin=M.delete putChan lEnv
                         }
               _ -> throwError $ ProtocolMismatch c sess
  tyCkProc env' pCont

tyCkProc env (PSel chan lab pCont) = do
  sess <- lookupChan chan env
  env' <- case sess of
               SSelect ls -> case lookup lab ls of
                                  Just s -> return $ updateSession chan s env
                                  Nothing -> throwError $ UnknownLabel lab
               _ -> throwError $ ProtocolMismatch chan sess
  tyCkProc env' pCont

tyCkProc env (PCase chan cases) = do
  sess <- lookupChan chan env
  case sess of
       SChoice ls ->
         let l1 = (S.fromList $ map fst ls)
             l2 = (S.fromList $ map fst cases)
             -- "Symmetric difference"
             diffLabels = (l1 S.\\ l2) `S.union` (l2 S.\\ l1)
         in if S.size diffLabels /= 0
               then throwError $ CaseLabelMismatch diffLabels
               else do
                 -- Type check each process of the case statement with the
                 -- channel updated to reflect the session in the
                 -- corresponding "choice" session type.
                 let (caseErrMap, residualEnvMap) = M.mapEitherWithKey
                      (\label proc -> flip tyCkProc proc $ updateSession chan
                        (fromMaybe (error "Should not happen because we check\
                                         \ beforehand that the case and session\
                                         \ labels match.")
                                   (lookup label ls)
                        ) env
                      ) (M.fromList cases)
                 -- The first map for each value in splitEs is what remains of
                 -- the original input environment. The second map contains the
                 -- components of the environment that the case used.
                 let splitEs = M.map (splitEnvs env) residualEnvMap
                 -- combine the maps of errors so we report more to the user
                 let allCaseErrs = caseErrMap `M.union`
                      fst (M.mapEither (envErrors . snd) splitEs)
                 when (M.size allCaseErrs /= 0)
                      (throwError $ CaseProcErrs allCaseErrs)
                 -- All cases must use an environment identically otherwise it's
                 -- an error. Consider if it didn't and the case was one of
                 -- several processes being run in parallel: we wouldn't be able
                 -- to determine which of the resulting environments needed to
                 -- be used to type the next concurrent process.
                 -- This line takes one of the unused environments from the case
                 let retEnv = fst . snd . L.head . M.toList $ splitEs
                 -- Make sure that all linear sessions were used in the same way
                 foldM (\e1 e2 ->
                         let lin1 = M.map leSess (cteLin e1)
                             lin2 = M.map leSess (cteLin e2)
                          in if lin1 /= lin2
                                then throwError CasesUseDifferentLinearResources
                                else return e1
                       ) retEnv (M.map fst splitEs)
       _ -> throwError $ ProtocolMismatch chan sess

tyCkProc env (PNewVal name expr pCont) = do
  seqEnv <- mkSeqEnv env
  env' <- case seqInfer seqEnv expr of
               Left errs -> throwError $ SeqTIErrs errs
               Right ty  -> addIfNotRedefinition (Base name ty) env
  tyCkProc env' pCont

tyCkProc env (PNewChan n1 n2 sTy pCont) =
      addIfNotRedefinition (DualLinear n1 n2 sTy) env
  >>= (\e -> return e{cteFresh=S.insert n1 $ S.insert n2 $ cteFresh e})
  >>= flip tyCkProc pCont

tyCkProc env (PPar ps) = do
  -- Mark all channels as now in a concurrent context, this means that newly
  -- introduced channels can now be used
  let env' = env{cteLin=M.map (\le -> le{leConcCtx=True}) (cteLin env)}
  -- Type check each process individually. After checking each, look at the
  -- returned environment to determine which channels were used. Make sure that
  -- the used channels do not interfere with each other
  --finalEnv <- foldM
  foldM
    (\e p -> do
      newEnv <- tyCkProc e p
      -- The first thing we'll consider is that the resulting environment
      -- contains both resources from e and newly introduced resources. Of these
      -- resources, the ones used in e and the ones newly introduced must
      -- respect linearity. The unused resources will be passed on to type the
      -- next process.
      let (unusedEnv, usedEnv) = splitEnvs e newEnv
      _ <- envErrors usedEnv
      -- the lin map of newEnv should be a super-set of e and any sessions of
      -- newEnv that differ from those of e implies the corresponding channels
      -- were used. Of these used channels, we check that none of them were
      -- interferring in e
      -- First, determine which channels were used
      -- We are not interested in the channels introduced in p since they were
      -- already typed when tyCkProc was run on it.
      let usedChans = M.keysSet
           $ M.filter id
           $ M.intersectionWith ((/=) `on` leSess) (cteLin e) (cteLin newEnv)
      -- Then, check for channel interference for each channel used
      mapM_ (\c -> do
              let intSet = leIntSet $ fromMaybe
                    (error "Expected channel to be in the environment since\
                          \ usedChans is derived from environment keys.")
                    (M.lookup c $ cteLin e)
                  intChans = intSet `S.intersection` usedChans
              when (S.size intChans /= 0)
                   (throwError $ ChannelInterference c intChans)
            ) usedChans
      -- Based on the channels used, update newEnv to reflect new constraints
      -- Basically, the set of duals of usedChans now all interfere with each
      -- other. Keep in mind that we don't always know the dual of a channel,
      -- e.g. when we receive a session over a channel or when one is an
      -- argument to a named proc. In the first case, the type system ensures
      -- non-interference by making sure than the dual of a session sent over a
      -- channel is non-interferring with the continuation of the session that
      -- sent it (see the T(circled cross) rule). In the second case, I ensure
      -- that all arguments to a named session are non-interferring and are
      -- being used in a concurrent context (since we have no way of knowing
      -- how the named process uses the channels we can't rely on it to use them
      -- in any specific manner).
      let dualChans = S.fromList $ mapMaybe (\c -> leDual $ M.findWithDefault
                                      (error "Chans are derived from this env\
                                      \ so this can't happen.")
                                      c (cteLin newEnv)
                              ) (S.toList usedChans)
      return $ foldr (\c e'@(ConcTyEnv{cteLin=l}) ->
                       case M.lookup c l of
                            Just le@(LinEnv{leIntSet=intSet}) ->
                              -- record update disaster
                              e'{cteLin=M.insert c le{leIntSet=intSet `S.union` (c `S.delete` dualChans)} l}
                            Nothing -> e'
                     ) unusedEnv dualChans
    ) env' ps

tyCkProc env (PNamed n as) = do
  let procSig = fromMaybe (error "Previous analysis should have identified\
                                \ unknown names.")
                          (M.lookup n (cteProcs env))
  when (length procSig /= length as)
       $ throwError $ InsufficientProcArgs n
  argTypeMismatches <-
    foldr (\((argName, argType), argVal) m -> do
      acc <- m
      case (argType, argVal) of
           -- Make sure the type of e matches t
           (TorSTy t  , Right expr) -> do
             seqEnv <- mkSeqEnv env
             case seqInfer seqEnv expr of
                  Left errs -> throwError $ SeqTIErrs errs
                  Right ty  -> if t == ty
                                  then return acc
                                  else return $ argName:acc
           -- Make sure the type of c matches s
           (TorSSess s,  Left c) -> case M.lookup c (cteLin env) of
                                         Just le -> if s == leSess le
                                                       then return acc
                                                       else return $ argName:acc
                                         Nothing -> throwError $ UndefinedChan c
           -- Expected either type or session but got the opposite
           _ -> return $ argName:acc
      ) (return []) (zip procSig as)
  unless (null argTypeMismatches)
       $ throwError $ ProcArgTypeMismatch (S.fromList argTypeMismatches)
  undefined

tyCkProc env (PCoRec n inits p) = undefined

tyCkProc env PNil = return env

