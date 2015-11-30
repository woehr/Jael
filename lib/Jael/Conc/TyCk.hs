module Jael.Conc.TyCk where

import qualified Data.Map as M
import qualified Data.Set as S
import Jael.Conc.Env
import Jael.Conc.Proc
import Jael.Conc.Session
import Jael.Seq.CG_AST
import Jael.Seq.Env
import Jael.Seq.Types

type SessTyErrM = Either SessTyErr

data SessTyErr = UnusedResources { unusedLin :: M.Map Channel Session
                                 , unusedSeq :: M.Map Text Ty
                                 }
               | UndefinedChan Channel
               | RedefinedName Text
               | ProtocolMismatch Channel Session
               | TypeMismatch Channel Ty
               | DuplicateSeqEnvItem [Text]
               | SeqErrs [CGTypeErr]
               | UnknownLabel Label
               | CaseLabelMismatch (S.Set Label)
               | NonFreshChan Text
               | PutChanDualNotFound Channel
               | ChannelInterference Channel (S.Set Channel)
               | NonParallelUsage Channel
               | InterferingProcArgs Text (M.Map Channel (S.Set Channel))
               | CaseProcErrs (M.Map Label SessTyErr)
               -- Issue #4
               | CasesUseDifferentLinearResources
               | InsufficientProcArgs Text
               | ProcArgTypeMismatch (S.Set Text)
               | FordwardedChansNotDual Channel Channel
               | AttemptedChannelIgnore Channel Session
               | RecVarUnfoldInRecProc Channel
               -- The error when an inductive session is passed to a recursive
               -- process without the induction session definition in the
               -- primary position of the session type.
               | NonPrimaryIndSessArg Channel
               -- The error for when it can't be determined whether the two ends
               -- of a channel properly implement the left and right
               -- co-inductive rules.
               | IndSessUseReqd Channel
               | IndSessImplReqd Channel
  deriving (Eq, Show)

data RecType = RTUse
             | RTImpl
             deriving (Show)

invertUsage :: RecType -> RecType
invertUsage x = case x of
                     RTUse -> RTImpl
                     RTImpl -> RTUse

toRI :: RecType -> RecImpl
toRI x = case x of
              RTUse -> RIUse
              RTImpl -> RIImpl

-- This function is unsafe as it assumes that the channel name (k) is already in
-- the linear environment. This function replaces the session represented by
-- names until the session is no longer a SCoInd, SVar, or SDualVar
unfoldAlias :: Text -> ConcTyEnv -> ConcTyEnv
unfoldAlias k env@(ConcTyEnv{cteLin=linEnv, cteAlias=alsEnv}) =
  let missingKey = error "Assumed map would contain key but it did not."
      kEnv@(LinEnv{leAliases=a}) = M.findWithDefault missingKey k linEnv
      kEnv' = case leSess $ M.findWithDefault missingKey k linEnv of
        (SVar var) -> Just
          kEnv{leSess=fromMaybe (M.findWithDefault missingKey var alsEnv)
                                (M.lookup var a)}

        (SDualVar var) -> Just
          kEnv{leSess=dual $ M.findWithDefault missingKey var alsEnv}

        _ -> Nothing

   in maybe env (\e -> unfoldAlias k env{cteLin=M.insert k e linEnv}) kEnv'

addIfNotRedefinition :: EnvValue -> ConcTyEnv -> SessTyErrM ConcTyEnv
addIfNotRedefinition v env@(ConcTyEnv {cteLin=lEnv, cteBase=bEnv}) =
  let nameExists n = n `M.member` lEnv || n `M.member` bEnv
  in case v of
       RxdLinear n s
         | nameExists n -> throwError $ RedefinedName n
         | otherwise -> return $ unfoldAlias n $ env{
                          cteLin=M.insert n (newLinEnv s Nothing True) lEnv
                        }

       NewLinear n1 n2 s
         | nameExists n1 -> throwError $ RedefinedName n1
         | nameExists n2 -> throwError $ RedefinedName n2
         | otherwise ->
             return $ addInterferenceUnsafe n1 n2 env{
               cteLin=M.insert n1 (newLinEnv       s  (Just n2) False) $
                      M.insert n2 (newLinEnv (dual s) (Just n1) False) lEnv
             }

       Base n t
         | nameExists n -> throwError $ RedefinedName n
         | otherwise -> return $ env{cteBase=M.insert n (False, t) bEnv}

-- This function modifies the session of c to v. It is called when a session is
-- deconstructed by a get, put, case, or select. In addition to removing the
-- fresh flag, this function ensures that the "recursive implementation" flag is
-- respected. This means that the session is used in a co-recursive process
-- if the flag is RIImpl or not used in a co-recursive process if the flag is
-- RIUse. If the flag is RIUnknown it is set accordingly in both the session
-- being updated and its dual (to be the opposite). It is a compiler error if
-- there is no dual and the flag is RIUnknown since it is initialized to
-- RIUnknown only if there is a dual. If the dual was removed from the
-- environment through use, its dual (this channel) should have had its flag
-- updated.
updateSession :: Channel -> Session -> ConcTyEnv -> SessTyErrM ConcTyEnv
updateSession c v env@(ConcTyEnv {cteLin=linEnv, cteFresh=freshEnv}) =
  case M.lookup c linEnv of
       Just le -> do
         when (null (cteRec env) && leRecImpl le == RIImpl)
              $ throwError $ IndSessUseReqd c
         when ((not . null) (cteRec env) && leRecImpl le == RIUse)
              $ throwError (IndSessImplReqd c)
         -- update the linear environment to reflect new recursive usage flags
         return $ if leRecImpl le == RIUnknown
                   then
                     let dualName =
                           fromMaybe (error "updateSession error: Channel being\
                                            \ updated had leRecImpl set to\
                                            \ RIUnknown but didn't have a dual")
                                     (leDual le)
                         dualEnv =
                           M.findWithDefault
                             (error "updateSession error: Channel being\
                                    \ updated had dual but dual was not\
                                    \ present in the environment.")
                             dualName linEnv
                      in if null (cteRec env)
                            -- not in a recursive environment
                            then unfoldAlias c env{ cteLin=M.insert c le{ leSess=v
                                                                        , leRecImpl=RIUse
                                                                        }
                                                         $ M.insert dualName
                                                                    dualEnv{leRecImpl=RIImpl}
                                                                    linEnv
                                                  , cteFresh=S.delete c freshEnv
                                                  }
                            else unfoldAlias c env{ cteLin=M.insert c le{ leSess=v
                                                                        , leRecImpl=RIImpl
                                                                        }
                                                         $ M.insert dualName
                                                                    dualEnv{leRecImpl=RIUse}
                                                                    linEnv
                                                  , cteFresh=S.delete c freshEnv
                                                  }
                   else unfoldAlias c env{ cteLin=M.insert c le{leSess=v} linEnv
                                         , cteFresh=S.delete c freshEnv}

       Nothing -> error "It is expected that if a channel's session is being\
                       \ updated that it already exists in the environment."

-- Lookup a channel and return its session. Unlike lookupChanUnfold, this
-- function does not unfold inductive sessions or error when looking up a
-- session in a co-recursive process context. This makes it suitable for the
-- forwarding process to use (which is the only type of process that can make
-- use of a recursive session without unfolding). See last paragraph of section
-- three of "Corecursion and non-divergence in session typed processes" by
-- Toninho et al.
lookupChan :: Channel -> ConcTyEnv -> SessTyErrM Session
lookupChan = lookupChanHelper False

-- Get the session associated with channel c
-- Note that this function is for the purpose of making use of the channel,
-- thus this returns corecursive sessions that have unfolded variables.
lookupChanUnfold :: Channel -> ConcTyEnv -> SessTyErrM Session
lookupChanUnfold = lookupChanHelper True

lookupChanHelper :: Bool -> Channel -> ConcTyEnv -> SessTyErrM Session
lookupChanHelper bUnfold c env =
  case M.lookup c (cteLin env) of
       Just (LinEnv{leSess=s, leConcCtx=True}) ->
         -- Any inductive session used within a recursive
         -- process is limited in its use up to the induction
         -- variable. This is an important aspect of the nu right
         -- rule. See "Corecursion and non-divergence in session
         -- typed processes" by Toninho et al.
         case (bUnfold, not . null $ cteRec env, s) of
              -- reached the recursion variable and we want to unfold in a
              -- recursive process
              (True, True, SCoInd _ _) -> throwError $ RecVarUnfoldInRecProc c
              -- The only case where we may have to call unfoldSession, we want
              -- to unfold and are not in a recursive process
              (True, False, _) -> return $ unfoldSession s
              -- All other cases can just return the session
              _ -> return s

       Just (LinEnv{leConcCtx=False}) -> throwError $ NonParallelUsage c
       _ -> throwError $ UndefinedChan c

-- Get the session associated with channel c, only if it is a fresh channel
lookupFreshChan :: Channel -> ConcTyEnv -> SessTyErrM Session
lookupFreshChan c (ConcTyEnv{cteLin=linEnv, cteFresh=freshEnv}) =
  case (M.lookup c linEnv, c `S.member` freshEnv) of
       (Just (LinEnv{leSess=s}), True) -> return s
       (Nothing, _) -> throwError $ UndefinedChan c
       (Just _, False) -> throwError $ NonFreshChan c

mkSeqEnv :: ConcTyEnv -> SessTyErrM TyEnv
mkSeqEnv (ConcTyEnv{cteBase=bEnv, cteSeq=sEnv}) =
  case addToEnv sEnv $ M.toList $ M.map polyTy (M.map snd bEnv) of
       Left errs -> throwError $ DuplicateSeqEnvItem errs
       Right env -> return env

-- Separate the arguments of a TopProc into linear sessions and base types
separateArgs :: [(Text, TyOrSess)] -> ([(Channel, Session)], [(Text, Ty)])
separateArgs = foldr
  (\(n, x) (ss, ts) -> case x of
                            TorSTy t   -> (ss, (n,t):ts)
                            TorSSess s -> ((n,s):ss, ts)
  ) ([],[])

envErrors :: ConcTyEnv -> SessTyErrM ()
envErrors (ConcTyEnv{cteLin=linEnv, cteBase=baseEnv}) =
  let uLin = M.filter (/= SEnd) (M.map leSess linEnv)
      uSeq = M.map snd . M.filter (not . fst) $ baseEnv
   in when ((not . null) uLin || (not . null) uSeq)
           (throwError UnusedResources{unusedLin = uLin, unusedSeq = uSeq})

markSeqUsed :: S.Set Text -> ConcTyEnv -> ConcTyEnv
markSeqUsed vars env@(ConcTyEnv{cteBase=baseEnv}) =
  env{cteBase=foldr (M.adjust (\(_,t)->(True,t))) baseEnv vars}

-- Mark a linear channel as "used". This means the channel is removed from the
-- environment if eligible. If it has a dual in the environment that needs to
-- "implement" behaviour, mark it as such.
consumeLinear :: Maybe RecType -> Channel -> ConcTyEnv -> SessTyErrM ConcTyEnv
consumeLinear mRecType c env@(ConcTyEnv{cteLin=linEnv}) =
  let cEnv@(LinEnv{leRecImpl=cRec}) =
        M.findWithDefault (error $ "Expected " <> show c <> " to be in the env")
                          c linEnv
   in case (mRecType, cRec) of
           (Just RTUse, RIImpl) -> throwError $ IndSessImplReqd c
           (Just RTImpl, RIUse) -> throwError $ IndSessUseReqd c
           (_, RIUnknown) ->
             let d = fromMaybe (error $ "Expected " <> show c <> " to have a\
                                        \ dual in this situation")
                               (leDual cEnv)
                 dEnv@(LinEnv{leRecImpl=dRec}) =
                   M.findWithDefault (error $ "Expected the dual of " <> show c
                                            <> " to have its dual " <> show d
                                            <> " in the linear env")
                                     d linEnv
              in if dRec /= RIUnknown
                    then error "Expect an RIUnknown channel and its dual to\
                               \ always both have RIUnknown."
                    else case mRecType of
                              Just recType ->
                                return $
                                  env{cteLin=
                                    M.insert d dEnv{leRecImpl=
                                                    toRI . invertUsage $ recType}
                                    $ M.delete c linEnv
                                    }
                              -- There has to be a dual because of RIUnknown.
                              -- We need to set that dual to the opposite usage
                              -- of whatever context we're currently in
                              Nothing -> error "unimplemented. not sure this\
                                              \ case can be hit ..."
           _ -> return env{cteLin=M.delete c linEnv}

-- Take an old env and an updated env and return the used and unused parts of
-- the updated one.
splitEnvs :: ConcTyEnv -> ConcTyEnv -> (ConcTyEnv, ConcTyEnv)
splitEnvs origEnv newEnv@(ConcTyEnv{cteLin=le, cteBase=be}) = do
  let (unused, used) =
        -- This partitioning works if a resource is deleted from newEnv (e.g.,
        -- session is sent over a channel) and if a resource is used (i.e., its
        -- session becomes SEnd). This is important because some operations
        -- update the env by deleting the channel name while others destructure
        -- the session until it hits the SEnd case.
        M.partitionWithKey (\k v -> case M.lookup k (cteLin origEnv) of
                                         -- We use == here instead of coIndEq.
                                         -- I expect == to work since there is
                                         -- no reason for the recursion variable
                                         -- to change within the environment.
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

mkConcEnv :: [(Text, TyOrSess)]
          -> TyEnv
          -> M.Map Text Session
          -> M.Map Text [(Text, TyOrSess)]
          -> ConcTyEnv
mkConcEnv as sEnv sessNames namedProcs =
  -- Separate arguments into linear and base types
  let (ls, bs) = separateArgs as
      -- Do some processing of the top level sessions so type checking can match on
      -- what we expect the session to be. The updateSession function does the same
      -- thing if the session after unfolding is SCoInd, SVar, or SDualVar
      env = emptyEnv
              { cteLin   = M.map (\s->newLinEnv s Nothing True) (M.fromList ls)
              , cteBase  = M.fromList $ map (\(n,t)->(n,(False,t))) bs
              , cteSeq   = sEnv
              , cteAlias = sessNames
              , cteProcs = namedProcs
              }
   in foldr (unfoldAlias . fst) env ls

tyCheckTopProc :: TyEnv
               -> M.Map Text Session
               -> M.Map Text [(Text, TyOrSess)]
               -> TopProc
               -> Maybe SessTyErr
tyCheckTopProc sEnv sessNames namedProcs (TopProc as p) =
   case tyCkProc (mkConcEnv as sEnv sessNames namedProcs) p of
        Left err -> Just err
        Right env' -> either Just (const Nothing) $ envErrors env'

-- Type checks a process. Returns either a type checking error or an updated
-- environment
tyCkProc :: ConcTyEnv -> Proc -> SessTyErrM ConcTyEnv
tyCkProc env (PGetChan c name p) = do
  -- Get the session the channel c is suppose to implement
  sess <- lookupChanUnfold c env
  -- Check that the session implements a "get", update the session in the
  -- environment, and introduce the new name
  env' <- case sess of
               SGetSess v s -> do
                  env'  <- addIfNotRedefinition (RxdLinear name v) env
                  updateSession c s env'
               _ -> throwError $ ProtocolMismatch c sess
  tyCkProc env' p

tyCkProc env (PGetVal c name p) = do
  -- Get the session the channel c is suppose to implement
  sess <- lookupChanUnfold c env
  -- Check that the session implements a "get", update the session in the
  -- environment, and introduce the new name
  env' <- case sess of
               SGetTy   v s -> do
                  env'  <- addIfNotRedefinition (Base name v) env
                  -- c has to already be in the environment so an insert
                  -- replaces the old session with the updated one
                  updateSession c s env'
               _ -> throwError $ ProtocolMismatch c sess
  tyCkProc env' p

tyCkProc env (PGetIgn c p) = do
  sess <- lookupChanUnfold c env
  env' <- case sess of
               SGetTy _ s -> updateSession c s env
               SGetSess v _ -> throwError $ AttemptedChannelIgnore c v
               _ -> throwError $ ProtocolMismatch c sess
  tyCkProc env' p

tyCkProc env@(ConcTyEnv{cteLin=lEnv}) (PPutChan c putChan pCont) = do
  sess <- lookupChanUnfold c env
  env' <- case sess of
               SPutSess v sCont -> do
                 putSess <- lookupFreshChan putChan env
                 if not (putSess `coIndEq` v)
                    then throwError $ ProtocolMismatch c putSess
                    else
                      -- putChan must be fresh so it must also have a dual
                      -- That dual, and the continuation session of c (sCont)
                      -- must be used in parallel according to the typing rules
                      -- to prevent interference.
                      let putChanEnv = M.findWithDefault
                                         (error "putChan must be in the env or\
                                               \ we would have errored prior.")
                                         putChan lEnv
                          putChanDual = fromMaybe (error "putChan must have a\
                                                        \ dual since it must\
                                                        \ be fresh.")
                                                  (leDual putChanEnv)
                          -- What might not necessarily be true is whether
                          -- the dual channel is actually in the current
                          -- environment. It could have been used in some other
                          -- process prior to the placement of putChan on c
                       in do
                          unless (putChanDual `M.member` lEnv)
                                 $ throwError $ PutChanDualNotFound putChanDual
                          -- Remove the session placed on the channel from the
                          -- environment; update the type of the session;
                          -- mark the session as no longer in a concurrent
                          -- context to satisfy rule T(circle cross)
                          updateSession c sCont (addInterferenceUnsafe putChanDual c env)
                            >>= consumeLinear (Just RTUse) putChan
                            >>= (\e -> return e{cteLin=M.adjust
                                                 (\le -> le{leConcCtx=False})
                                                    c
                                                    (cteLin e)
                                                }
                                )
               _ -> throwError $ ProtocolMismatch c sess
  tyCkProc env' pCont

tyCkProc env (PPutVal c putExpr pCont) = do
  sess <- lookupChanUnfold c env
  env' <- case sess of
               SPutTy v sCont ->
                 case seqTyCk env putExpr v of
                      -- Better error reporting
                      Left (SeqErrs [CGTypeMismatch inferred]) -> throwError $ TypeMismatch c inferred
                      Right _ -> updateSession c sCont $ markSeqUsed (freeVars putExpr) env
                      Left x -> Left x
               _ -> throwError $ ProtocolMismatch c sess
  tyCkProc env' pCont

tyCkProc env (PSel chan lab pCont) = do
  sess <- lookupChanUnfold chan env
  env' <- case sess of
               SSelect ls -> case lookup lab ls of
                                  Just s -> updateSession chan s env
                                  Nothing -> throwError $ UnknownLabel lab
               _ -> throwError $ ProtocolMismatch chan sess
  tyCkProc env' pCont

tyCkProc env (PCase chan cases) = do
  sess <- lookupChanUnfold chan env
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
                      (\label proc -> updateSession chan
                        (fromMaybe (error "Should not happen because we check\
                                         \ beforehand that the case and session\
                                         \ labels match.")
                                   (lookup label ls)
                        ) env >>= flip tyCkProc proc
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
                 let retEnv = fst . snd $
                      case M.toList splitEs of
                           x:_ -> x
                           []  -> error "Should not happen because the grammar\
                                       \ ensures cases exist. Previous errors\
                                       \ that could cause splitEs to be empty\
                                       \ would have errored out prior to this."
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
  ty <- seqTyInf env expr
  env' <- addIfNotRedefinition (Base name ty) env
  tyCkProc env' pCont

tyCkProc env (PNewChan n1 n2 sTy pCont) =
      addIfNotRedefinition (NewLinear n1 n2 sTy) env
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
      envErrors usedEnv
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
                              e'{cteLin=
                                   M.insert
                                     c
                                     le{leIntSet=
                                          intSet `S.union` S.delete c dualChans}
                                     l
                                }
                            Nothing -> e'
                     ) unusedEnv dualChans
    ) env' ps

tyCkProc env (PCoRec n is p) = do
  -- Determine the types of the arguments to the corecursive process
  let varNames = map fst is
  let argVals  = map snd is
  argTypes <- mapM
    (\ce ->
       case ce of
            Left c ->
              case M.lookup c $ cteLin env of
                   Just cEnv ->
                     let s = leSess cEnv
                      in case (isInductiveSession s, s) of
                              (True, SCoInd _ _) -> return . TorSSess $ s
                              (True, _) -> throwError $ NonPrimaryIndSessArg c
                              _ -> return . TorSSess $ s
                   Nothing -> throwError $ UndefinedChan c
            Right e -> liftM TorSTy $ seqTyInf env e
    ) argVals
  let coRecSig = zip varNames argTypes
  -- Check for errors in the arguments passed to the corecursive process
  checkProcArgErrs env n argVals coRecSig

  -- Consume recursive process arguments from the environment. Do this first
  -- so these errors are reported before bothering to type check the recursive
  -- process.
  retEnv <- updateEnvFromRecArgs env argVals

  -- create a new environment for the corecursive process
  let coRecEnv = (mkConcEnv coRecSig
                            (cteSeq   env)
                            (cteAlias env)
                            (cteProcs env)
                 ){cteRec=M.insert n coRecSig (cteRec env)}

  -- The type of inductive sessions in coRecSig are left unfolded for the type
  -- checking of recursive process variables, but so they can be used in the
  -- process itself we need to unfold them in the environment. This is because
  -- any inductive session is not allowed to be unfolded when used in a
  -- co-recursive process definition. All sessions should also be marked as
  -- implementing inductive behaviour.
  let coRecEnv' = coRecEnv{cteLin=M.map (
                            \le@(LinEnv{leSess=s}) -> le{leSess=unfoldSession s, leRecImpl=RIImpl}
                           ) (cteLin coRecEnv)
                          }
  coRecResidualEnv <- tyCkProc coRecEnv' p

  -- Type checking the co-recursive process should consume everything that was
  -- in its environment
  envErrors coRecResidualEnv

  return retEnv

tyCkProc env (PNamed n as) =
  -- Is what we're trying to type a recursion variable or a named process?
  case M.lookup n (cteRec env) of
       Just sig -> typeRecursionVar env n as sig
       Nothing -> typeNamedProc env n as

tyCkProc env (PFwd c1 c2) = do
  s1 <- lookupChan c1 env
  s2 <- lookupChan c2 env
  unless (s1 `isDual` s2)
    $ throwError $ FordwardedChansNotDual c1 c2
  consumeLinear Nothing c1 env >>= consumeLinear Nothing c2

tyCkProc env PNil = return env

-- Helper functions to keep things a bit cleaner

updateEnvFromProcArgs :: ConcTyEnv -> [ChanEx] -> SessTyErrM ConcTyEnv
updateEnvFromProcArgs =
  foldM (
    \acc ce ->
      case ce of
           Left c -> consumeLinear (Just RTUse) c acc
           Right e -> return $ markSeqUsed (freeVars e) acc
    )

updateEnvFromRecArgs :: ConcTyEnv -> [ChanEx] -> SessTyErrM ConcTyEnv
updateEnvFromRecArgs =
  foldM (
    \acc ce ->
      case ce of
           Left c -> consumeLinear (Just RTImpl) c acc
           Right e -> return $ markSeqUsed (freeVars e) acc
    )

seqTyInf :: ConcTyEnv -> CGEx -> SessTyErrM Ty
seqTyInf env expr = do
  seqEnv <- mkSeqEnv env
  case typeInf seqEnv expr of
       Left e -> throwError $ SeqErrs [e]
       Right typedEx -> return (tyOf typedEx)

seqTyCk :: ConcTyEnv -> CGEx -> Ty -> SessTyErrM ()
seqTyCk env expr ty = do
  seqEnv <- mkSeqEnv env
  case typeCheck seqEnv expr ty of
       Left e -> throwError $ SeqErrs [e]
       Right _ -> return ()

-- The current env, the env in which the recursive definition was made, the
-- recursion variable, and the parameters to the recursive call
typeRecursionVar :: ConcTyEnv
                 -> Text
                 -> [ChanEx]
                 -> [(Text, TyOrSess)]
                 -> SessTyErrM ConcTyEnv
typeRecursionVar env n as sig = do
  checkProcArgErrs env n as sig
  updateEnvFromRecArgs env as

typeNamedProc :: ConcTyEnv -> Text -> [ChanEx] -> SessTyErrM ConcTyEnv
typeNamedProc env n as = do
  let procSig = fromMaybe (error "Previous analysis should have identified\
                                \ unknown names.")
                          (M.lookup n (cteProcs env))

  checkProcArgErrs env n as procSig
  -- Should be good at this point. Update the environment to reflect what has
  -- been used/consumed by the named proc
  updateEnvFromProcArgs env as

checkProcArgErrs :: ConcTyEnv
                 -> Text
                 -> [ChanEx]
                 -> [(Text, TyOrSess)]
                 -> SessTyErrM ()
checkProcArgErrs env n as sig = do
  when (length sig /= length as)
       $ throwError $ InsufficientProcArgs n

  argTypeMismatches <-
    foldr (\((argName, argType), argVal) m -> do
      acc <- m
      case (argType, argVal) of
           -- Make sure the type of e matches t
           (TorSTy t  , Right expr) ->
             case seqTyCk env expr t of
                  -- Better error reporting, first case reports when a type is
                  -- infered but it is incorrect, second reports other inference
                  -- errors.
                  Left (SeqErrs [CGTypeMismatch _]) -> return $ argName:acc
                  Left  e -> throwError e
                  Right _ -> return acc
           -- Make sure the type of c matches s
           (TorSSess s,  Left c) -> case M.lookup c (cteLin env) of
                                         Just le -> if s `coIndEq` leSess le
                                                       then return acc
                                                       else return $ argName:acc
                                         Nothing -> throwError $ UndefinedChan c
           -- Expected either type or session but got the opposite
           _ -> return $ argName:acc
      ) (return []) (zip sig as)
  unless (null argTypeMismatches)
         $ throwError $ ProcArgTypeMismatch (S.fromList argTypeMismatches)

  -- Make sure that all the channels passed to a process are non-interfering
  let chanArgSet = S.fromList $ lefts as
  let interferingArgMap = foldr
        (\c m ->
          let cEnv = M.findWithDefault
                       (error "Should error before this point if the channel is\
                             \ not in the linear environemnt.")
                       c (cteLin env)
              interferingChans = leIntSet cEnv `S.intersection` chanArgSet
           in if null interferingChans
                 then m
                 else M.insert c interferingChans m
        ) M.empty chanArgSet
  unless (null interferingArgMap)
         $ throwError $ InterferingProcArgs n interferingArgMap

