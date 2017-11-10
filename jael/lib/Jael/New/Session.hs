{-# Language DeriveFunctor #-}

module Jael.New.Session where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

import Jael.New.Type

data SessionF a = SGetTyF Type a
                | SPutTyF Type a
                | SGetSessF a a
                | SPutSessF a a
                | SChoiceF [(T.Text, a)]
                | SSelectF [(T.Text, a)]
                | SCoIndF T.Text a
                | SVarF T.Text
                | SDualVarF T.Text
                | SEndF
               deriving (Functor, Show)

---- Takes a combining function and applies it to corresponding labels in the
---- label lists. Returns true if the combining function is true for all
---- corresponding labels and both label lists have the same set of labels.
--combineLabelList :: (a -> a -> Bool) -> [(T.Text, a)] -> [(T.Text, a)] -> Bool
--combineLabelList f xs ys =
--  let m1 = M.fromList xs
--      m2 = M.fromList ys
--      res = M.intersectionWith f m1 m2
--   in M.size m1 == M.size res && and res
--
---- Partial function. Returns the dual of a session. Will error if a recursion
---- variable is dualed. This case should be caught when validating a session.
--dual :: Session -> Session
--dual = dual' S.empty
--
--dual' :: S.Set T.Text -> Session -> Session
--dual' vs (SGetTy   t c) = SPutTy   t (dual' vs c)
--dual' vs (SPutTy   t c) = SGetTy   t (dual' vs c)
--dual' vs (SGetSess s c) = SPutSess s (dual' vs c)
--dual' vs (SPutSess s c) = SGetSess s (dual' vs c)
--dual' vs (SChoice  cs)  = SSelect (map (second $ dual' vs) cs)
--dual' vs (SSelect  cs)  = SChoice (map (second $ dual' vs) cs)
--dual' vs (SCoInd v c)   = SCoInd v (dual' (S.insert v vs) c)
--dual' vs (SVar v)       = if v `S.member` vs
--                              then SVar v
--                              else SDualVar v
--    -- A valid session should not have a "dualed" recursion variable
--dual' vs (SDualVar v)   = if v `S.member` vs
--                             then error "Compiler error. This should not happen\
--                                        \ if the session was validated."
--                             else SVar v
--dual' _ (SEnd) = SEnd
--
--data SessGramState = SessGramState
--                       { sgsRecVars  :: S.Set T.Text
--                       , sgsSessVars :: S.Set T.Text
--                       , sgsDualVars :: S.Set T.Text
--                       } deriving (Show)
--
--type SessGramM = ExceptT SessDefErr (State SessGramState)
--
----gToSession :: GSession -> Either SessDefErr Session
----gToSession s =
----  let (res, (SessGramState{..})) = runState (runExceptT $ gToSession' s)
----                                            (SessGramState S.empty S.empty S.empty)
----   in case res of
----           Left err -> Left err
----           Right sess -> do
----             let unused = sgsRecVars S.\\ sgsSessVars
----             unless (null unused) $ throwError (SDEUnused unused)
----             return sess
--
--addRecVar :: T.Text -> SessGramM ()
--addRecVar v = do
--  recVars <- gets sgsRecVars
--  when (v `S.member` recVars) $ throwError (SDEDupInd v)
--  modify (\s@(SessGramState{..}) -> s{sgsRecVars=S.insert v sgsRecVars})
--
--addSessVar :: T.Text -> SessGramM ()
--addSessVar v = modify (\s@(SessGramState{..}) -> s{sgsSessVars=S.insert v sgsSessVars})
--
--addDualVar :: T.Text -> SessGramM ()
--addDualVar v = do
--  recVars <- gets sgsRecVars
--  when (v `S.member` recVars) $ throwError (SDEDualRec v)
--  modify (\s@(SessGramState{..}) -> s{sgsDualVars=S.insert v sgsDualVars})
--
---- Converts a list of labeled GSession's to Sessions making sure to apply the
---- same initial state to each.
----convertLabeledSessions :: [(T.Text, GSession)] -> SessGramM [(T.Text, Session)]
----convertLabeledSessions ss = do
----  let rep = S.fromList . repeated . map fst $ ss
----  unless (null rep) $ throwError (SDEDupLabels rep)
----  initialState <- get
----  res <- mapM (\(n, g) -> do s <- gToSession' g
----                             -- Check that any co-inductive sessions have their
----                             -- recursion variables used. We need to do this
----                             -- because we can't propagate recursion variable
----                             -- names in the returned state since other branches
----                             -- of choice/select session are allowed to use the
----                             -- same name.
----                             used <- gets sgsSessVars
----                             defd <- gets sgsRecVars
----                             let unused = (defd S.\\ sgsRecVars initialState) S.\\ used
----                             unless (null unused) $ throwError (SDEUnused unused)
----                             updatedState <- get
----                             put initialState
----                             return ((n, s), updatedState)
----              ) ss
----  -- We must remove "locally defined" used variables before merging sets since
----  -- a used variable in one branch doesn't mean a different variable with the
----  -- same name is used in another branch.
----  mapM_ (\(SessGramState{sgsDualVars=upDvs, sgsSessVars=upSvs, sgsRecVars=upRvs}) ->
----          modify (\s@(SessGramState{sgsDualVars=ds, sgsSessVars=vs}) ->
----                   s{ sgsDualVars=ds `S.union` upDvs
----                    -- add used session variables that were not bound by a recursion def'n
----                    , sgsSessVars=vs `S.union` (upSvs S.\\ (upRvs S.\\ sgsRecVars initialState))
----                    }
----                 )
----        ) (map snd res)
----  return $ map fst res
--
----gToSession' :: GSession -> SessGramM Session
----gToSession' (GSessEnd) = return SEnd
----
----gToSession' (GSessVar     (UIdent v)) = addSessVar (pack v) >> return (SVar     $ pack v)
----gToSession' (GSessVarDual (UIdent v)) = addDualVar (pack v) >> return (SDualVar $ pack v)
----
----gToSession' (GSessRec (UIdent x) (GSessVar     (UIdent y))) = throwError $ SDETrivialRec (pack x) (pack y)
----gToSession' (GSessRec (UIdent x) (GSessVarDual (UIdent y))) = throwError $ SDETrivialRec (pack x) (pack y)
----gToSession' (GSessRec (UIdent v) c) = addRecVar (pack v) >> liftM (SCoInd (pack v)) (gToSession' c)
----
----gToSession' (GSessGet (GSessTy   t) c) =
----  let s1t = gToType t
----   in case s1TypeToS2Type s1t of
----           Just t' -> liftM (SGetTy t') (gToSession' c)
----           Nothing -> throwError (SDEIncompleteType s1t)
----
----gToSession' (GSessGet (GSessSess s) c) = either throwError (\s' -> liftM (SGetSess s') (gToSession' c)) (gToSession s)
----
----gToSession' (GSessPut (GSessTy   t) c) =
----  let s1t = gToType t
----   in case s1TypeToS2Type s1t of
----           Just t' -> liftM (SPutTy t') (gToSession' c)
----           Nothing -> throwError (SDEIncompleteType s1t)
----
----gToSession' (GSessPut (GSessSess s) c) = either throwError (\s' -> liftM (SPutSess s') (gToSession' c)) (gToSession s)
----
----gToSession' (GSessSel ss) = do
----  let labeledGSess = map (\(GSessChoice (LIdent n) g) -> (pack n, g)) ss
----  liftM SSelect $ convertLabeledSessions labeledGSess
----
----gToSession' (GSessCho ss) = do
----  let labeledGSess = map (\(GSessChoice (LIdent n) g) -> (pack n, g)) ss
----  liftM SChoice $ convertLabeledSessions labeledGSess
--
--replaceVarInSession  :: (T.Text, Session) -> Session -> Session
--replaceVarInSession (var, replacement) = F.cata alg
--  where alg s@(SVarF n) = if var == n
--                             then replacement
--                             else F.embed s
--        alg s = F.embed s
--
--isDual :: Session -> Session -> Bool
--isDual s1 s2 = s1 `coIndEq` dual s2
--
---- Checks whether two sessions are equal under induction variable renaming
--coIndEq :: Session -> Session -> Bool
--coIndEq (SCoInd n1 s1) (SCoInd n2 s2) =
--  replaceVarInSession (n1, SEnd) s1 `coIndEq`
--  replaceVarInSession (n2, SEnd) s2
--coIndEq (SGetTy t1 s1) (SGetTy t2 s2) = t1 == t2 && s1 `coIndEq` s2
--coIndEq (SPutTy t1 s1) (SPutTy t2 s2) = t1 == t2 && s1 `coIndEq` s2
--coIndEq (SGetSess g1 s1) (SGetSess g2 s2) = g1 `coIndEq` g2 && s1 `coIndEq` s2
--coIndEq (SPutSess p1 s1) (SPutSess p2 s2) = p1 `coIndEq` p2 && s1 `coIndEq` s2
--coIndEq (SChoice xs) (SChoice ys) = combineLabelList coIndEq xs ys
--coIndEq (SSelect xs) (SSelect ys) = combineLabelList coIndEq xs ys
--coIndEq (SEnd) (SEnd) = True
--coIndEq _ _ = False
--
--unfoldSession :: Session -> Session
--unfoldSession inSess@(SCoInd var inCont) =
--  replaceVarInSession (var, inSess) inCont
--unfoldSession s = s
--
--isInductiveSession :: Session -> Bool
--isInductiveSession = F.cata alg
--  where alg (SCoIndF _ _) = True
--        alg (SGetTyF _ y) = y
--        alg (SPutTyF _ y) = y
--        alg (SGetSessF _ y) = y
--        alg (SPutSessF _ y) = y
--        alg (SChoiceF xs) = any snd xs
--        alg (SSelectF xs) = any snd xs
--        alg _ = False
--
--freeIndVars :: Session -> S.Set T.Text
--freeIndVars = F.cata alg
--  where alg (SCoIndF   n vs) = S.delete n vs
--        alg (SGetTyF   _ vs) = vs
--        alg (SPutTyF   _ vs) = vs
--        alg (SGetSessF _ vs) = vs
--        alg (SPutSessF _ vs) = vs
--        alg (SChoiceF    xs) = S.unions (map snd xs)
--        alg (SSelectF    xs) = S.unions (map snd xs)
--        alg (SVarF     v)    = S.singleton v
--        alg (SDualVarF v)    = S.singleton v
--        alg (SEndF)          = S.empty
--
--
