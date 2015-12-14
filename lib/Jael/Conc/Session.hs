{-# Language RecordWildCards #-}

module Jael.Conc.Session where

import qualified Data.Functor.Foldable as F
import qualified Data.Map as M
import qualified Data.Set as S
import           Jael.Grammar
import           Jael.Seq.Types
import           Jael.Util

data SessDefErr = SDEDupInd     Text
                | SDEDupLabels  (S.Set Text)
                | SDEUnused     (S.Set Text)
                | SDEDualRec    Text
                | SDETrivialRec Text Text
                | SDEIncompleteType S1Ty
                deriving (Eq, Show)

data Session = SGetTy S2Ty Session
             | SPutTy S2Ty Session
             | SGetSess Session Session
             | SPutSess Session Session
             | SChoice [(Text, Session)]
             | SSelect [(Text, Session)]
             | SCoInd Text Session
             | SVar Text
             | SDualVar Text
             | SEnd
             deriving (Eq, Show)

-- Some functions in this module recurse into the session in SGetSess and
-- SPutSess while others do not. This is why that session is a Session instead
-- of an "a"
data SessionF a = SGetTyF S2Ty a
                | SPutTyF S2Ty a
                | SGetSessF a a
                | SPutSessF a a
                | SChoiceF [(Text, a)]
                | SSelectF [(Text, a)]
                | SCoIndF Text a
                | SVarF Text
                | SDualVarF Text
                | SEndF
               deriving (Functor, Show)

type instance F.Base Session = SessionF

instance F.Foldable Session where
  project (SGetTy x y) = SGetTyF x y
  project (SPutTy x y) = SPutTyF x y
  project (SGetSess x y) = SGetSessF x y
  project (SPutSess x y) = SPutSessF x y
  project (SChoice x) = SChoiceF x
  project (SSelect x) = SSelectF x
  project (SCoInd x y) = SCoIndF x y
  project (SVar x) = SVarF x
  project (SDualVar x) = SDualVarF x
  project (SEnd) = SEndF

instance F.Unfoldable Session where
  embed (SGetTyF x y) = SGetTy x y
  embed (SPutTyF x y) = SPutTy x y
  embed (SGetSessF x y) = SGetSess x y
  embed (SPutSessF x y) = SPutSess x y
  embed (SChoiceF x) = SChoice x
  embed (SSelectF x) = SSelect x
  embed (SCoIndF x y) = SCoInd x y
  embed (SVarF x) = SVar x
  embed (SDualVarF x) = SDualVar x
  embed (SEndF) = SEnd

-- Takes a combining function and applies it to corresponding labels in the
-- label lists. Returns true if the combining function is true for all
-- corresponding labels and both label lists have the same set of labels.
combineLabelList :: (a -> a -> Bool) -> [(Text, a)] -> [(Text, a)] -> Bool
combineLabelList f xs ys =
  let m1 = M.fromList xs
      m2 = M.fromList ys
      res = M.intersectionWith f m1 m2
   in M.size m1 == M.size res && and res

-- Partial function. Returns the dual of a session. Will error if a recursion
-- variable is dualed. This case should be caught when validating a session.
{-
dual :: Session -> Session
dual s = let
    vs = S.fromList . (\(x, _, _) -> x) . varUsage $ s
    alg (SGetTyF   x (_, y)) = SPutTy   x y
    alg (SPutTyF   x (_, y)) = SGetTy   x y
    alg (SGetSessF (x, _) (_, y)) = SPutSess x y
    alg (SPutSessF (x, _) (_, y)) = SGetSess x y
    alg (SChoiceF  x)   = SSelect (map (second snd) x)
    alg (SSelectF  x)   = SChoice (map (second snd) x)
    alg (SVarF x)       = if x `S.member` vs
                             then SVar x
                             else SDualVar x
    -- A valid session should not have a "dualed" recursion variable
    alg (SDualVarF x)   = if x `S.member` vs
                             then error "Compiler error. This should not happen\
                                        \ if the session was validated."
                             else SVar x
    alg x = F.embed x
  in F.para alg s
-}

data SessGramState = SessGramState
                       { sgsRecVars      :: S.Set Text
                       , sgsSessVars     :: S.Set Text
                       , sgsDualVars :: S.Set Text
                       } deriving (Show)

type SessGramM = ExceptT SessDefErr (State SessGramState)

gToSession :: GSession -> Either SessDefErr Session
gToSession s =
  let (res, (SessGramState{..})) = runState (runExceptT $ gToSession' s)
                                            (SessGramState S.empty S.empty S.empty)
   in case res of
           Left err -> Left err
           Right sess -> do
             let unused = sgsRecVars S.\\ sgsSessVars
             unless (null unused) $ throwError (SDEUnused unused)
             return sess

addRecVar :: Text -> SessGramM ()
addRecVar v = do
  recVars <- gets sgsRecVars
  when (v `S.member` recVars) $ throwError (SDEDupInd v)
  modify (\s@(SessGramState{..}) -> s{sgsRecVars=S.insert v sgsRecVars})

addSessVar :: Text -> SessGramM ()
addSessVar v = modify (\s@(SessGramState{..}) -> s{sgsSessVars=S.insert v sgsSessVars})

addDualVar :: Text -> SessGramM ()
addDualVar v = do
  recVars <- gets sgsRecVars
  when (v `S.member` recVars) $ throwError (SDEDualRec v)
  modify (\s@(SessGramState{..}) -> s{sgsDualVars=S.insert v sgsDualVars})

gToSession' :: GSession -> SessGramM Session
gToSession' (GSessEnd) = return SEnd

gToSession' (GSessVar     (UIdent v)) = addSessVar (pack v) >> return (SVar     $ pack v)
gToSession' (GSessVarDual (UIdent v)) = addDualVar (pack v) >> return (SDualVar $ pack v)

gToSession' (GSessRec (UIdent x) (GSessVar     (UIdent y))) = throwError $ SDETrivialRec (pack x) (pack y)
gToSession' (GSessRec (UIdent x) (GSessVarDual (UIdent y))) = throwError $ SDETrivialRec (pack x) (pack y)
gToSession' (GSessRec (UIdent v) c)    = addRecVar (pack v) >> gToSession' c

gToSession' (GSessGet (GSessTy   t) c) =
  let s1t = gToType t
      s2t = s1TypeToS2Type s1t
   in case s2t of
           Just t' -> liftM (SGetTy t') (gToSession' c)
           Nothing -> throwError (SDEIncompleteType s1t)

gToSession' (GSessGet (GSessSess s) c) = either throwError (\s' -> liftM (SGetSess s') (gToSession' c)) (gToSession s)

gToSession' (GSessPut (GSessTy   t) c) =
  let s1t = gToType t
      s2t = s1TypeToS2Type s1t
   in case s2t of
           Just t' -> liftM (SPutTy t') (gToSession' c)
           Nothing -> throwError (SDEIncompleteType s1t)

gToSession' (GSessPut (GSessSess s) c) = either throwError (\s' -> liftM (SPutSess s') (gToSession' c)) (gToSession s)

gToSession' (GSessSel ss) = do
  let rep = S.fromList . repeated $ map (\(GSessChoice (LIdent n) _) -> pack n) ss
  unless (null rep) $ throwError (SDEDupLabels rep)
  undefined

gToSession' (GSessCho ss) = do
  let rep = S.fromList . repeated $ map (\(GSessChoice (LIdent n) _) -> pack n) ss
  unless (null rep) $ throwError (SDEDupLabels rep)
  undefined

replaceVarInSession  :: (Text, Session) -> Session -> Session
replaceVarInSession (var, replacement) = F.cata alg
  where alg s@(SVarF n) = if var == n
                             then replacement
                             else F.embed s
        alg s = F.embed s

isDual :: Session -> Session -> Bool
isDual (SCoInd n1 s1) (SCoInd n2 s2) = isDual (replaceVarInSession (n1, SEnd) s1)
                                              (replaceVarInSession (n2, SEnd) s2)
isDual (SGetTy t1 s1)   (SPutTy t2 s2)   = t1 == t2 && isDual s1 s2
isDual (SPutTy t1 s1)   (SGetTy t2 s2)   = t1 == t2 && isDual s1 s2
isDual (SGetSess t1 s1) (SPutSess t2 s2) = t1 == t2 && isDual s1 s2
isDual (SPutSess t1 s1) (SGetSess t2 s2) = t1 == t2 && isDual s1 s2
isDual (SChoice xs) (SSelect ys) = combineLabelList isDual xs ys
isDual (SSelect xs) (SChoice ys) = combineLabelList isDual xs ys
isDual (SVar n1) (SDualVar n2) = n1 == n2
isDual (SDualVar n1) (SVar n2) = n1 == n2
isDual (SEnd) (SEnd) = True
isDual _ _ = False

-- Checks whether two sessions are equal under induction variable renaming
coIndEq :: Session -> Session -> Bool
coIndEq (SCoInd n1 s1) (SCoInd n2 s2) =
  replaceVarInSession (n1, SEnd) s1 `coIndEq`
  replaceVarInSession (n2, SEnd) s2
coIndEq (SGetTy t1 s1) (SGetTy t2 s2) = t1 == t2 && s1 `coIndEq` s2
coIndEq (SPutTy t1 s1) (SPutTy t2 s2) = t1 == t2 && s1 `coIndEq` s2
coIndEq (SGetSess g1 s1) (SGetSess g2 s2) = g1 `coIndEq` g2 && s1 `coIndEq` s2
coIndEq (SPutSess p1 s1) (SPutSess p2 s2) = p1 `coIndEq` p2 && s1 `coIndEq` s2
coIndEq (SChoice xs) (SChoice ys) = combineLabelList coIndEq xs ys
coIndEq (SSelect xs) (SSelect ys) = combineLabelList coIndEq xs ys
coIndEq (SEnd) (SEnd) = True
coIndEq _ _ = False

unfoldSession :: Session -> Session
unfoldSession inSess@(SCoInd var inCont) =
  replaceVarInSession (var, inSess) inCont
unfoldSession s = s

isInductiveSession :: Session -> Bool
isInductiveSession = F.cata alg
  where alg (SCoIndF _ _) = True
        alg (SGetTyF _ y) = y
        alg (SPutTyF _ y) = y
        alg (SGetSessF _ y) = y
        alg (SPutSessF _ y) = y
        alg (SChoiceF xs) = any snd xs
        alg (SSelectF xs) = any snd xs
        alg _ = False

