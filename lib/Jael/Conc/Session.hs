module Jael.Conc.Session where

import Data.Functor.Foldable as F
import qualified Data.Map as M
import qualified Data.Set as S
import Jael.Grammar
import Jael.Util
import Jael.Seq.Types

data SessDefErr = SessDefErr
  { sessErrDupInd      :: S.Set Text
  , sessErrDupLab      :: S.Set Text
  , sessErrUnused      :: S.Set Text
  , sessErrDualRec     :: S.Set Text
  , sessErrNoBehaviour :: S.Set Text
  , sessErrIncompleteType :: Bool
  } deriving (Eq, Show)

data S1Session = S1SGetTy S1Ty S1Session
               | S1SPutTy S1Ty S1Session
               | S1SGetSess S1Session S1Session
               | S1SPutSess S1Session S1Session
               | S1SChoice [(Text, S1Session)]
               | S1SSelect [(Text, S1Session)]
               | S1SCoInd Text S1Session
               | S1SVar Text
               | S1SDualVar Text
               | S1SEnd
               deriving (Eq, Show)

data S1SessionF a = S1SGetTyF S1Ty a
                  | S1SPutTyF S1Ty a
                  | S1SGetSessF S1Session a
                  | S1SPutSessF S1Session a
                  | S1SChoiceF [(Text, a)]
                  | S1SSelectF [(Text, a)]
                  | S1SCoIndF Text a
                  | S1SVarF Text
                  | S1SDualVarF Text
                  | S1SEndF
                  deriving (Functor, Show)

type instance Base S1Session = S1SessionF

instance F.Foldable S1Session where
  project (S1SGetTy x y)   = S1SGetTyF x y
  project (S1SPutTy x y)   = S1SPutTyF x y
  project (S1SGetSess x y) = S1SGetSessF x y
  project (S1SPutSess x y) = S1SPutSessF x y
  project (S1SChoice x)    = S1SChoiceF x
  project (S1SSelect x)    = S1SSelectF x
  project (S1SCoInd x y)   = S1SCoIndF x y
  project (S1SVar x)       = S1SVarF x
  project (S1SDualVar x)   = S1SDualVarF x
  project (S1SEnd)         = S1SEndF

instance F.Unfoldable S1Session where
  embed (S1SGetTyF x y)   = S1SGetTy x y
  embed (S1SPutTyF x y)   = S1SPutTy x y
  embed (S1SGetSessF x y) = S1SGetSess x y
  embed (S1SPutSessF x y) = S1SPutSess x y
  embed (S1SChoiceF x)    = S1SChoice x
  embed (S1SSelectF x)    = S1SSelect x
  embed (S1SCoIndF x y)   = S1SCoInd x y
  embed (S1SVarF x)       = S1SVar x
  embed (S1SDualVarF x)   = S1SDualVar x
  embed (S1SEndF)         = S1SEnd

data S2Session = SGetTy S2Ty S2Session
               | SPutTy S2Ty S2Session
               | SGetSess S2Session S2Session
               | SPutSess S2Session S2Session
               | SChoice [(Text, S2Session)]
               | SSelect [(Text, S2Session)]
               | SCoInd Text S2Session
               | SVar Text
               | SDualVar Text
               | SEnd
               deriving (Eq, Show)

-- Some functions in this module recurse into the session in SGetSess and
-- SPutSess while others do not. This is why that session is a Session instead
-- of an "a"
data S2SessionF a = SGetTyF S2Ty a
                  | SPutTyF S2Ty a
                  | SGetSessF S2Session a
                  | SPutSessF S2Session a
                  | SChoiceF [(Text, a)]
                  | SSelectF [(Text, a)]
                  | SCoIndF Text a
                  | SVarF Text
                  | SDualVarF Text
                  | SEndF
                 deriving (Functor, Show)

type instance Base S2Session = S2SessionF

instance F.Foldable S2Session where
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

instance F.Unfoldable S2Session where
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
dual :: S2Session -> S2Session
dual s = let
    vs = S.fromList . (\(x, _, _) -> x) . varUsage2 $ s
    alg (SGetTyF   x y) = SPutTy   x y
    alg (SPutTyF   x y) = SGetTy   x y
    alg (SGetSessF x y) = SPutSess x y
    alg (SPutSessF x y) = SGetSess x y
    alg (SChoiceF  x)   = SSelect x
    alg (SSelectF  x)   = SChoice x
    alg (SVarF x)       = if x `S.member` vs
                             then SVar x
                             else SDualVar x
    -- A valid session should not have a "dualed" recursion variable
    alg (SDualVarF x)   = if x `S.member` vs
                             then error "Compiler error. This should not happen\
                                        \ if the session was validated."
                             else SVar x
    alg x = embed x
  in cata alg s

convLabelList :: [GSessChoice] -> [(Text, S1Session)]
convLabelList = map (\(GSessChoice (GChoiceLabel (LIdent x)) s)->
                        (pack x, gToSession s))

gToSession :: GSession -> S1Session
gToSession (GSessEnd) = S1SEnd
gToSession (GSessVar (UIdent var)) = S1SVar (pack var)
gToSession (GSessVarDual (UIdent var)) = S1SDualVar (pack var)
gToSession (GSessRec (UIdent var) c) = S1SCoInd (pack var) (gToSession c)
gToSession (GSessGet (GSessTy t) c) = S1SGetTy (gToType t) (gToSession c)
gToSession (GSessGet (GSessSess s) c) = S1SGetSess (gToSession s) (gToSession c)
gToSession (GSessPut (GSessTy t) c) = S1SPutTy (gToType t) (gToSession c)
gToSession (GSessPut (GSessSess s) c) = S1SPutSess (gToSession s) (gToSession c)
gToSession (GSessSel ss) = S1SSelect (convLabelList ss)
gToSession (GSessCho ss) = S1SChoice (convLabelList ss)

tupListMerge :: ([a], [b], [c]) -> ([a], [b], [c]) -> ([a], [b], [c])
tupListMerge (a, b, c) (a', b', c') = (a ++ a', b ++ b', c ++ c')

-- Returns a list of co-inductive names, a list of session variables, and a list
-- of dualed session variables
varUsage :: S1Session -> ([Text], [Text], [Text])
varUsage = cata alg
  where alg :: Base S1Session ([Text], [Text], [Text]) -> ([Text], [Text], [Text])
        alg (S1SEndF) = ([], [], [])
        alg (S1SVarF var) = ([], [var], [])
        alg (S1SDualVarF var) = ([], [], [var])
        alg (S1SCoIndF var (us, vs, ws)) = (var:us, vs, ws)
        alg (S1SGetTyF _ y) = y
        alg (S1SPutTyF _ y) = y
        alg (S1SGetSessF x y) = varUsage x `tupListMerge` y
        alg (S1SPutSessF x y) = varUsage x `tupListMerge` y
        alg (S1SChoiceF xs) = foldr (tupListMerge . snd) ([], [], []) xs
        alg (S1SSelectF xs) = foldr (tupListMerge . snd) ([], [], []) xs

varUsage2 :: S2Session -> ([Text], [Text], [Text])
varUsage2 = cata alg
  where alg :: Base S2Session ([Text], [Text], [Text]) -> ([Text], [Text], [Text])
        alg (SEndF) = ([], [], [])
        alg (SVarF var) = ([], [var], [])
        alg (SDualVarF var) = ([], [], [var])
        alg (SCoIndF var (us, vs, ws)) = (var:us, vs, ws)
        alg (SGetTyF _ y) = y
        alg (SPutTyF _ y) = y
        alg (SGetSessF x y) = varUsage2 x `tupListMerge` y
        alg (SPutSessF x y) = varUsage2 x `tupListMerge` y
        alg (SChoiceF xs) = foldr (tupListMerge . snd) ([], [], []) xs
        alg (SSelectF xs) = foldr (tupListMerge . snd) ([], [], []) xs

-- Finds any duplicate labels in a single select or choice. Across several
-- select or choice, labels can be reused.
dupLabels :: S1Session -> S.Set Text
dupLabels = cata alg
  where alg :: Base S1Session (S.Set Text) -> S.Set Text
        alg (S1SCoIndF _ c) = c
        alg (S1SGetTyF _ y) = y
        alg (S1SPutTyF _ y) = y
        alg (S1SGetSessF x y) = dupLabels x `S.union` y
        alg (S1SPutSessF x y) = dupLabels x `S.union` y
        alg (S1SChoiceF xs) = S.unions $ S.fromList (repeated $ map fst xs):map snd xs
        alg (S1SSelectF xs) = S.unions $ S.fromList (repeated $ map fst xs):map snd xs
        alg _ = S.empty

-- TODO: Is `rec X. <X>` the only case?
trivialRecursionVars :: S1Session -> S.Set Text
trivialRecursionVars (S1SCoInd v (S1SVar v')) = if v == v'
                                               then S.singleton v
                                               else S.empty
trivialRecursionVars (S1SCoInd _ x) = trivialRecursionVars x
trivialRecursionVars (S1SGetTy _ x) = trivialRecursionVars x
trivialRecursionVars (S1SPutTy _ x) = trivialRecursionVars x
trivialRecursionVars (S1SGetSess x y) = trivialRecursionVars x `S.union` trivialRecursionVars y
trivialRecursionVars (S1SPutSess x y) = trivialRecursionVars x `S.union` trivialRecursionVars y
trivialRecursionVars (S1SChoice xs) = S.unions $ map (trivialRecursionVars . snd) xs
trivialRecursionVars (S1SSelect xs) = S.unions $ map (trivialRecursionVars . snd) xs
trivialRecursionVars _ = S.empty

replaceVarInSession  :: (Text, S2Session) -> S2Session -> S2Session
replaceVarInSession (var, replacement) = cata alg
  where alg s@(SVarF n) = if var == n
                             then replacement
                             else embed s
        alg s = embed s

isDual :: S2Session -> S2Session -> Bool
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
coIndEq :: S2Session -> S2Session -> Bool
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

unfoldSession :: S2Session -> S2Session
unfoldSession inSess@(SCoInd var inCont) =
  replaceVarInSession (var, inSess) inCont
unfoldSession s = s

freeIndVars :: S1Session -> S.Set Text
freeIndVars = freeIndVars' . varUsage

freeIndVars' :: ([Text], [Text], [Text]) -> S.Set Text
freeIndVars' (defd, vars, dualedVars) =
  S.fromList (vars ++ dualedVars) S.\\ S.fromList defd

--unusedIndVars :: Session -> S.Set Text
--unusedIndVars = unusedIndVars' . varUsage

unusedIndVars' :: ([Text], [Text], [Text]) -> S.Set Text
unusedIndVars' (defd, vars, dualedVars) =
  S.fromList defd S.\\ S.fromList (vars ++ dualedVars)

isInductiveSession :: S2Session -> Bool
isInductiveSession = cata alg
  where alg (SCoIndF _ _) = True
        alg (SGetTyF _ y) = y
        alg (SPutTyF _ y) = y
        alg (SGetSessF _ y) = y
        alg (SPutSessF _ y) = y
        alg (SChoiceF xs) = any snd xs
        alg (SSelectF xs) = any snd xs
        alg _ = False

s1SessionToS2Session :: S1Session -> Maybe S2Session
s1SessionToS2Session s1 = undefined

validateS1Session :: S1Session -> Either SessDefErr S2Session
validateS1Session s =
  let usages@(varsDefd, _, dualedVars) = varUsage s
      dupInd = repeated varsDefd
      dupLabs = dupLabels s
      unused = unusedIndVars' usages
      dualedRecs = S.fromList dualedVars `S.intersection` S.fromList varsDefd
      trivialRec = trivialRecursionVars s
      s2Sess = s1SessionToS2Session s
   in if (not . null) dupInd     ||
         (not . null) dupLabs    ||
         (not . null) unused     ||
         (not . null) dualedRecs ||
         (not . null) trivialRec ||
         isNothing s2Sess
        then Left SessDefErr
              { sessErrDupInd = S.fromList dupInd
              , sessErrDupLab = dupLabs
              , sessErrUnused = unused
              , sessErrDualRec = dualedRecs
              , sessErrNoBehaviour = trivialRec
              , sessErrIncompleteType = isNothing s2Sess
              }
        else Right (fromMaybe (error "Can not happen. s2Sess must be a Just")
                              s2Sess)

