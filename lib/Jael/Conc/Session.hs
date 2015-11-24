module Jael.Conc.Session where

import Data.Functor.Foldable as F
import qualified Data.Map as M
import qualified Data.Set as S
import Jael.Grammar
import Jael.UserDefTy
import Jael.Util
import Jael.Seq.CG_Types
import Jael.Seq.HM_Types

instance UserDefTy Session where
  type TGrammar Session = GSession
  type TError   Session = SessDefErr
  type TEnvItem Session = ()

  gToUserDefTy = gToSession
  validate = validateSession
  -- The way sessions are defined in the grammar currently means they can't
  -- refer to other session types
  typeDeps _ = S.empty
  -- The act of defining a session doesn't create other items that get added
  -- to the environment (unlike structures or enums which add functions)
  envItems _ = []

data SessDefErr = SessDefErr
  { sessErrDupInd      :: S.Set Text
  , sessErrDupLab      :: S.Set Text
  , sessErrUnused      :: S.Set Text
  , sessErrDualRec     :: S.Set Text
  , sessErrNoBehaviour :: S.Set Text
  } deriving (Eq, Show)

data Session = SGetTy Ty Session
             | SPutTy Ty Session
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
data SessionF a = SGetTyF Ty a
                | SPutTyF Ty a
                | SGetSessF Session a
                | SPutSessF Session a
                | SChoiceF [(Text, a)]
                | SSelectF [(Text, a)]
                | SCoIndF Text a
                | SVarF Text
                | SDualVarF Text
                | SEndF
               deriving (Functor, Show)

type instance Base Session = SessionF

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
dual :: Session -> Session
dual s = let
    vs = S.fromList . (\(x, _, _) -> x) . varUsage $ s
    alg :: Base Session Session -> Session
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

convLabelList :: [GSessChoice] -> [(Text, GSession)]
convLabelList = map (\(GSessChoice (GChoiceLabel (LIdent x)) s)-> (pack x, s))

gToSession :: GSession -> Session
gToSession = ana coalg
  where coalg :: GSession -> Base Session GSession
        coalg (GSessEnd) = SEndF
        coalg (GSessVar (UIdent var)) = SVarF (pack var)
        coalg (GSessVarDual (UIdent var)) = SDualVarF (pack var)
        coalg (GSessRec (UIdent var) c) = SCoIndF (pack var) c
        coalg (GSessGet (GSessTy t) c) = SGetTyF ((tyOf . gToType) t) c
        coalg (GSessGet (GSessSess s) c) = SGetSessF (gToSession s) c
        coalg (GSessPut (GSessTy t) c) = SPutTyF ((tyOf . gToType) t) c
        coalg (GSessPut (GSessSess s) c) = SPutSessF (gToSession s) c
        coalg (GSessSel ss) = SSelectF (convLabelList ss)
        coalg (GSessCho ss) = SChoiceF (convLabelList ss)

tupListMerge :: ([a], [b], [c]) -> ([a], [b], [c]) -> ([a], [b], [c])
tupListMerge (a, b, c) (a', b', c') = (a ++ a', b ++ b', c ++ c')

-- Returns a list of co-inductive names, a list of session variables, and a list
-- of dualed session variables
varUsage :: Session -> ([Text], [Text], [Text])
varUsage = cata alg
  where alg :: Base Session ([Text], [Text], [Text]) -> ([Text], [Text], [Text])
        alg (SEndF) = ([], [], [])
        alg (SVarF var) = ([], [var], [])
        alg (SDualVarF var) = ([], [], [var])
        alg (SCoIndF var (us, vs, ws)) = (var:us, vs, ws)
        alg (SGetTyF _ y) = y
        alg (SPutTyF _ y) = y
        alg (SGetSessF x y) = varUsage x `tupListMerge` y
        alg (SPutSessF x y) = varUsage x `tupListMerge` y
        alg (SChoiceF xs) = foldr (tupListMerge . snd) ([], [], []) xs
        alg (SSelectF xs) = foldr (tupListMerge . snd) ([], [], []) xs

-- Finds any duplicate labels in a single select or choice. Across several
-- select or choice, labels can be reused.
dupLabels :: Session -> S.Set Text
dupLabels = cata alg
  where alg :: Base Session (S.Set Text) -> S.Set Text
        alg (SCoIndF _ c) = c
        alg (SGetTyF _ y) = y
        alg (SPutTyF _ y) = y
        alg (SGetSessF x y) = dupLabels x `S.union` y
        alg (SPutSessF x y) = dupLabels x `S.union` y
        alg (SChoiceF xs) = S.unions $ S.fromList (repeated $ map fst xs):map snd xs
        alg (SSelectF xs) = S.unions $ S.fromList (repeated $ map fst xs):map snd xs
        alg _ = S.empty

-- TODO: Is `rec X. <X>` the only case?
trivialRecursionVars :: Session -> S.Set Text
trivialRecursionVars (SCoInd v (SVar v')) = if v == v'
                                               then S.singleton v
                                               else S.empty
trivialRecursionVars (SCoInd _ x) = trivialRecursionVars x
trivialRecursionVars (SGetTy _ x) = trivialRecursionVars x
trivialRecursionVars (SPutTy _ x) = trivialRecursionVars x
trivialRecursionVars (SGetSess x y) = trivialRecursionVars x `S.union` trivialRecursionVars y
trivialRecursionVars (SPutSess x y) = trivialRecursionVars x `S.union` trivialRecursionVars y
trivialRecursionVars (SChoice xs) = S.unions $ map (trivialRecursionVars . snd) xs
trivialRecursionVars (SSelect xs) = S.unions $ map (trivialRecursionVars . snd) xs
trivialRecursionVars _ = S.empty

replaceVarInSession  :: (Text, Session) -> Session -> Session
replaceVarInSession (var, replacement) = cata alg
  where alg s@(SVarF n) = if var == n
                             then replacement
                             else embed s
        alg s = embed s

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

freeIndVars :: Session -> S.Set Text
freeIndVars = freeIndVars' . varUsage

freeIndVars' :: ([Text], [Text], [Text]) -> S.Set Text
freeIndVars' (defd, vars, dualedVars) =
  S.fromList (vars ++ dualedVars) S.\\ S.fromList defd

--unusedIndVars :: Session -> S.Set Text
--unusedIndVars = unusedIndVars' . varUsage

unusedIndVars' :: ([Text], [Text], [Text]) -> S.Set Text
unusedIndVars' (defd, vars, dualedVars) =
  S.fromList defd S.\\ S.fromList (vars ++ dualedVars)

isInductiveSession :: Session -> Bool
isInductiveSession = cata alg
  where alg :: Base Session Bool -> Bool
        alg (SCoIndF _ _) = True
        alg (SGetTyF _ y) = y
        alg (SPutTyF _ y) = y
        alg (SGetSessF _ y) = y
        alg (SPutSessF _ y) = y
        alg (SChoiceF xs) = any snd xs
        alg (SSelectF xs) = any snd xs
        alg _ = False

validateSession :: Session -> Maybe SessDefErr
validateSession s =
  let usages@(varsDefd, _, dualedVars) = varUsage s
      dupInd = repeated varsDefd
      dupLabs = dupLabels s
      unused = unusedIndVars' usages
      dualedRecs = S.fromList dualedVars `S.intersection` S.fromList varsDefd
      trivialRec = trivialRecursionVars s
   in if (not . null) dupInd     ||
         (not . null) dupLabs    ||
         (not . null) unused     ||
         (not . null) dualedRecs ||
         (not . null) trivialRec
        then Just SessDefErr
              { sessErrDupInd = S.fromList dupInd
              , sessErrDupLab = dupLabs
              , sessErrUnused = unused
              , sessErrDualRec = dualedRecs
              , sessErrNoBehaviour = trivialRec
              }
        else Nothing

