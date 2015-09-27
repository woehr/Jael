{-# Language NoImplicitPrelude, TypeFamilies #-}

module Jael.Conc.Session where

import ClassyPrelude hiding (Foldable)
import Data.Functor.Foldable
import qualified Data.Map as M
import qualified Data.Set as S
import Jael.Grammar
import Jael.Util
import Jael.Seq.Types

data SessDefErr = SessDefErr
  { sessErrDupInd :: (S.Set Text)
  , sessErrDupLab :: (S.Set Text)
  , sessErrFree   :: (S.Set Text)
  , sessErrUnused :: (S.Set Text)
  } deriving (Eq, Show)

data Session = SGetTy Ty Session
             | SPutTy Ty Session
             | SGetSess Session Session
             | SPutSess Session Session
             | SChoice [(Text, Session)] Session
             | SSelect [(Text, Session)] Session
             | SRepl Session
             | SCoInd Text Session
             | SIndVar Text
             | SEnd
             deriving (Show)

data SessionF a = SGetTyF Ty a
                | SPutTyF Ty a
                | SGetSessF a a
                | SPutSessF a a
                | SChoiceF [(Text, a)] a
                | SSelectF [(Text, a)] a
                | SReplF a
                | SCoIndF Text a
                | SIndVarF Text
                | SEndF
               deriving (Functor, Show)

type instance Base Session = SessionF

instance Foldable Session where
  project (SGetTy x y) = SGetTyF x y
  project (SPutTy x y) = SPutTyF x y
  project (SGetSess x y) = SGetSessF x y
  project (SPutSess x y) = SPutSessF x y
  project (SChoice x y) = SChoiceF x y
  project (SSelect x y) = SSelectF x y
  project (SRepl x) = SReplF x
  project (SCoInd x y) = SCoIndF x y
  project (SIndVar x) = SIndVarF x
  project (SEnd) = SEndF

instance Unfoldable Session where
  embed (SGetTyF x y) = SGetTy x y
  embed (SPutTyF x y) = SPutTy x y
  embed (SGetSessF x y) = SGetSess x y
  embed (SPutSessF x y) = SPutSess x y
  embed (SChoiceF x y) = SChoice x y
  embed (SSelectF x y) = SSelect x y
  embed (SReplF x) = SRepl x
  embed (SCoIndF x y) = SCoInd x y
  embed (SIndVarF x) = SIndVar x
  embed (SEndF) = SEnd

convLabelList :: [GSessLab] -> [(Text, GSession)]
convLabelList = map (\(GSessLab (LIdent x) s)-> (pack x, s))

gToSession :: GSession -> Session
gToSession = ana coalg
  where coalg :: GSession -> Base Session GSession
        coalg (GSessEnd) = SEndF
        coalg (GSessVar (UIdent var)) = SIndVarF (pack var)
        coalg (GSessRec (UIdent var) cont) = SCoIndF (pack var) cont
        coalg (GSessGet (GSessTy t) cont) = SGetTyF (gToType t) cont
        coalg (GSessGet (GSessSess s) cont) = SGetSessF s cont
        coalg (GSessPut (GSessTy t) cont) = SPutTyF (gToType t) cont
        coalg (GSessPut (GSessSess s) cont) = SPutSessF s cont
        coalg (GSessSel ss cont) = SSelectF (convLabelList ss) cont
        coalg (GSessCho ss cont) = SChoiceF (convLabelList ss) cont

tupListMerge :: ([a], [b]) -> ([a], [b]) -> ([a], [b])
tupListMerge (as, bs) (a's, b's) = (as ++ a's, bs ++ b's)

varUsage :: Session -> ([Text], [Text])
varUsage = cata alg
  where alg :: Base Session ([Text], [Text]) -> ([Text], [Text])
        alg (SEndF) = ([], [])
        alg (SIndVarF var) = ([], [var])
        alg (SCoIndF var (vs, us)) = (var:vs, us)
        alg (SGetTyF _ y) = y
        alg (SPutTyF _ y) = y
        alg (SGetSessF xs ys) = xs `tupListMerge` ys
        alg (SPutSessF xs ys) = xs `tupListMerge` ys
        alg (SChoiceF xs y) = foldr tupListMerge ([], []) (y:(map snd xs))
        alg (SSelectF xs y) = foldr tupListMerge ([], []) (y:(map snd xs))
        alg (SReplF x) = x

-- Finds any duplicate labels in a single select or choice. Accross several
-- select or choice labels can be reused.
dupLabels :: Session -> S.Set Text
dupLabels = cata alg
  where alg :: Base Session (S.Set Text) -> S.Set Text
        alg (SCoIndF _ cont) = cont
        alg (SGetTyF _ y) = y
        alg (SPutTyF _ y) = y
        alg (SGetSessF xs ys) = xs `S.union` ys
        alg (SPutSessF xs ys) = xs `S.union` ys
        alg (SChoiceF xs y) = S.unions $ (S.fromList $ repeated $ map fst xs):y:(map snd xs)
        alg (SSelectF xs y) = S.unions $ (S.fromList $ repeated $ map fst xs):y:(map snd xs)
        alg _ = S.empty

--freeIndVars :: Session -> S.Set Text
--freeIndVars = freeIndVars' . varUsage

freeIndVars' :: ([Text], [Text]) -> S.Set Text
freeIndVars' (defd, used) = S.fromList used S.\\ S.fromList defd

--unusedIndVars :: Session -> S.Set Text
--unusedIndVars = unusedIndVars' . varUsage

unusedIndVars' :: ([Text], [Text]) -> S.Set Text
unusedIndVars' (defd, used) = S.fromList defd S.\\ S.fromList used

validateSession :: Session -> Maybe SessDefErr
validateSession s =
  let usages@(varsDefd, _) = varUsage s
      dupInd = repeated varsDefd
      dupLabs = dupLabels s
      free = freeIndVars' usages
      unused = unusedIndVars' usages
   in if (length dupInd /= 0) ||
         (S.size dupLabs /= 0) ||
         (S.size free /= 0) ||
         (S.size unused /= 0)
        then Just SessDefErr
              { sessErrDupInd = S.fromList dupInd
              , sessErrDupLab = dupLabs
              , sessErrFree   = free
              , sessErrUnused = unused
              }
        else Nothing

validateSessions :: M.Map Text Session -> Maybe (M.Map Text SessDefErr)
validateSessions ss =
  let errMap = M.mapMaybe validateSession ss
   in if M.size errMap == 0 then Nothing else Just errMap

