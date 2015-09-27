{-# Language NoImplicitPrelude, TypeFamilies #-}

module Jael.Conc.Session where

import ClassyPrelude hiding (Chan, Foldable)
import Data.Functor.Foldable
import qualified Data.Map as M
import qualified Data.Set as S
import Jael.Grammar
import Jael.Util
import Jael.Seq.Types

data Session = SGetTy Ty Session
             | SPutTy Ty Session
             | SGetSess Session Session
             | SPutSess Session Session
             | SChoice (M.Map Text Session) Session
             | SSelect (M.Map Text Session) Session
             | SRepl Session
             | SCoInd Text Session
             | SIndVar Text
             | SEnd
             deriving (Show)

data SessionF a = SGetTyF Ty a
                | SPutTyF Ty a
                | SGetSessF a a
                | SPutSessF a a
                | SChoiceF (M.Map Text a) a
                | SSelectF (M.Map Text a) a
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

gLabelListToMap :: [GSessLab] -> M.Map Text GSession
gLabelListToMap xs =
  case insertCollectDups
          M.empty
          (map (\(GSessLab (LIdent x) s)-> (pack x, s)) xs) of
          Left dups -> error "Duplicate labels in session type."
       Right m   -> m

gToSess :: GSession -> Session
gToSess = ana coalg
  where coalg :: GSession -> Base Session GSession
        coalg (GSessEnd) = SEndF
        coalg (GSessVar (UIdent var)) = SIndVarF (pack var)
        coalg (GSessRec (UIdent var) cont) = SCoIndF (pack var) cont
        coalg (GSessGet (GSessTy t) cont) = SGetTyF (gToType t) cont
        coalg (GSessGet (GSessSess s) cont) = SGetSessF s cont
        coalg (GSessPut (GSessTy t) cont) = SPutTyF (gToType t) cont
        coalg (GSessPut (GSessSess s) cont) = SPutSessF s cont
        coalg (GSessSel ss cont) = SSelectF (gLabelListToMap ss) cont
        coalg (GSessCho ss cont) = SChoiceF (gLabelListToMap ss) cont

varMerge :: Ord a
         => (S.Set a, S.Set a)
         -> (S.Set a, S.Set a)
         -> (S.Set a, S.Set a)
varMerge (a1, a2) (b1, b2) = (a1 `S.union` b1, a2 `S.union` b2)

defAndUsedIndVars :: Session -> (S.Set Text, S.Set Text)
defAndUsedIndVars = cata alg
  where alg :: Base Session (S.Set Text, S.Set Text) -> (S.Set Text, S.Set Text)
        alg (SEndF) = (S.empty, S.empty)
        alg (SIndVarF var) = (S.empty, S.singleton var)
        alg (SCoIndF var cont) = (S.singleton var, S.empty) `varMerge` cont
        alg (SGetTyF _ y) = y
        alg (SPutTyF _ y) = y
        alg (SGetSessF x y) = x `varMerge` y
        alg (SPutSessF x y) = x `varMerge` y
        alg (SChoiceF xs y) = foldr varMerge (S.empty, S.empty) (y:(M.elems xs))
        alg (SSelectF xs y) = foldr varMerge (S.empty, S.empty) (y:(M.elems xs))
        alg (SReplF x) = x

freeIndVars :: Session -> S.Set Text
freeIndVars s = uncurry (flip (S.\\)) $ defAndUsedIndVars s

unusedIndVars :: Session -> S.Set Text
unusedIndVars s = uncurry (S.\\) $ defAndUsedIndVars s

