{-# Language NoImplicitPrelude #-}

module Jael.Conc.Env where

import ClassyPrelude hiding (Chan)
import qualified Data.Map as M
import qualified Data.Set as S
import Jael.Conc.Proc
import Jael.Conc.Session
import Jael.Seq.Types

data EnvValue = Linear Session | Base Ty
  deriving (Show)

data ConcTyEnv = ConcTyEnv
  { cteLin   :: M.Map Chan Session
  , cteRec   :: M.Map (Chan, Text) Session
  , ctePar   :: M.Map Chan (S.Set Chan)
  , cteFresh :: S.Set Chan
  , cteBase  :: M.Map Text (Bool, Ty)
  , cteSeq   :: TyEnv
  , cteAlias :: M.Map Text Session
  , cteProcs :: M.Map Text [(Text, TyOrSess)]
  } deriving (Show)

emptyEnv :: ConcTyEnv
emptyEnv = ConcTyEnv
  { cteLin   = M.empty
  , cteRec   = M.empty
  , ctePar   = M.empty
  , cteFresh = S.empty
  , cteBase  = M.empty
  , cteSeq   = TyEnv M.empty
  , cteAlias = M.empty
  , cteProcs = M.empty
  }

-- Do two channels have to be used in parallel?
useInPar :: Chan -> Chan -> ConcTyEnv -> Bool
useInPar c1 c2 (ConcTyEnv{ctePar=parEnv}) =
  let res1 = case M.lookup c1 parEnv of
                  Just cs -> c2 `S.member` cs
                  Nothing -> False
      res2 = case M.lookup c2 parEnv of
                  Just cs -> c1 `S.member` cs
                  Nothing -> False
   in if res1 /= res2
         then error "Failed sanity check. There is a logic error somewhere if\
                   \ both channels are not required to be used in parallel with\
                   \ the other."
         else res1

addParChans :: Chan -> Chan -> ConcTyEnv -> ConcTyEnv
addParChans c1 c2 env@(ConcTyEnv{ctePar=parEnv}) =
  let parEnv' = case M.lookup c1 parEnv of
                     Just s -> M.insert c1 (S.insert c2 s) parEnv
                     Nothing -> M.insert c1 (S.singleton c2) parEnv
   in env{ctePar=case M.lookup c2 parEnv' of
                      Just s -> M.insert c2 (S.insert c1 s) parEnv'
                      Nothing -> M.insert c2 (S.singleton c1) parEnv'
   }

