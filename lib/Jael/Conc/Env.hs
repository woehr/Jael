{-# Language NoImplicitPrelude #-}

module Jael.Conc.Env where

import ClassyPrelude hiding (Chan)
import qualified Data.Map as M
import qualified Data.Set as S
import Jael.Conc.Proc
import Jael.Conc.Session
import Jael.Seq.Types
import Jael.Util.UPair

data EnvValue = Linear Session | Base Ty
  deriving (Show)

data ConcTyEnv = ConcTyEnv
  { cteLin   :: M.Map Chan Session
  , cteFresh :: S.Set Chan
  , cteRec   :: M.Map (Chan, Text) Session
  , ctePar   :: S.Set (UPair Chan)
  , cteBase  :: M.Map Text (Bool, Ty)
  , cteSeq   :: TyEnv
  , cteAlias :: M.Map Text Session
  , cteProcs :: M.Map Text [(Text, TyOrSess)]
  } deriving (Show)

emptyEnv :: ConcTyEnv
emptyEnv = ConcTyEnv
  { cteLin   = M.empty
  , cteFresh = S.empty
  , cteRec   = M.empty
  , ctePar   = S.empty
  , cteBase  = M.empty
  , cteSeq   = TyEnv M.empty
  , cteAlias = M.empty
  , cteProcs = M.empty
  }

