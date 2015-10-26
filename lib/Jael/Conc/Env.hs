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
  {
  -- The linear environment tracking channel types
    cteLin   :: M.Map Chan Session
  -- Whether a channel has been used yet
  , cteFresh :: S.Set Chan
  -- Mapping of recursion variables for channels
  , cteRec   :: M.Map (Chan, Text) Session
  -- Track non-interference of channels and enforce parallel channel use
  , ctePar   :: M.Map Chan (Bool, S.Set Chan)
  -- Types (and usage) of sequential variables
  , cteBase  :: M.Map Text (Bool, Ty)
  -- Type environment with builtins and user defined types
  , cteSeq   :: TyEnv
  -- Names that have been given to sessions
  , cteAlias :: M.Map Text Session
  -- Names and argument types of sessions
  , cteProcs :: M.Map Text [(Text, TyOrSess)]
  } deriving (Show)

emptyEnv :: ConcTyEnv
emptyEnv = ConcTyEnv
  { cteLin   = M.empty
  , cteFresh = S.empty
  , cteRec   = M.empty
  , ctePar   = M.empty
  , cteBase  = M.empty
  , cteSeq   = TyEnv M.empty
  , cteAlias = M.empty
  , cteProcs = M.empty
  }

