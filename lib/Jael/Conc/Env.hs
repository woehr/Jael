{-# Language NoImplicitPrelude #-}

module Jael.Conc.Env where

import ClassyPrelude hiding (Chan)
import qualified Data.Map as M
import qualified Data.Set as S
import Jael.Conc.Proc
import Jael.Conc.Session
import Jael.Seq.Types

data EnvValue = NewLinear Chan Chan Session
              | RxdLinear Chan Session
              | Base Text Ty
  deriving (Show)

-- A type tracking whether a channel is currently used in a concurrent context
type ConcCtx = Bool
-- A map tracking alias and recursion variables for a channel's session
type AliasMap = M.Map Text Session
-- A set of channels with which the channel can not be used sequentially
type InterferenceSet = S.Set Chan

data LinEnv = LinEnv
  { leSess    :: Session
  , leDual    :: Maybe Chan
  , leConcCtx :: ConcCtx
  , leAliases :: AliasMap
  , leIntSet  :: InterferenceSet
  } deriving (Show)

newLinEnv :: Session -> Maybe Chan -> ConcCtx -> LinEnv
newLinEnv s dualChan cc = LinEnv{ leSess    = s
                                , leDual    = dualChan
                                , leConcCtx = cc
                                , leAliases = M.empty
                                , leIntSet  = S.empty
                                }

data ConcTyEnv = ConcTyEnv
  {
  -- The linear environment tracking channel types
  -- A mapping of recursion variables for channels
  -- Track non-interference of channels and enforce parallel channel use
    cteLin   :: M.Map Chan LinEnv
  -- Whether a channel has been used yet
  , cteFresh :: S.Set Chan
  -- Map of environments for recursion contexts
  , cteRec   :: M.Map Text [(Text, TyOrSess)]
  -- Types (and usage) of sequential variables
  , cteBase  :: M.Map Text (Bool, Ty)
  -- Type environment with builtins and user defined types
  , cteSeq   :: TyEnv
  -- Top level names that have been given to sessions
  , cteAlias :: M.Map Text Session
  -- Names and argument types of top level processes
  -- I assume that all arguments to a process are independent, that is, two
  -- channels that are in each others interference sets can not be passed
  -- together. This also implies that passing two channels together causes
  -- them to be added to each others interference set.
  , cteProcs :: M.Map Text [(Text, TyOrSess)]
  } deriving (Show)

emptyEnv :: ConcTyEnv
emptyEnv = ConcTyEnv
  { cteLin   = M.empty
  , cteFresh = S.empty
  , cteRec   = M.empty
  , cteBase  = M.empty
  , cteSeq   = TyEnv M.empty
  , cteAlias = M.empty
  , cteProcs = M.empty
  }

-- Assume that c1 and c2 both exist in cteLin and add each to the others
-- interference set
addInterferenceUnsafe :: Chan -> Chan -> ConcTyEnv -> ConcTyEnv
addInterferenceUnsafe c1 c2 env@(ConcTyEnv{cteLin=linEnv}) =
  let c1env@(LinEnv{leIntSet=intSet1}) =
        M.findWithDefault (error $ "Expected " ++ unpack c1 ++ " to be in the\
                                  \ linear environment but it was not.")
                          c1 linEnv
      c2env@(LinEnv{leIntSet=intSet2}) =
        M.findWithDefault (error $ "Expected " ++ unpack c2 ++ " to be in the\
                                  \ linear environment but it was not.")
                          c2 linEnv
   in env{cteLin=M.insert c1 c1env{leIntSet=S.insert c2 intSet1}
               $ M.insert c2 c2env{leIntSet=S.insert c1 intSet2} linEnv}

