{-# Language NoImplicitPrelude #-}

module Jael.Conc.Env where

import ClassyPrelude hiding (Chan)
import qualified Data.Bimap as B
import qualified Data.Map as M
import Jael.Conc.Proc
import Jael.Conc.Session
import Jael.Seq.Types

data EnvValue = Linear Session | Base Ty
  deriving (Show)

data ConcTyEnv = ConcTyEnv
  { cteLin   :: M.Map Chan Session
  , cteRec   :: M.Map (Chan, Text) Session
  , cteDual  :: B.Bimap Chan Chan
  , cteBase  :: M.Map Text (Bool, Ty)
  , cteSeq   :: TyEnv
  , cteAlias :: M.Map Text Session
  , cteProcs :: M.Map Text [(Text, TyOrSess)]
  } deriving (Show)

emptyEnv :: ConcTyEnv
emptyEnv = ConcTyEnv
  { cteLin   = M.empty
  , cteRec   = M.empty
  , cteDual  = B.empty
  , cteBase  = M.empty
  , cteSeq   = TyEnv M.empty
  , cteAlias = M.empty
  , cteProcs = M.empty
  }

chanHasDual :: Chan -> ConcTyEnv -> Bool
chanHasDual c (ConcTyEnv{cteDual=dualEnv}) =
  c `B.member` dualEnv || c `B.memberR` dualEnv

