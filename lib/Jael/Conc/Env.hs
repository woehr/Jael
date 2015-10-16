{-# Language NoImplicitPrelude #-}

module Jael.Conc.Env where

import ClassyPrelude hiding (Chan)
import qualified Data.Map as M
import Jael.Conc.Proc
import Jael.Conc.Session
import Jael.Seq.Types

data EnvValue = Linear Session | Base Ty | Dual Chan
  deriving (Show)

data ConcTyEnv = ConcTyEnv
  { lin   :: M.Map Chan Session
  , duals :: M.Map Chan Chan
  , base  :: M.Map Text Ty
  } deriving (Show)

