{-# Language NoImplicitPrelude #-}

module Jael.Conc.Env where

import ClassyPrelude hiding (Chan)
import qualified Data.Map as M
import Jael.Conc.Proc
import Jael.Conc.Session
import Jael.Seq.Types

data ConcTyEnv = ConcTyEnv
  { lin   :: M.Map Chan Session
  --, unres :: M.Map Text Session
  , base  :: M.Map Text Ty
  } deriving (Show)

