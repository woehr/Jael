{-# Language NoImplicitPrelude #-}

module Jael.Conc.Env where

import ClassyPrelude
import qualified Data.Map as M
import Jael.Conc.Session

data ConcTyEnv = ConcTyEnv
  { lin   :: M.Map Text Session
  , unres :: M.Map Text Session
  } deriving (Show)

