{-# Language NoImplicitPrelude #-}

module Jael.Conc.Env where

import ClassyPrelude
import qualified Data.Map as M
import Jael.Conc.Session

data ConcTyEnv = ConcTyEnv
  { linear :: M.Map Text Session
  , unlimited :: M.Map Text Session
  } deriving (Show)

