{-# Language NoImplicitPrelude #-}

module Jael.Conc.TyCk where

import ClassyPrelude hiding (Chan, Foldable)
import Data.Functor.Foldable
import qualified Data.Map as M
import qualified Data.Set as S
import Jael.Conc.Proc
import Jael.Conc.Session
import Jael.Seq.Types

data SessEnv = SessEnv
  { -- Free vars in P of data values
    psi :: M.Map Text Ty
    -- Shared sessions
  , gamma :: M.Map Text Session
    -- Linear sessions
  , delta :: M.Map Text Session
  }
  deriving (Show)

data SessTyErr = SessTyErr
  deriving (Eq, Show)

sessTyCheck :: SessEnv -> Proc -> Maybe SessTyErr
sessTyCheck env p = undefined

